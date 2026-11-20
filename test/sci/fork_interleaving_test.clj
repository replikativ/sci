(ns sci.fork-interleaving-test
  "Regressions for interleavings of forks, retirement and publications found
   in review. Each test forces its interleaving with hooks on world internals
   and latches (bounded waits on conditions), not sleeps."
  (:require [clojure.test :refer [deftest is testing]]
            [sci.core :as sci]
            [sci.impl.world :as world])
  (:import [java.lang.ref WeakReference]))

(defn- forkable [opts]
  (sci/init (assoc opts :runtime-mode :forkable)))

(defn- await-condition
  "Wait up to `ms` for `(pred)`; whether it became true."
  [ms pred]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/onSpinWait) (recur))))))

(defn- slot-of [ctx handle]
  (world/slot-of (:sci.impl/world ctx) handle))

(deftest an-env-watch-may-wait-for-a-fork
  ;; A watch on the context's env atom is user code: it must not run inside a
  ;; publication, where a fork it waits for could never take the gate.
  (let [ctx (forkable {})
        _ (sci/eval-string* ctx "(def x 0)")
        forked (promise)]
    (add-watch (:env ctx) ::fork-from-watch
               (fn [_ _ _ _]
                 (when-not (realized? forked)
                   (deliver forked (deref (future (sci/fork ctx)) 2000 ::timeout)))))
    (try
      (sci/eval-string* ctx "(def x 1)")
      (is (map? (deref forked 5000 ::no-watch)) "the fork the watch waited for completed")
      (finally (remove-watch (:env ctx) ::fork-from-watch)))))

(deftest first-realization-never-copies-a-dependent-root-without-its-prerequisite
  ;; Two volatiles of a still-mutable primary: b is set from a. The first fork
  ;; realizes them one after the other; a write of a and then of b between
  ;; those reads must not give the child the new b with the old a.
  (let [t1-entered (promise) t1-go (promise) t2-entered (promise)
        ctx (forkable {:namespaces
                       {'host {'t1 #(do (deliver t1-entered true) (deref t1-go 5000 :timeout))
                               't2-entered #(deliver t2-entered true)
                               'await-written (fn [v] (await-condition 1000 #(= 1 @v)))}}})
        [v0 v1] (sci/eval-string* ctx "(def vs [(volatile! 0) (volatile! 0)]) vs")
        order (keys (:descriptors @(.-registry ^sci.impl.world.DenseWorld (:sci.impl/world ctx))))
        a-index (if (< (.indexOf ^java.util.List (vec order) v0)
                       (.indexOf ^java.util.List (vec order) v1)) 0 1)
        [a b] (if (zero? a-index) [v0 v1] [v1 v0])
        b-index (- 1 a-index)
        a-slot (slot-of ctx a)
        cells-set! @#'world/cells-set!
        fork-thread (promise)
        t1 (future (sci/eval-string* ctx (str "(vswap! (vs " a-index ") (fn [_] (host/t1) 1))")))
        _ (deref t1-entered 5000 :timeout)
        t2 (future (sci/eval-string*
                    ctx (str "(vswap! (vs " b-index ") (fn [_] (host/t2-entered)"
                             " (host/await-written (vs " a-index ")) @(vs " a-index ")))")))
        _ (deref t2-entered 5000 :timeout)
        child (with-redefs [world/cells-set!
                            (fn [cells slot value]
                              (when (and (identical? (Thread/currentThread) @fork-thread)
                                         (= slot a-slot))
                                ;; a's root has been read for the child
                                (deliver t1-go true)
                                (await-condition 1000 #(= 1 @b)))
                              (cells-set! cells slot value))]
                (deref (future (deliver fork-thread (Thread/currentThread)) (sci/fork ctx))
                       10000 ::timeout))]
    (deliver t1-go true)
    (is (= 1 (deref t1 5000 ::timeout)))
    (deref t2 5000 ::timeout)
    (is (map? child))
    (when (map? child)
      (let [[ca cb] (sci/eval-string* child (str "[@(vs " a-index ") @(vs " b-index ")]"))]
        (is (<= cb ca) (str "child has b=" cb " without its prerequisite a=" ca))))))

(deftest a-mutable-primary-write-racing-the-first-fork-keeps-slot-and-root-equal
  (let [entered (promise) go (promise)
        ctx (forkable {:namespaces {'host {'entered #(deliver entered true)
                                           'wait #(deref go 5000 :timeout)}}})
        v (sci/eval-string* ctx "(def v (volatile! 0)) v")
        v-slot (slot-of ctx v)
        t1 (future (sci/eval-string* ctx "(vswap! v (fn [_] (host/entered) (host/wait) 1))"))
        _ (deref entered 5000 :timeout)
        _ (sci/fork ctx)                    ; realizes the primary meanwhile
        cells-set! @#'world/cells-set!
        t1-thread (promise)]
    (with-redefs [world/cells-set!
                  (fn [cells slot value]
                    (when (and (= slot v-slot) (= 1 value)
                               (deliver t1-thread (Thread/currentThread))
                               (identical? (Thread/currentThread) @t1-thread))
                      ;; t1 is about to publish 1: another writer commits 2 first
                      (deref (future (sci/eval-string* ctx "(vreset! v 2)")) 2000 :timeout))
                    (cells-set! cells slot value))]
      (deliver go true)
      (deref t1 5000 ::timeout))
    (is (= (sci/eval-string* ctx "@v") (sci/eval-string* (sci/fork ctx) "@v"))
        "a later fork copies the value the primary reads")))
