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
