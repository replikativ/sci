(ns sci.impl.syntax
  "Materialization of syntax returned by trusted native macros.

   Native syntaxquote uses host lazy sequence caches. Code consumers may
   inspect or splice those forms inside SCI, so they need immutable syntax
   rather than a foreign runtime cursor. Quoted data remains opaque."
  {:no-doc true})

(defn- host-lazy? [value]
  #?(:cljd false
     :clj (instance? clojure.lang.LazySeq value)
     :cljs (instance? cljs.core/LazySeq value)))

(declare canonicalize)

(defn- syntax-seq [form]
  (let [head (first form)
        quoted? (and (symbol? head) (or (= 'quote head) (= 'clojure.core/quote head)))]
    (loop [items (seq form) out [] changed? (host-lazy? form)]
      (if items
        (let [item (first items)
              projected (if (and quoted? (seq out)) item (canonicalize item))
              tail (rest items)]
          (recur (seq tail) (conj out projected)
                 (or changed? (not (identical? item projected)) (host-lazy? tail))))
        (if changed?
          (with-meta
            (if (list? form)
              (apply list out)
              (reduce (fn [tail item] (cons item tail)) () (rseq out)))
            (meta form))
          form)))))

(defn canonicalize
  "Eagerly materialize a native macro's code collections. This is a syntax
   boundary, not an adapter for arbitrary runtime values or quoted payloads.
   Reuse unchanged immutable input subtrees and preserve their metadata."
  [form]
  (cond
    (seq? form) (syntax-seq form)

    (vector? form)
    (let [changed? (volatile! false)
          result (mapv (fn [item]
                         (let [projected (canonicalize item)]
                           (when-not (identical? item projected) (vreset! changed? true))
                           projected))
                       form)]
      (if @changed? (with-meta result (meta form)) form))

    (and (map? form) (not (record? form)))
    (let [changed? (volatile! false)
          entries (reduce-kv
                  (fn [out k v]
                    (let [new-k (canonicalize k) new-v (canonicalize v)]
                      (when-not (and (identical? k new-k) (identical? v new-v))
                        (vreset! changed? true))
                      (conj out [new-k new-v])))
                  [] form)]
      (if @changed? (with-meta (into (empty form) entries) (meta form)) form))

    (set? form)
    (let [changed? (volatile! false)
          items (reduce (fn [out item]
                           (let [projected (canonicalize item)]
                             (when-not (identical? item projected) (vreset! changed? true))
                             (conj out projected)))
                         [] form)]
      (if @changed? (with-meta (into (empty form) items) (meta form)) form))

    :else form))
