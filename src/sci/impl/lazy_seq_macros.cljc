(ns sci.impl.lazy-seq-macros
  "Compile-time helpers kept separate from managed sequence runtime state.
   Self-hosted ClojureScript can load this namespace as macros without
   bootstrapping a second copy of the runtime protocols and types."
  {:no-doc true}
  (:refer-clojure :exclude [lazy-seq]))

(defmacro def-sequence-type [name fields data-methods chunked?]
     (let [cljs? #?(:cljs true :default (boolean (:ns &env)))
           methods
           (if cljs?
             '(cljs.core/ISeqable
               (-seq [this] (seq-value this))
               cljs.core/ISeq
               (-first [this] (seq-first this))
               (-rest [this] (seq-rest this))
               cljs.core/INext
               (-next [this] (seq-next this))
               cljs.core/ICollection
               (-conj [this x] (cons x this))
               cljs.core/IEmptyableCollection
               (-empty [this] (with-meta '() (-seq-metadata this)))
               cljs.core/ISequential
               cljs.core/IEquiv
               (-equiv [this other] (seq-equiv this other true))
               cljs.core/IHash
               (-hash [this] (seq-hash this))
               cljs.core/IMeta
               (-meta [this] (-seq-metadata this))
               cljs.core/IWithMeta
               (-with-meta [this m] (seq-meta-clone this m))
               cljs.core/IPending
               (-realized? [this] (seq-realized? this))
               cljs.core/IPrintWithWriter
               (-pr-writer [this writer opts]
                 (cljs.core/pr-sequential-writer writer cljs.core/pr-writer "(" " " ")" opts this))
               Object
               (toString [this] (seq-string this)))
             '(clojure.lang.Seqable
               (seq [this] (seq-value this))
               clojure.lang.ISeq
               (first [this] (seq-first this))
               (next [this] (seq-next this))
               (more [this] (seq-rest this))
               (^clojure.lang.ISeq cons [this x] (clojure.lang.Cons. x this))
               clojure.lang.IPersistentCollection
               (count [this] (seq-count this))
               (empty [this] (with-meta '() (-seq-metadata this)))
               (equiv [this other] (seq-equiv this other true))
               clojure.lang.Sequential
               clojure.lang.IHashEq
               (hasheq [this] (seq-hash this))
               clojure.lang.IMeta
               (meta [this] (-seq-metadata this))
               clojure.lang.IObj
               (withMeta [this m] (seq-meta-clone this m))
               clojure.lang.IPending
               (isRealized [this] (seq-realized? this))
               java.util.List
               (size [this] (seq-count this))
               (isEmpty [this] (nil? (seq-value this)))
               (contains [this x] (.contains ^java.util.List (java-list this) x))
               (containsAll [this xs] (.containsAll ^java.util.List (java-list this) xs))
               (get [this index] (.get ^java.util.List (java-list this) index))
               (indexOf [this x] (.indexOf ^java.util.List (java-list this) x))
               (lastIndexOf [this x] (.lastIndexOf ^java.util.List (java-list this) x))
               (iterator [this] (seq-iterator this))
               (listIterator [this] (.listIterator ^java.util.List (java-list this)))
               (listIterator [this index] (.listIterator ^java.util.List (java-list this) index))
               (subList [this start end] (.subList ^java.util.List (java-list this) start end))
               (^objects toArray [this] (.toArray ^java.util.List (java-list this)))
               (^objects toArray [this ^objects array] (.toArray ^java.util.List (java-list this) array))
               (^boolean add [_ ^Object _x] (throw (UnsupportedOperationException.)))
               (^void add [_ ^int _index ^Object _x] (throw (UnsupportedOperationException.)))
               (^boolean addAll [_ ^java.util.Collection _xs] (throw (UnsupportedOperationException.)))
               (^boolean addAll [_ ^int _index ^java.util.Collection _xs] (throw (UnsupportedOperationException.)))
               (^boolean remove [_ ^Object _x] (throw (UnsupportedOperationException.)))
               (^Object remove [_ ^int _index] (throw (UnsupportedOperationException.)))
               (removeAll [_ _xs] (throw (UnsupportedOperationException.)))
               (retainAll [_ _xs] (throw (UnsupportedOperationException.)))
               (clear [_] (throw (UnsupportedOperationException.)))
               (set [_ _index _x] (throw (UnsupportedOperationException.)))
               Object
               (equals [this other] (seq-equiv this other false))
               (hashCode [this] (seq-java-hash this))
               (toString [this] (seq-string this))))
           chunk-methods
           (when chunked?
             (if cljs?
               '(cljs.core/IChunkedSeq
                 (-chunked-first [this] (chunk-first* this))
                 (-chunked-rest [this] (chunk-rest* this))
                 cljs.core/IChunkedNext
                 (-chunked-next [this] (chunk-next* this)))
               '(clojure.lang.IChunkedSeq
                 (chunkedFirst [this] (chunk-first* this))
                 (chunkedNext [this] (chunk-next* this))
                 (chunkedMore [this] (chunk-rest* this)))))]
       `(deftype ~name ~fields
          sci.fork/Forkable (~'fork-value [~'this] ~'this)
          sci.impl.lazy-seq/ISequenceData ~@data-methods
          ~@methods ~@chunk-methods)))

(defmacro lazy-seq [& body]
  `(sci.impl.lazy-seq/lazy-seq* (fn [] ~@body)))
