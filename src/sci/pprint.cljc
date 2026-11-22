(ns sci.pprint
  "Require this namespace if you want to extend pretty-printing to
  records created with SCI."
  {:no-doc true}
  (:require
   [clojure.pprint :as pprint]
   [sci.impl.records]
   [sci.impl.world :as world]
   [sci.lang]))

#?(:clj (set! *warn-on-reflection* true))

(defmethod pprint/simple-dispatch sci.impl.records.SciRecord [obj]
  ;; A user dispatch renders whole (buffered) with the home admitted; a
  ;; retired home, or a retired world met while rendering, prints the fields.
  (if-let [pm (world/printing-entry (.-type-meta ^sci.impl.records.SciRecord obj)
                                    :sci.impl/pprint-simple-dispatch)]
    (let [s (world/call-with-type-admitted
             (.-type ^sci.impl.records.SciRecord obj)
             #(with-out-str (pm obj))
             (constantly nil))]
      (if s
        (print s)
        (pprint/simple-dispatch (into {} obj))))
    (pprint/simple-dispatch (into {} obj))))

(defmethod pprint/simple-dispatch sci.lang.Var [obj]
  (pr obj))
