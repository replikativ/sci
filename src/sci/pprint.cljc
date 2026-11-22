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
  ;; A user dispatch writes straight to the pretty writer (as upstream),
  ;; with the home admitted; a home already retired prints the fields.
  (if-let [pm (world/printing-entry (.-type-meta ^sci.impl.records.SciRecord obj)
                                    :sci.impl/pprint-simple-dispatch)]
    (world/call-with-type-admitted-streaming
     (.-type ^sci.impl.records.SciRecord obj)
     #(pm obj)
     (fn [_] (pprint/simple-dispatch (into {} obj))))
    (pprint/simple-dispatch (into {} obj))))

(defmethod pprint/simple-dispatch sci.lang.Var [obj]
  (pr obj))
