(ns tracer.ray
  [:require [tracer.vecX :refer :all]])

(defrecord Ray [org dir])
(defn parametarize-ray
  "Vector math to get position on a ray."
  [ray t] (v_+ (:org ray) (f_* (:dir ray) t)))