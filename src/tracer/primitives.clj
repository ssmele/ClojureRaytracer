(ns tracer.primitives
  (:require [tracer.vecX :refer :all]))

(defprotocol PRIMITIVE
  "Protocol for all primitives to follow."
  (intersect [this ray]))

(defrecord Sphere [center radius color]
  PRIMITIVE
  (intersect [_ ray] (let [oc (v_- (:org ray) center)
                           a (dot (:dir ray) (:dir ray))
                           b (* (dot oc (:dir ray)) 2.0)
                           c (- (dot oc oc) (* radius radius))
                           discrim (- (* b b) (* 4 a c))]
                       (if (>= discrim 0)
                         (min (/ (+ (* -1 b) (Math/sqrt discrim)) (* 2 a))
                              (/ (- (* -1 b) (Math/sqrt discrim)) (* 2 a)))
                         -1.0))))