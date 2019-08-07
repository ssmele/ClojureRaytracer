(ns tracer.vecX)

;; Protocol that represents everything a vector needs.
(defprotocol VEC3
  "This protocol represents the needed functionality for the basic unit of mathematical operation throughout
  this ray tracing program."
  (x [_])
  (y [_])
  (z [_])
  (f-op [this op f])
  (f_div [this f]) (f_+ [this f]) (f_* [this f]) (f_- [this f])
  (v-op [this op v])
  (v_div [this v]) (v_+ [this v]) (v_* [this v]) (v_- [this v])
  (dot [this other])
  (cross [this other])
  (unit-vector [this])
  (squared-length [this])
  (length [this]))

;; Record representation of a vector.
(defrecord vec3 [x y z]
  VEC3
  (x [_] x)
  (y [_] y)
  (z [_] z)
  (f-op [_ op f] (vec3. (op x f) (op y f) (op z f)))
  (v-op [_ op {x2 :x, y2 :y, z2 :z}] (vec3. (op x x2) (op y y2) (op z z2)))
  (dot [_ {x2 :x, y2 :y, z2 :z}] (+ (* x x2) (* y y2) (* z z2)))
  (cross [_ {x2 :x, y2 :y, z2 :z}] (vec3. (- (* y z2) (* z y2))
                                          (- (* -1 x z) (* z x2))
                                          (- (* x y2) (* y x2))))
  (squared-length [_] (+ (* x x) (* y y) (* z z)))
  (length [this] (Math/sqrt (squared-length this)))
  (f_+ [this f] (f-op this + f))
  (f_div [this f] (f-op this / f))
  (f_- [this f] (f-op this - f))
  (f_* [this f] (f-op this * f))
  (v_div [this v] (v-op this / v))
  (v_+ [this v] (v-op this + v))
  (v_* [this v] (v-op this * v))
  (v_- [this v] (v-op this - v))
  (unit-vector [this] (f_div this (length this))))

;; Custom clojure type representation.
(deftype vec3t [x y z]
  VEC3
  (x [_] x)
  (y [_] y)
  (z [_] z)
  (f-op [_ op f] (vec3t. (op x f) (op y f) (op z f)))
  (v-op [_ op v] (vec3t. (op x (.x v)) (op y (.y v)) (op z (.z v))))
  (dot [_ v] (+ (* x (.x v)) (* y (.y v)) (* z (.z v))))
  (cross [_ v] (vec3t. (- (* y (.z v)) (* z (.y v)))
                       (- (* -1 x z) (* z (.x v)))
                       (- (* x (.y v)) (* y (.x v)))))
  (squared-length [_] (+ (* x x) (* y y) (* z z)))
  (length [this] (Math/sqrt (squared-length this)))
  (f_+ [this f] (f-op this + f))
  (f_div [this f] (f-op this / f))
  (f_- [this f] (f-op this - f))
  (f_* [this f] (f-op this * f))
  (v_div [this v] (v-op this / v))
  (v_+ [this v] (v-op this + v))
  (v_* [this v] (v-op this * v))
  (v_- [this v] (v-op this - v))
  (unit-vector [this] (f_div this (length this))))
