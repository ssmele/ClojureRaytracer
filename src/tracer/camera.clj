(ns tracer.camera
  (:require [tracer.ray :refer :all]
            [tracer.vecX :refer :all]))


(defprotocol SceneCamera
  "Protocol for cameras within the scene.

  Must be able to generate ray from given pixel location."
  (generate-ray [this x y aa]))

; "Very basic scene camera. Doesn't produce the best results."
(defrecord Cam
  [llc horz vert org w h]
  SceneCamera
  (generate-ray [_ x y aa]
    (let [u (/ (float x) (float w))
          v (/ (float y) (float h))]
    (->Ray org (v_+ llc (v_+ (f_* horz u) (f_* vert v)))))))

; "More advance camera model that takes into consideration the aspect of the scene."
(defrecord AspectCam
  [w h fov org]
  SceneCamera
  (generate-ray [_ x y aa]
    (let [x (if (> aa 1) (- (+ x (rand)) 0.5) x)
          y (if (> aa 1) (- (+ y (rand)) 0.5) y)
          inv-width (/ 1 (float w))
          inv-height (/ 1 (float h))
          aspect-ratio (/ w (float h))
          ;; Possible math error.
          angle (Math/tan (/ (* Math/PI 0.5 fov) 180))
          ray-dir-x (* (- (*  2 (+ x 0.5) inv-width) 1) angle aspect-ratio)
          ray-dir-y (* (- 1 (* (+ y 0.5) inv-height 2)) angle)]
      (->Ray org (unit-vector (->vec3 ray-dir-x ray-dir-y -1))))))
