(ns tracer.with-type
  (:gen-class)
  (:require [tracer.imaging :refer :all]
            [tracer.vecX :refer :all]
            [tracer.primitives :refer :all]
            [tracer.ray :refer :all]
            [tracer.camera :refer :all]
            [tracer.light-sources :refer :all]))


;; Repeated the render function 5 times and took the time to get the average when using clojures
;; custom deftype construct. The elapsed time was 151929.321 msecs. On average thats 30385.8642 msecs. For the
;; version in which I used the defrecord construct the elapsed time was 9572.1496 msecs. On average this
;; is 1914.42992msecs. The drastic differences in time was quite shocking to me. I expected the custom
;; datatype to be much faster. This makes partial sense though as the Clojure docs strongly recommend
;; using records for most things. I believe the general rule of thumb is to only use types when some sort
;; of polymorphism is required, or when you want to retain low level control of your data construct.

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn gradient-color [ray]
  "Method to derive gradient rgb value from ray."
  (let [uv (unit-vector (:dir ray))
        t (* 0.5 (+ (.y uv) 1.0))]
    (v_+
      (f_* (->vec3t 1 1 1) (- 1.0 t))
      (f_* (->vec3t 0.5 0.7 1.0) t))))

(defrecord Hit [shape hit-loc t])

(defn get-closest [ray scene]
  "Gets the first object that intersects with the ray."
  (let [int-map (reduce (fn [c so]
                          (let [intersection_point (intersect so ray)]
                            (if (>= intersection_point 0)
                              (conj c (->Hit so (parametarize-ray ray intersection_point) intersection_point))
                              c)))
                        []
                        scene)]
    (if (empty? int-map)
      nil
      (apply (partial min-key :t) int-map))))

(defn can-see-light [light scene intersection-loc]
  "Sees if any objects are in the way of the light."
  (let [direction-to-light (unit-vector (v_- (:loc light) intersection-loc))
        light-ray (->Ray intersection-loc direction-to-light)]
    (not (some #(> (intersect % light-ray) 0) scene))))

(defn diffuse [L N clr kd]
  "Calculates diffuse component of the lighting model."
  (f_* clr (* 0.333 kd (max 0 (dot L N)))))

(defn spectral [SC R V ks alpha]
  "Calcualtes the spectral component of the lighting model."
  (let [a (dot R V)
        b (max a 0)
        c (Math/pow b alpha)
        d (* ks c)
        e (f_* SC d)])
  (f_* SC (* 0.333 ks (Math/pow (max (dot R V) 0) alpha))))

(defn diffusev2 [L N kd]
  (* kd (max 0 (dot L N))))

(defn phong [light hit ray spec-clr]
  "Does computation of phong lighting components."
  (let [;; Normal of intersection on sphere.
        N (unit-vector (v_- (:hit-loc hit) (get-in hit [:shape :center])))
        ;; Direction to light source.
        L (unit-vector (v_- (:loc light) (:hit-loc hit)))
        ;; Direction to viewpoint.
        V (f_* (:dir ray) -1)
        ;; Reflection vector.
        R (unit-vector (v_- (f_* (f_* N (dot N L)) 2) L))
        ;; Phone parameters.
        ks 3 alpha 100]
    (v_+
      ;; Diffuse component.
      (diffuse L N (get-in hit [:shape :color]) 1)
      ;; Spectral component
      (spectral spec-clr R V ks alpha))))


(defn color [ray scene light-sources]
  "Find color of ray within the scene."
  (let [spectral-color (->vec3t 1 1 1)
        closest-hit (get-closest ray scene)]
    ;;See if we hit an object in the scene.
    (if closest-hit
      (f_div (reduce (fn [lclr ls]
                       ;; If the object can see the light apply phong model to it.
                       (if (can-see-light ls scene (:hit-loc closest-hit))
                         (v_+ lclr (phong ls closest-hit ray spectral-color))
                         lclr))
                     (->vec3t 0 0 0)
                     light-sources)
             (count light-sources))
      (gradient-color ray))))

(defn render [camera scene light-sources anti-alias filename]
  "Another simple test method."
  (let [; Setting up needed values for the scene and camera location.
        w (:w camera)
        h (:h camera)
        new-image (create-image w h)]
    ;; Reduce over image pixels.
    (reduce (fn [img [x y]]
              (let [col (f_div
                          ;; Second reduce is for anti-aliasing.
                          (reduce (fn [clr _] (v_+ clr
                                                   (let [ray (generate-ray camera x y anti-alias)]
                                                     ;; Find the color for the specific ray in the scene.
                                                     (color ray scene light-sources))))
                                  (->vec3t 0 0 0)
                                  (range anti-alias))
                          anti-alias)]
                (set-pixel! img x y (RGB->Scalar col))
                img))
            new-image
            (for [x (range w) y (range h)] [x y]))
    (save-image new-image filename)))


;; Example call to render.
(defn run-render []
  (let [w 640 h 480
        camera (->AspectCam w h 30 (->vec3t 0 0 0))
        scene [(->Sphere (->vec3t 0.0 -10004 -20.0) 10000 (->vec3t 0.70, 0.54, 0.33))
               (->Sphere (->vec3t 0 0 -20) 4 (->vec3t 1.00, 0.32, 0.36))
               (->Sphere (->vec3t 0 0 -100) 4 (->vec3t 1.00, 0.32, 0.36))
               (->Sphere (->vec3t 5 -1 -15) 2 (->vec3t 0.90, 0.76, 0.46))
               (->Sphere (->vec3t 5 0 -25) 3 (->vec3t 0.65, 0.77, 0.97))
               (->Sphere (->vec3t -5.5 0, -13) 3 (->vec3t 0.90, 0.90, 0.90))]
        light-sorces [(->PointSource (->vec3t -60.0, 60, 60))
                      (->PointSource (->vec3t 60.0, 60, 60))
                      (->PointSource (->vec3t 0.0, 60, 60))]]
    (render camera scene light-sorces 1 "./resources/trace.png")))
