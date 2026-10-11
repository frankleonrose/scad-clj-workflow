(ns scad-clj-workflow.extra.clip
  (:require [scad-clj.model :as m]))

(def largest-dimension 2000)

(defn norm
  "Calculates the magnitude (length) of a vector."
  [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn dot
  "Calculates the dot product of two vectors."
  [[x1 y1 z1] [x2 y2 z2]]
  (+ (* x1 x2) (* y1 y2) (* z1 z2)))

(defn cross
  "Calculates the cross product or normal vector of two vectors."
  [[x1 y1 z1] [x2 y2 z2]]
  [(- (* y1 z2) (* z1 y2))
   (- (* z1 x2) (* x1 z2))
   (- (* x1 y2) (* y1 x2))])

(defn angle
  "Calculates the angle between two vectors."
  [v1 v2]
  (Math/acos (/ (dot v1 v2) (* (norm v1) (norm v2)))))

(defn clip
  "Clip away all parts of object that are beyond the clipping
     plane defined by the point and normal. When no normal is
     specified, Z+ [0 0 1] is used."
  ([point object]
   (clip point [0 0 1] object))
  ([[x y z :as _point] [xn yn zn :as _normal] object]
   (let [mask (->> (m/cube largest-dimension largest-dimension largest-dimension)
                   (m/translate [0 0 (- (/ largest-dimension 2))]))
         non-zero? (complement zero?)
         xy-projection (Math/sqrt (+ (* xn xn) (* yn yn)))
         [xr yr zr] (if (zero? xy-projection)
                      (if (neg? zn)
                        [Math/PI 0 0] ;; 180 degree rotation
                        [0 0 0] ;; No rotation
                        )
                      [0 (- (Math/atan2 xy-projection zn)) (- (Math/atan2 yn xn))])]
     (prn [xr yr zr])
     (cond->> (m/intersection
               ;; Place and rotate so the part to keep is below Z=0 plane
               (cond->> object
                 :always
                 (m/translate [(- x) (- y) (- z)])
                 (non-zero? zr)
                 (m/rotate [0 0 zr])
                 (non-zero? yr)
                 (m/rotate [0 yr 0])
                 (non-zero? xr)
                 (m/rotate [xr 0 0]))
               mask)
       (non-zero? xr)
       (m/rotate [(- xr) 0 0])
       (non-zero? yr)
       (m/rotate [0 (- yr) 0])
       (non-zero? zr)
       (m/rotate [0 0 (- zr)])
       :always
       (m/translate [x y z])))))
