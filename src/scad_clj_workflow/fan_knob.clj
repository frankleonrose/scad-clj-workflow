(ns scad-clj-workflow.fan-knob
  (:require [scad-clj.model :as m]
            [scad-clj-workflow.helpers :refer [render]]
            [scad-clj-workflow.extra.clip :refer [clip largest-dimension]]
            [scad-clj-workflow.extra.calc :refer [calc]]))

(defn torus [od td]
  (->> (m/circle (/ td 2))
       (m/translate [(/ (- od td) 2) 0 0])
       (m/extrude-rotate {:convexity 10})))

(defn in-to-mm [in] (* in 25.4))
(def shaft-diameter-mm (in-to-mm 0.25))
(def shaft-d-cut-radius (/ (in-to-mm 0.05) 2))

(def motor-diameter-mm 10)
(def motor-length-mm 10)
(def motor-housing-protrusion-mm
  "How far from bottom the motor housing protrudes in order to actually grip shaft well."
  0)

(def shaft-hole
  (->> (m/union
        (m/cylinder (/ shaft-diameter-mm 2) 6 :center false)
        (->> (m/cylinder [(/ shaft-diameter-mm 2) 0] 4 :center false)
             (m/translate [0 0 6])))
       (clip [shaft-d-cut-radius 0 0] [1 0 0])
       (m/translate [0 0 (- motor-housing-protrusion-mm)])))

(def motor-housing
  (let [motor-radius-mm (/ motor-diameter-mm 2)]
    (->> (m/with-fn 6
           (m/cylinder motor-radius-mm motor-length-mm :center false))
         (m/translate [0 0 (- motor-housing-protrusion-mm)]))))

(def grill-diameter-mm 40)
(def grill-thickness-mm 0.25)
(def grill-hole-mm 1.5)
(def grill-name-diameter-mm 5)
(def grill-name-thickness-mm 1)

(def vent-curve-radius-mm 3)
(def vent-thickness-mm 0.5)
(def vent-spacing-mm 1)

(def bottom-grill-diameter-mm (+ grill-diameter-mm (* 2 vent-curve-radius-mm) (- (* 2 vent-thickness-mm))))

(def grill-holes
  (let [grill-radius-mm (/ bottom-grill-diameter-mm 2)
        grill-cell-mm (+ grill-hole-mm grill-thickness-mm)]
    (apply m/union
           (for [x (range (inc (int (/ bottom-grill-diameter-mm (+ grill-hole-mm grill-thickness-mm)))))
                 y (range (inc (int (/ bottom-grill-diameter-mm (+ grill-hole-mm grill-thickness-mm)))))
                 :let [x (+ (- grill-radius-mm) (* x grill-cell-mm))
                       y (+ (- grill-radius-mm) (* y grill-cell-mm))]]
             (->> (m/cube grill-hole-mm grill-hole-mm 100)
                  (m/translate [x y 0]))))))

(def bottom-grill
  (m/difference
   (m/cylinder (/ bottom-grill-diameter-mm
                  2) grill-thickness-mm :center false)
   grill-holes
   shaft-hole))

(def fan-height-mm 19)
(def top-grill
  (let [grill-shape (->> (m/sphere (/ grill-diameter-mm 2))
                         (m/scale [1 1 0.25])
                         (clip [0 0 0] [0 0 -1]))
        grill-shrink (/ (- grill-diameter-mm (* grill-thickness-mm 2))
                        grill-diameter-mm)]
    (->> (m/difference
          (m/difference
           ;; Make a shell of grill shape
           grill-shape
           (->> grill-shape
                (m/translate [0 0 (- grill-thickness-mm)])
                (m/scale [grill-shrink grill-shrink 1])))
          grill-holes)
         (m/union
          (torus grill-diameter-mm 3)
          ;; Nameplate
          (->> (m/cylinder (/ grill-name-diameter-mm 2) grill-name-thickness-mm)
               (m/translate [0 0 (/ grill-diameter-mm 8)])))
         (m/translate [0 0 fan-height-mm]))))

(def vent
  (->> (m/difference
        (m/circle vent-curve-radius-mm)
        (m/circle (- vent-curve-radius-mm vent-thickness-mm))
        (->> (m/square 10 10)
             (m/translate [5 0 0]))
        (->> (m/square 10 10)
             (m/translate [0 5 0])))
       (m/translate [(+ vent-curve-radius-mm (- vent-thickness-mm) (/  grill-diameter-mm  2)) 0 0])
       (m/extrude-rotate {:convexity 10})
       (m/translate [0 0 vent-curve-radius-mm])
       (m/union (torus (+ grill-diameter-mm (* 2 vent-curve-radius-mm)) 1))))

(def vents
  (->> (for [z (range 5)
             :let [z (* z (+ vent-curve-radius-mm vent-spacing-mm))]]
         (->> vent
              (m/translate [0 0 z])))
       (apply m/union)
       (clip [0 0 0] [0 0 -1])))

(def strut-thickness-mm 1)
(def strut-width-mm  4)

(def strut
  "Major strut on inside of vent rings."
  (->> (m/cube strut-thickness-mm strut-width-mm fan-height-mm)
       (clip [0 0 (- (/ fan-height-mm 2) 0.5)] [1 0 1])
       (clip [0 0 (- (/ fan-height-mm 2) 0.5)] [-1 0 1])
       (m/translate [(+ (/  grill-diameter-mm  2) (- (/ strut-thickness-mm 2))) 0 (/ fan-height-mm 2)])))
(def struts
  (->> (for [angle (range 4)
             :let [angle (* angle Math/PI 0.5)]]
         (->> strut
              (m/rotate [0 0 angle])))
       (apply m/union)))

(def mini-strut
  "Narrow strut between primary struts for added rigidity."
  (->> (m/cube strut-thickness-mm (/ strut-width-mm 2) fan-height-mm)
       (m/translate [(+ (/  grill-diameter-mm  2) (- (/ strut-thickness-mm 2))) 0 (/ fan-height-mm 2)])))
(def mini-struts
  (->> (for [angle (range 4)
             :let [angle (* (+ angle 0.5) Math/PI 0.5)]]
         (->> mini-strut
              (m/rotate [0 0 angle])))
       (apply m/union)))

(def strut-socket
  "Socket in which a strut from bottom piece inserts into top piece."
  (->> (m/cube (* 2 strut-thickness-mm) (+ strut-width-mm (* 2 strut-thickness-mm)) fan-height-mm)
       (m/translate [(+ (/  grill-diameter-mm  2) (- (/ strut-thickness-mm 2))) 0 (/ fan-height-mm 2)])))
(def strut-sockets
  (->> (for [angle (range 4)
             :let [angle (* angle Math/PI 0.5)]]
         (->> strut-socket
              (m/rotate [0 0 angle])))
       (apply m/union)))

(def mount-width-mm 3)

(def mount
  "Radial shape attaching motor mount to strut."
  (->> (m/cylinder motor-length-mm (/ grill-diameter-mm 2))
       (m/rotate [0 (/ Math/PI 2) 0])
       (m/scale [1 (/ mount-width-mm motor-length-mm 2) 1])
       (clip [0 0 0] [0 0 -1])
       (m/translate [(/ grill-diameter-mm 4) 0 grill-thickness-mm])))
(def mounts
  (m/difference
   (->> (for [angle (range 4)
              :let [angle (* angle Math/PI 0.5)]]
          (->> mount
               (m/rotate [0 0 angle])))
        (apply m/union))
   shaft-hole))

(def shaft-strut
  (->> (m/cylinder (/ shaft-diameter-mm 2) (+ fan-height-mm (/ grill-diameter-mm 8) motor-housing-protrusion-mm (- motor-length-mm)) :center false)
       #_(clip [shaft-d-cut-radius 0 0] [1 0 0])
       (m/translate [0 0 (- motor-length-mm motor-housing-protrusion-mm)])))

(def prop-length-mm (/ grill-diameter-mm 4))
(def prop-thickness-mm 1)
(def prop-height-mm (+ motor-length-mm 5))

(def prop
  (->> (m/sphere prop-length-mm)
       (m/scale [1 0.75 (/ prop-thickness-mm prop-length-mm)])
       (m/rotate [(/ Math/PI 5) 0 0])
       (m/translate [prop-length-mm 0 prop-height-mm])))
(def props
  (->> (for [angle (range 3)
             :let [angle (+ (* angle Math/PI (/ 2 3)) (* Math/PI 0.07))]]
         (->> prop
              (m/rotate [0 0 angle])))
       (apply m/union)))

(def fan
  (m/union
   #_(m/fn! 50)
   (m/fa! 1)
   (m/fs! 1)
   bottom-grill
   top-grill
   struts
   vents
   mounts
   ;;props
   mini-struts
   shaft-strut
   (m/difference
    motor-housing
    shaft-hole)))

(render
 (let [slice-height-mm (- (* 3 (+ vent-curve-radius-mm vent-spacing-mm)) 0.9)]
   (m/union
    (->> fan
         (clip [0 0 slice-height-mm])
         (m/union struts))
    (->> (m/difference
          (->> (m/union fan strut-sockets)
               (clip [0 0 slice-height-mm] [0 0 -1])
               (m/union props))
          ;; Subtract slightly pumped struts to make holes the actual struts will fit into
          (m/minkowski struts (m/sphere 0.1)))
         (m/translate [60 0 (- 0 fan-height-mm (/ grill-diameter-mm 8) (/ grill-name-thickness-mm 2))])
         (m/rotate [Math/PI 0 0])
#_         (m/translate [0 0 (+ slice-height-mm (/ grill-diameter-mm 8) (/ grill-name-thickness-mm 2))])))))

