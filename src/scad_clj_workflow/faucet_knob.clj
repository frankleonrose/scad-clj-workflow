;; https://github.com/PEZ/scad-clj-workflow
;;
;; Designed, printed, and installed on 2026-10-09
;; (The week I finished assembling the Prusa Core ONE+)
;;
;; Printed in black PETG. Removing the scaffolding was a bit of a challenge with a pen knife.
;;
;; At this original diameter ~40mm, it feels a little too difficult to turn.
;; I could reprint larger, but will wait for more feedback.
;;
(ns scad-clj-workflow.faucet-knob
  (:require [scad-clj.model :as m]
            [scad-clj-workflow.helpers :refer [render]]))

(defn in-to-mm [in] (* in 25.4))

(def post-height 5)
(def base-radius 7)
(def petal-radius 5)
(def petal-count 29)

(def post
  "The square shaft of the valve. Screw hole in the center."
  (let [post-side (in-to-mm 0.30)
        post-height 15
        post-above-baseline 5]
    (->> (m/cube post-side post-side post-height)
         (m/translate [0 0 (+ (- (/ post-height 2)) post-above-baseline)]))))

(def screw
  (binding [m/*fn* 40
            m/*fs* 0.1
            m/*fa* 0.1]
    (let [shaft-radius (/ (in-to-mm 0.15) 2)
          head-radius (/ (in-to-mm 0.30) 2)
          head-height 2]
      (->> (m/union
            ;; screw shaft (roughly)
            (->> (m/cylinder shaft-radius 15 :center false)
                 (m/rotate [Math/PI 0 0]))
            ;; screw head
            (->> (m/cylinder [shaft-radius head-radius] head-height :center false)
                 (m/scale [1 1 1.1])
                 (m/translate [0 0 -0.1]))
            (->> (m/cylinder head-radius 15 :center false)
                 (m/translate [0 0 head-height])))
           (m/translate [0 0 post-height])))))

(def knob-base
  (let [base (m/cylinder base-radius post-height :center false)
        base-top (->>
                  (m/cylinder base-radius 2 :center false)
                  (m/translate [0 0 post-height]))
        model (-> (m/union
                   base base-top)
                  (m/difference post screw))]
    model))

(binding [m/*fn* 40
          m/*fs* 2
          m/*fa* 12]
  (let [petals (->> (for [i (range petal-count)]
                      (->> (m/cylinder petal-radius 1)
                           (m/scale [1.9 1 1])
                           (m/rotate [(/ Math/PI 2) 0 0])
                           (m/translate [(+ petal-radius base-radius -2) 0 0])
                           (m/rotate [0 0 (* i (/ petal-count) Math/TAU)])))
                    (apply m/union)
                    (m/translate [0 0 4]))
        petals (-> petals
                   (m/minkowski (m/sphere 0.5))
                   (m/difference post screw))
        model (m/union
               ;;screw ;; to see placement
               knob-base
               petals)]
    (render model)))