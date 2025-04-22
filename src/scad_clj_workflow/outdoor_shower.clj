(ns scad-clj-workflow.outdoor-shower
  (:require [scad-clj.model :as m]
            [scad-clj-workflow.helpers :refer [render]]))

(def light-color [9/10 9/10 7/10 1])
(def wood-color [8/10 6/10 4/10 1])
(def pipe-color [1/10 1/10 1/10 1])
(def pipe-inside-color [1 1 1 1])
(def window-color [1 1 1 1])
(def glass-color [0.7 0.7 1 1])

(defn inches-to-mm [inches]
  (* inches 25.4))

(defn feet-to-mm [feet]
  (* feet 12 25.4))

(def corrugation-period-mm 67.5)
(def corrugation-amplitude-mm (inches-to-mm 0.5))
(defn corrugations [distance-mm]
  (/ distance-mm corrugation-period-mm))

(defn flat-corrugated-side
  ([width phase]
   (flat-corrugated-side width phase 0))
  ([width phase delta-y]
   (let [waves (corrugations width)
         steps (int (* waves 20))
         rad-per-step (* waves 2 Math/PI (/ steps))]
     (for [i (range steps)
           :let [modulate (* corrugation-amplitude-mm (Math/sin (+ (* rad-per-step i) phase)))
                 x (* i width (/ 1 steps))
                 y (+ modulate delta-y)]]
       [x y]))))

(defn flat-corrugated
  ([width height] (flat-corrugated width height 0))
  ([width height phase]
   (->> (m/union
         (->>
          (concat (flat-corrugated-side width phase)
                  (reverse (flat-corrugated-side width phase -0.5)))
          (m/polygon)
          (m/extrude-linear {:height height})
          (m/color pipe-color))
         (->>
          (concat (flat-corrugated-side width phase)
                  (reverse (flat-corrugated-side width phase -0.5)))

          (m/polygon)
          (m/extrude-linear {:height height})
          (m/translate [0 0.1 0])
          (m/color pipe-inside-color)))
        (m/translate [(/ width -2) 0 (/ height -2)]))))

(def shower-width-mm (feet-to-mm 6.5))
(def shower-length-mm (feet-to-mm 8))
(def shower-height-mm (feet-to-mm 6))
(def shower-wall-height-mm (feet-to-mm 5))
(def building-wall-height-mm (feet-to-mm 6.5))
(def shower-wall-pos-mm (feet-to-mm 5))
(def shower-floor-gap-mm (inches-to-mm 0.5))
(def shower-base-posts-count 3)
(def inner-door-width (/ shower-width-mm 2)
  #_(feet-to-mm 3))
(def inner-wall-width (- shower-width-mm inner-door-width))
(def window-width (feet-to-mm 2.5))
(def shower-wall-support-width-mm (inches-to-mm 1.5))

(defn shower-post [length-mm]
  (->>
   (m/cube length-mm (inches-to-mm 3.5) (inches-to-mm 3.5))
   (m/color wood-color)))

(def shower-board-width-mm (inches-to-mm 5.5))
(def shower-wall-support-height-mm (inches-to-mm 3.5))

(defn shower-board [length-mm]
  (->>
   (m/cube length-mm shower-board-width-mm (inches-to-mm 1.25))
   (m/color wood-color)))

(defn shower-wall-support [length-mm]
  (->>
   (m/cube length-mm shower-wall-support-height-mm shower-wall-support-width-mm)
   (m/color wood-color)))

(defn shower-wall [width height]
  (let [brace-delta (- (/ shower-wall-height-mm 2) shower-wall-support-width-mm)
        overlap-mm (inches-to-mm 1.5) ;; corrugated overlaps post
        overlap-width (+ width (* 2 overlap-mm))]
    (m/union (->> (m/union (flat-corrugated overlap-width height)
                           (->> (flat-corrugated overlap-width height)
                                (m/color pipe-inside-color)
                                (m/translate [0 3 0])))
                  (m/translate [(- overlap-mm) (- (inches-to-mm 2.5)) 0]))
             (->> (shower-wall-support width)
                  (m/rotate [(/ Math/PI 2) 0 0])
                  (m/translate [(/ width 2) (- (inches-to-mm 1)) brace-delta]))
             (->> (shower-wall-support width)
                  (m/rotate [(/ Math/PI 2) 0 0])
                  (m/translate [(/ width 2) (- (inches-to-mm 1)) (- brace-delta)])))))

(defn building-wall [width height]
  (->> (m/union (flat-corrugated width height)
                (->> (flat-corrugated width height)
                     (m/color pipe-inside-color)
                     (m/translate [0 3 0])))
       (m/translate [0 (inches-to-mm 1.5) (- (inches-to-mm 2))])))

(defn window []
  (let [width window-width
        height (feet-to-mm 1.75)
        margin-mm (inches-to-mm 2.5)]
    (->> (m/union
          (->> (m/cube width (inches-to-mm 2.5) height )
               (m/color window-color))
          (->> (m/cube (- width (* 2 margin-mm))
                       (inches-to-mm 4.5)
                       (- height (* 2 margin-mm)))
               (m/color glass-color)))
         (m/translate [0 (- (inches-to-mm 1.5)) 0]))))

(render
 (m/union

  (apply m/union
         (for [i (range shower-base-posts-count)]
           (->> (shower-post shower-length-mm)
                (m/translate [0 (* i (/ shower-width-mm (dec shower-base-posts-count))) 0]))))

  (->> (apply m/union
              (let [board-and-gap-mm (+ shower-floor-gap-mm shower-board-width-mm)
                    count (quot shower-length-mm board-and-gap-mm)]
                (for [i (range count)]
                  (->> (shower-board shower-width-mm)
                       (m/translate [0 (* (+ i 0.5) board-and-gap-mm) 0])))))
       (m/rotate [0 0 (* Math/PI 0.5)])
       (m/translate [(/ shower-length-mm 2) (/ shower-width-mm 2) (/ (inches-to-mm 3.5) 2)]))

  (apply m/union (let []
                   (for [[x y height] [[0 0 building-wall-height-mm]
                                [shower-length-mm 0 building-wall-height-mm]
                                [0 shower-width-mm building-wall-height-mm]
                                [shower-length-mm shower-width-mm shower-height-mm]
                                [shower-wall-pos-mm shower-width-mm shower-height-mm]
                                [shower-wall-pos-mm inner-door-width shower-height-mm]]]
                     (->> (shower-post height)
                          (m/rotate [0 (* Math/PI 0.5) 0])
                          (m/translate [(- x (/ shower-length-mm 2))
                                        y
                                        (/ (+ (inches-to-mm 3.5)
                                              (inches-to-mm 1.25)
                                              height) 2)])))))

  (->> (m/union (->> (building-wall shower-length-mm building-wall-height-mm)
                     (m/translate [(- (/ shower-length-mm 2)) 0 0])
                     (m/rotate [0 0 Math/PI]))

                (->> (window)
                     (m/translate [0 0 (feet-to-mm 2.5)]))

                (->> (building-wall shower-width-mm building-wall-height-mm)
                     (m/rotate [0 0 (/ Math/PI 2)])
                     (m/translate [(- (/ shower-length-mm 2)) 0 0]))

                (->> (window)
                     (m/rotate [0 0 (/ Math/PI 2)])
                     (m/translate [(- (/ shower-length-mm 2))
                                   (feet-to-mm 5)
                                   (feet-to-mm 2.5)]))

                (->> (shower-wall (feet-to-mm 5) shower-wall-height-mm)
                     (m/rotate [0 0 Math/PI])
                     (m/translate [(- (feet-to-mm 5) (/ shower-length-mm 2)) shower-width-mm 0]))

                (->> (shower-wall shower-width-mm shower-wall-height-mm)
                     (m/rotate [0 0 (/ Math/PI 2)])
                     (m/translate [(/ shower-length-mm 2) 0 0]))

                (->> (shower-wall inner-wall-width shower-wall-height-mm)
                     (m/rotate [0 0 (/ Math/PI 2)])
                     (m/translate [(- (feet-to-mm 5) (/ shower-length-mm 2)) inner-door-width 0])))

       (m/translate [0 0 (+ (/ shower-wall-height-mm 2)
                            (feet-to-mm 1))]))))