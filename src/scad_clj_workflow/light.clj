;; https://github.com/PEZ/scad-clj-workflow
(ns scad-clj-workflow.light
  (:require [scad-clj-workflow.outdoor-shower :refer [inches-to-mm] :as shower]
            [scad-clj.model :as m]
            [scad-clj-workflow.helpers :refer [render]]))

(def mid-block (->> (m/cube 6 6 1)
                    (m/translate [0 0 15])))

(defn leg [v]
  (->> (m/cube 5 5 1)
       (m/translate v)
       (m/hull mid-block)))
#_
(render (m/union (leg [15 10 0])
                 (leg [15 -10 0])
                 (leg [-15 10 0])
                 (leg [-15 -10 0])))

(def light-color [9/10 9/10 7/10 1])
(def wood-color [8/10 6/10 4/10 1])
(def pipe-color [1/10 1/10 1/10 1])
(def pipe-inside-color [1 1 1 1])

(defn bar-light []
  (m/union
   (->>
    (m/cube 1040 100 100)
    (m/color wood-color))
   (->>
    (m/cube 1000 15 5)
    (m/translate [0 0 -50])
    (m/color light-color))))

(defn pipe-light []
  (m/union
   (->>
    (m/cylinder 15 1040)
    (m/rotate [0 (/ m/pi 2) 0])
    (m/hull)
    (m/color pipe-color))
   (->>
    (m/cube 1000 15 5)
    (m/translate [0 0 -15])
    (m/color light-color))))

  (defn corrugation [width]

  (->> (m/square 5 width)
       (m/translate [150 0 0])
       (m/extrude-rotate {:angle 90 :convexity 1})
       (m/translate [-150 0 0])
       (m/rotatev 45 [0 0 100])
       #_
       (m/extrude-rotate {:angle 90 :convexity 1.1})))

(def corrugation-period-mm 70)
(def corrugation-amplitude-mm 10)
(defn corrugations [distance-mm]
  (quot distance-mm corrugation-period-mm))

(defn curved-corrugated-side [fraction-of-circle radius]
  (let [steps 200
        waves (corrugations (* 2 Math/PI fraction-of-circle radius))
        arc (* 2 Math/PI fraction-of-circle (/ steps))
        wavelet (* (+ waves 0.5) 2 Math/PI (/ steps))]
    (for [i (range steps)
          :let [modulate (* corrugation-amplitude-mm (Math/sin (* wavelet i)))
                radius (+ radius modulate)
                x (* radius (Math/cos (* i arc)))
                y (* radius (Math/sin (* i arc)))]]
      [x y])))

(defn curved-corrugated [fraction-of-circle radius height]
  (->>
   (concat (curved-corrugated-side fraction-of-circle (dec radius))
           (reverse (curved-corrugated-side fraction-of-circle radius)))
   (m/polygon)
   (m/extrude-linear {:height height})
   (m/color pipe-color)))

(defn flat-corrugated [width length]
  (->> (shower/flat-corrugated width length)
       (m/rotate [(/ Math/PI -2) 0 (/ Math/PI 2)])
       (m/translate [(/ length -2) 0 0])))

(defn strip [length]
  (->>
   (m/cube length 15 5)
   (m/rotate [0 0 (* Math/PI 0.5)])
   (m/translate [(- 7.5) 0 0])
   (m/color light-color)))

(defn rod [length]
  (->>
   (m/cube length (inches-to-mm 0.5) (inches-to-mm 0.5))
   #_(m/rotate [0 0 (* Math/PI 0.5)])
   #_(m/translate [(- 7.5) 0 0])
   (m/color pipe-color)))

(defn mm-to-inches [mm]
   (float (/ (Math/round (* 10 (/ mm 25.4))) 10)))

(render
 (m/union

  #_#_(curved-corrugated 0.5 500 1060)
    (flat-corrugated 500 700)

  ;; Curved shade wiith cross bar
  (let [arc-length 0.3
        radius 350
        length 1060
        cross-bar 475
        post-delta (- (/ length 2) 50)]
    (->> (m/union
          (->> (m/union
                (curved-corrugated arc-length radius 1060)
                (->> (curved-corrugated arc-length (- radius 3) 1060)
                     (m/color pipe-inside-color)))
               (m/rotate [(* -0.5 Math/PI) (* (- 0.5 arc-length) Math/PI) 0])
               (m/rotate [0 Math/PI 0]))
          (->> (m/cube 10 10 260)
               (m/color pipe-color)
               (m/translate [0 post-delta 270]))
          (->> (m/cube 10 10 260)
               (m/color pipe-color)
               (m/translate [0 (- post-delta) 270]))
          (->> (m/cube 10 10 cross-bar)
               (m/rotate [0 (/ Math/PI 2) 0])
               (m/color pipe-color)
               (m/translate [0 post-delta 250]))
          (->> (m/cube 10 10 cross-bar)
               (m/rotate [0 (/ Math/PI 2) 0])
               (m/color pipe-color)
               (m/translate [0 (- post-delta) 250]))
          (->> (m/union
                (->> (m/cube length 10 3)
                     (m/color light-color)
                     (m/rotate [0 0 (/ Math/PI 2)])
                     (m/translate [15 0 0]))
                (->> (m/cube length 10 3)
                     (m/color light-color)
                     (m/rotate [0 0 (/ Math/PI 2)])
                     (m/translate [-15 0 0]))
                (->> (m/cube (* 2 (- post-delta 30)) 10 3)
                     (m/color light-color)
                     (m/rotate [0 0 (/ Math/PI 2)])
                     (m/translate [0 0 0]))
                (->> (m/cube length 40 3)
                     (m/color pipe-color)
                     (m/rotate [0 0 (/ Math/PI 2)])
                     (m/translate [0 0 (- 3)]))

                (->> (m/cube length 20 3)
                     (m/color pipe-color)
                     (m/rotate [(/ Math/PI 4) 0 (/ Math/PI 2)])
                     (m/translate [-26 0 4]))
                (->> (m/cube length 10 3)
                     (m/color light-color)
                     (m/rotate [(/ Math/PI 4) 0 (/ Math/PI 2)])
                     (m/translate [-26 0 8]))
                (->> (m/cube length 20 3)
                     (m/color pipe-color)
                     (m/rotate [(/ Math/PI -4) 0 (/ Math/PI 2)])
                     (m/translate [26 0 4]))
                (->> (m/cube length 10 3)
                     (m/color light-color)
                     (m/rotate [(/ Math/PI -4) 0 (/ Math/PI 2)])
                     (m/translate [26 0 8])))
               (m/translate [0 0 140])))
         (m/translate [0 0 -200])))


  ;; Curved shade wiith lights in troughs
  (->> (let [arc-length 0.2
             radius 350
             bars (corrugations (* 2 Math/PI arc-length radius))]
         (->> (m/union
               (->> (curved-corrugated arc-length radius 1060)
                    (m/rotate [(* -0.5 Math/PI) (* (- 0.5 arc-length) Math/PI) 0]))
               (->> (apply m/union
                           (for [i (range bars)
                                 :let [angle (* 2 Math/PI arc-length (+ i 0.65) (/ (+ bars 0.5)))]]
                             (->> (strip 1000)
                                  (m/translate [0 0 (- 0 radius -5)])
                                  (m/rotate [0 angle 0]))))
                    (m/rotate [0
                               (* (- arc-length) Math/PI)
                               0])))
              (m/translate [0 0 radius])))
       (m/translate [0 0 500]))


  #_(->> (m/polygon [[10, 10] [100, 100] [100, 0]])
         (m/extrude-linear {:height 500}))
  #_(->> (bar-light)
         (m/translate [-1500 1500 0]))

  #_(->> (apply m/union
                (map (fn [[rot alt]]
                       (->> (pipe-light)
                            (m/translate [0 520 0])
                            (m/rotate [0 (* alt m/pi 1/15) (* m/pi rot)])))
                     [[0 1] [1/2 -1] [1 1] [3/2 -1]]))
         (m/translate [1500 1500 0]))

  (let [length (inches-to-mm 100)
        width (inches-to-mm 18)
        margin (inches-to-mm 3)
        fixture-length (- length (* 2 margin))
        fixture-width (- width (* 2 margin))
        screw-count 6
        screw-offset-y (- (/ fixture-width 2) 250)
        wood-thickness (inches-to-mm 0.75)
        wood-margin (inches-to-mm 1)
        bracket-height (inches-to-mm 7)]
    (->>
     (m/union
      ;; Wooden base
      (->> (m/cube fixture-length fixture-width wood-thickness)
           (m/translate [0 0 wood-margin])
           (m/color wood-color))
      ;; Corrugated reflector
      (flat-corrugated width length)

      ;; Screws
      (apply m/union
             (for [offset-y [screw-offset-y (- screw-offset-y)]
                   row (range screw-count)
                   :let [offset-x (- (/ fixture-length 2)
                                     (* (inc row) (/ fixture-length (inc screw-count))))]]
               (->> (m/cylinder 10 15)
                    (m/translate [offset-x offset-y 0])
                    (m/color pipe-color))))

      ;; Light bracket
      (->> (m/union
            (->> (rod fixture-length)
                 (m/rotate [0 0 0]))
            (->> (rod bracket-height)
                 (m/rotate [0 (/ Math/PI 2) 0])
                 (m/translate [(/ fixture-length 2) 0
                               (+ (/ bracket-height 2)
                                  (inches-to-mm -0.25))]))
            (->> (rod bracket-height)
                 (m/rotate [0 (/ Math/PI 2) 0])
                 (m/translate [(/ fixture-length -2) 0
                               (+ (/ bracket-height 2)
                                  (inches-to-mm -0.25))]))
            (->> (m/cube fixture-length 20 3)
                 (m/color pipe-color)
                 (m/rotate [(/ Math/PI 8) 0 0])
                 (m/translate [0 -22 4]))
            (->> (m/cube fixture-length 10 3)
                 (m/color light-color)
                 (m/rotate [(/ Math/PI 8) 0 0])
                 (m/translate [0 -22 8]))
            (->> (m/cube fixture-length 20 3)
                 (m/color pipe-color)
                 (m/rotate [(/ Math/PI -8) 0 0])
                 (m/translate [0 22 4]))
            (->> (m/cube fixture-length 10 3)
                 (m/color light-color)
                 (m/rotate [(/ Math/PI -8) 0 0])
                 (m/translate [0 22 8])))
           (m/translate [0 0 (+ (- bracket-height)
                                wood-margin
                                wood-thickness)])))

     (m/translate [0 0 -500])))

  (let [awning-length (inches-to-mm 113)
        awning-width (inches-to-mm 22)
        awning-lip (inches-to-mm 1.5)
        fixture-margin (inches-to-mm 6)
        fixture-length (- awning-length (* 2 fixture-margin))
        fixture-width (- awning-width (* 2 fixture-margin))
        frame-margin (inches-to-mm 3)
        frame-length (- fixture-length (* 2 frame-margin))
        wood-width (- fixture-width (* 2 frame-margin))
        screw-offset-y (/ wood-width 3)
        screw-count 10
        wood-thickness (inches-to-mm 0.75)
        wood-z (inches-to-mm 1)
        awning-z (+ wood-z wood-thickness)
        frame-height (inches-to-mm 6)
        led-gap (inches-to-mm 1)
        rod-thickness (inches-to-mm 0.5)
        led-length (- frame-length (* 2 led-gap) (* 2 rod-thickness))
        frame-rod (fn [frame-height]
                    (m/difference
                     (rod frame-height)
                     (m/union
                      (->> (m/cube (inches-to-mm 1.5) (* 2 rod-thickness) (/ rod-thickness 1.5))
                           (m/translate [(/ frame-height 2) 0 0]))
                      (->> (m/cylinder 2.5 frame-height)
                           (m/translate [(- (/ frame-height 2) 5) 0 0]))
                      (->> (m/cylinder 2.5 frame-height)
                           (m/translate [(- (/ frame-height 2) (inches-to-mm 1)) 3 0])))))]
    (prn {:long-rod (mm-to-inches frame-length)
          :short-rods (mm-to-inches (+ frame-height wood-thickness shower/corrugation-amplitude-mm))
          :shade-length (mm-to-inches fixture-length)
          :shade-width (mm-to-inches fixture-width)}
         :led-length (str (float (/ led-length 1000)) "m"))
    (->>
     (m/union
        ;; Awning ceiling
      (->> (m/cube awning-length awning-width 1)
           (m/translate [0 0 awning-z])
           (m/color pipe-color))
        ;; Awning lip
      (->> (m/cube awning-length awning-lip 1)
           (m/rotate [(/ Math/PI 2) 0 0])
           (m/translate [0 (/ awning-width 2) (- awning-z (/ awning-lip 2))])
           (m/color pipe-color))

      ;; Wooden base
      (->> (m/cube frame-length wood-width wood-thickness)
           (m/translate [0 0 wood-z])
           (m/color wood-color))  ;; Needs to be painted black, because visible

        ;; Corrugated reflector
      (->> (flat-corrugated fixture-length fixture-width)
           (m/rotate [0 0 (/ Math/PI 2)]))

        ;; Screws
      (apply m/union
             (for [offset-y [screw-offset-y (- screw-offset-y)]
                   row (range screw-count)
                   :let [offset-x (- (/ frame-length 2)
                                     (* (inc row) (/ fixture-length (inc screw-count))))]]
               (->> (m/cylinder 10 15)
                    (m/translate [offset-x offset-y 0])
                    (m/color pipe-color))))

        ;; Light frame
      (->> (m/union
            (->> (rod frame-length)
                 (m/rotate [0 0 0]))
            (->> (frame-rod frame-height)
                 (m/rotate [(/ Math/PI 2) (/ Math/PI -2) 0])
                 (m/translate [(/ frame-length 2) 0
                               (+ (/ frame-height 2)
                                  (inches-to-mm -0.25))]))
            (->> (frame-rod frame-height)
                 (m/rotate [(/ Math/PI 2) (/ Math/PI -2)  0])
                 (m/translate [(/ frame-length -2) 0
                               (+ (/ frame-height 2)
                                  (inches-to-mm -0.25))]))
            ;; LED strip
            (->> (m/cube led-length 16 5)
                 (m/color pipe-color)
                 (m/translate [0 0 8]))
            (->> (m/cube led-length 11 5)
                 (m/color light-color)
                 (m/translate [0 0 10])))
           (m/translate [0 0 (+ (- frame-height)
                                wood-z
                                wood-thickness)])))

     (m/translate [0 0 -1000])))

  #_(m/union
     (->> (corrugation 500)
          (m/mirror [1 0 0])
          (m/translate [-200 0 0]))
     (->> (corrugation 500)
          (m/translate [-1500 -1500 0])))))


(comment
  (ns-unmap *ns* 'flat-corrugated)
  )