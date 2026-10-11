;; https://github.com/PEZ/scad-clj-workflow
(ns scad-clj-workflow.splitnet-housing
  (:require [scad-clj.model :as m]
            [scad-clj-workflow.helpers :refer [render]]
            [scad-clj-workflow.extra.clip :refer [clip]]))

;; The circuit board is made of 3 intersecting circles, one large in the middle with
;; two smaller ones on each side.
(def small-board-radius    15)
(def large-board-diameter  48) ;; 47.12 plus a bit (was originally 47)
(def large-board-radius    (/ large-board-diameter 2.0))

(def screen-height         26.67) ;; From screen surface to bottom of space
(def board-height          10)    ;; From circuit board to bottom of space
(def board-thickness       2)

(def round-screen-diameter 44) ;; 43.05 plus a bit (was originally 39)
(def screen-radius         (/ round-screen-diameter 2.0))

(def board
  (m/color [0.75 0 0]
           (->> (m/union
                 (m/cylinder large-board-radius board-thickness)
                 (->> (m/cylinder small-board-radius board-thickness)
                      (m/translate [large-board-radius 0 0]))
                 (->> (m/cylinder small-board-radius board-thickness)
                      (clip [-3 0 0] [-1 0 0])
                      (m/translate [(- large-board-radius) 0 0])))
                (m/translate [0 0 board-height]))))
(def screen
  (m/color [0 0 0]
           (->> (m/cylinder screen-radius board-thickness)
                (m/translate [0 0 (- screen-height (/ board-thickness 2))]))))

;; Enclosure
(def back-thickness     1.5)
(def back-rim-thickness 1.5)
(def wall-thickness     1)
(def wall-gap           0.5)

(def small-dome-radius            (+ small-board-radius wall-gap))
(def dome-radius
  "Dome radius such that it is sufficiently large at screen height and board height
   to contain the screen and the board, respectively."
  (let [screen-angle       (Math/atan2 screen-height screen-radius)
        board-angle        (Math/atan2 board-height large-board-radius)
        dome-radius-screen (/ screen-radius (Math/cos screen-angle))
        dome-radius-board  (/ large-board-radius (Math/cos board-angle))]
    (max dome-radius-screen dome-radius-board)))
(def main-dome-cutout
  (->> (m/sphere dome-radius)))

(def board-mount-coordinates
  (for [x [23.5 -23.5]
        y [11.5 -11.5]]
    [x y]))
(def back-mount-coordinates
  (let [d (- dome-radius 2)]
    [[0 d]
     [0 (- d)]]))
(def antenna-connector-diameter   6.35)
(def antenna-nut-diameter         10.16) ;; washer 0.4", nut 9.144mm
(def antenna-mount-inner-diameter (+ antenna-nut-diameter 1)) ;; 0.5mm space around washer
(def antenna-mount-inner-radius   (/ antenna-mount-inner-diameter 2))
(def antenna-mount-diameter       (+ antenna-mount-inner-diameter wall-thickness))
(def antenna-mount-radius         (/ antenna-mount-diameter 2))
(def antenna-mount-len            40)
(def antenna-connector-radius     (/ antenna-connector-diameter 2))
(def antenna-mount-height         0)
(def antenna-mount-cutout
  (->> (m/union
        (m/cylinder antenna-mount-inner-radius (+ antenna-mount-len 10))
        (m/cylinder antenna-connector-radius (+ antenna-mount-len 20)))
       (m/rotate [0 (/ Math/PI 2) 0])
       (m/translate [(- 0 small-dome-radius (- (/ antenna-mount-len 2)) -6) 0 antenna-mount-height])))
(def antenna-mount-solid
  (->> (m/cylinder antenna-mount-radius antenna-mount-len)
       (m/rotate [0 (/ Math/PI 2) 0])
       (m/translate [(- 0 small-dome-radius (- (/ antenna-mount-len 2)) wall-thickness) 0 antenna-mount-height])))
(def antenna-mount
  (m/difference antenna-mount-solid
                antenna-mount-cutout
                (->> (m/sphere small-dome-radius))))
(def antenna-dome-cutout
  (->> (m/sphere small-dome-radius)
       (m/translate [(- large-board-radius) 0 board-height])))
(def antenna-dome
  (->> (m/difference
        (->> (m/union
              (m/difference
               (m/sphere (+ small-dome-radius wall-thickness))
               (m/sphere small-dome-radius)
               antenna-mount-cutout)
              antenna-mount)
             (m/translate [(- large-board-radius) 0 board-height])
             (clip [0 0 0] [0 0 -1]))
        antenna-dome-cutout

        main-dome-cutout)))

(def cable-dome-cutout
  (->> (m/sphere small-dome-radius)
       (m/translate [large-board-radius 0 board-height])))
(def cable-dome
  (m/difference
   (->> (m/difference
         (m/sphere (+ small-dome-radius wall-thickness))
         (m/sphere small-dome-radius))
        (m/translate [large-board-radius 0 board-height])
        (clip [0 0 0] [0 0 -1]))
   main-dome-cutout))

(def screen-cutout (m/cylinder (+ screen-radius wall-gap) largest-dimension))
(def main-dome (->> (m/difference
                     (m/sphere (+ dome-radius wall-thickness))
                     (m/sphere dome-radius)
                     ;; Cut the screen cylinder out
                     screen-cutout
                     antenna-dome-cutout
                     cable-dome-cutout)
                    ;; Cut the top off the dome
                    (clip [0 0 screen-height])
                    (clip [0 0 0] [0 0 -1])))

(def top-part-volume
  (m/union
   (m/sphere (+ dome-radius wall-thickness))
   (->> (m/sphere (+ small-dome-radius wall-thickness))
        (m/translate [large-board-radius 0 board-height]))
   (->> (m/sphere (+ small-dome-radius wall-thickness))
        (m/translate [(- large-board-radius) 0 board-height]))))

(def board-mounts
  (m/intersection
   (apply m/union
          (map (fn [[x y]]
                 (->> (m/difference
                       (m/cylinder 2.5 100 :center false)
                       (->> (m/cylinder 0.75 5 :center false)
                            (m/translate [0 0 -0.1])))
                      (m/rotate [0 0 (/ Math/PI 2)])
                      (m/translate [x y (+ board-height board-thickness)])))
               board-mount-coordinates))
   top-part-volume))

(def back-mounts
  (m/intersection
   (apply m/union
          (map (fn [[x y]]
                 (->> (m/difference
                       (m/cylinder 2.5 100 :center false)
                       (->> (m/cylinder 0.75 5 :center false)
                            (m/translate [0 0 -0.1])))
                      (m/rotate [0 0 (/ Math/PI 2)])
                      (m/translate [x y 0])))
               back-mount-coordinates))
   top-part-volume))

(def top-part
  (m/difference
   (m/union
    main-dome
    cable-dome
    antenna-dome
    board-mounts
    back-mounts)))

(def bottom-part
  (let [rim (->> (m/difference
                  top-part
                  back-mounts)
                 (clip [0 0 0.1])
                 (m/projection true)
                 (m/extrude-linear {:height back-rim-thickness :center false}))
        plate (->> rim
                   (m/hull)
                   ;;(m/projection true)
                   ;;(m/extrude-linear {:height back-thickness :center false})
                   )]
    (m/union plate
             (->> (m/intersection
                   (m/difference
                    ;; Rim with inner offset
                    (->> rim
                         (m/projection true)
                         (m/offset {:r 2 :chamfer true})
                         (m/extrude-linear {:height back-rim-thickness :center false}))
                    (->> rim
                         (m/scale [1 1 2]))
                    (m/hull back-mounts))
                   ;; Need to trim off outer offset material usingi intersection
                   ;; because m/offset wth negative parameter does not work.
                   ;; Or I don't understand how it works
                   (->> plate
                        (m/scale [0.98 0.98 1])))
                  (m/translate [0 0 back-thickness])))))

(->> (m/union
      (m/fn! 50)
      (m/fa! 1)
      (m/fs! 1)
      ;;board
      ;;screen
      (->> top-part
           (m/rotate [Math/PI 0 0])
           (m/translate [0 0 screen-height]))
      (->> bottom-part
           (m/translate [0 75 0])))
     (render))
