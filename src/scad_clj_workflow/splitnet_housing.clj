;; https://github.com/PEZ/scad-clj-workflow
(ns scad-clj-workflow.splitnet-housing
  (:require [scad-clj.model :as m]
            [scad-clj-workflow.helpers :refer [render]]))

(binding [m/*fn* 40
          m/*fs* 2
          m/*fa* 12]
  (let [small-board-radius           15
        small-radius                 (+ small-board-radius 2)
        large-board-diameter         47
        large-board-radius           (/ large-board-diameter 2.0)
        large-radius                 (+ large-board-radius 2)
        round-screen-diameter        39
        block-height                 100 ;; Height of the block used to cut away half of things
        back-thickness               1.5
        screen-height                (-> round-screen-diameter
                                         (/ 2.0)
                                         (/ large-radius)
                                         (Math/acos)
                                         (Math/sin)
                                         (* large-radius)) ;; Should be able to calculate
        screen-cutout                (->> (m/cylinder (/ round-screen-diameter 2.0) block-height)
                                          (m/translate [0 0 (+ back-thickness (/ block-height 2.0))]))
        board-cutout-height          6
        main-board-cutout            (->> (m/cylinder large-board-radius board-cutout-height)
                                          (m/translate [0 0 (+ back-thickness (/ board-cutout-height 2.0))]))
        cable-board-cutout           (->> (m/union
                                           (m/cylinder small-board-radius board-cutout-height)
                                           (->> (m/sphere (- small-radius 3))))
                                           (m/translate [0 0 -3])
                                          (m/translate [large-board-radius 0 (+ back-thickness (/ board-cutout-height 2.0))]))
        antenna-board-cutout         (->> (m/cylinder small-board-radius board-cutout-height)
                                          (m/translate [(- large-board-radius) 0 (+ back-thickness (/ board-cutout-height 2.0))]))
        main-dome                    (m/difference
                                      (m/sphere large-radius)
                                      (m/scale [1 1 1.00])
                                      (->> (m/cube 1000 1000 block-height)
                                           (m/translate [0 0 (+ screen-height -2 (/ block-height 2.0))])))
        cable-dome                   (->> (m/sphere small-radius)
                                          (m/scale [1 1 1.00])
                                          (m/translate [large-board-radius 0 0]))
        antenna-dome                 (->> (m/sphere small-radius)
                                          (m/scale [1 1 1.00])
                                          (m/translate [(- large-board-radius) 0 0]))
        antenna-connector-diameter   6.35
        antenna-connector-radius    (/ antenna-connector-diameter 2)
        antenna-nut-diameter         10.16 ;; washer 0.4", nut 9.144mm
        antenna-mount-inner-diameter (+ antenna-nut-diameter 1) ;; 0.5mm space around washer
        antenna-mount-inner-radius   (/ antenna-mount-inner-diameter 2)
        antenna-mount-diameter       (+ antenna-mount-inner-diameter 2)
        antenna-mount-radius         (/ antenna-mount-diameter 2)
        antenna-mount-len            20
        antenna-mount                (->> (m/cylinder antenna-mount-radius antenna-mount-len)
                                          (m/rotate [0 (/ Math/PI 2) 0])
                                          (m/translate [(- 0 large-board-radius small-radius (- (/ antenna-mount-len 2)) 0) 0 9]))
        antenna-mount-cutout         (->> (m/union
                                           (m/cylinder antenna-mount-inner-radius (+ antenna-mount-len 10))
                                           (m/cylinder antenna-connector-radius (+ antenna-mount-len 20)))
                                          (m/rotate [0 (/ Math/PI 2) 0])
                                          (m/translate [(- 0 large-board-radius small-radius (- (/ antenna-mount-len 2)) -6) 0 9]))
        threaded-inserts             (apply m/union
                                            (for [x [23.5 -23.5]
                                                  y [11.5 -11.5]]
                                              (->> (m/cylinder 1.5 5)
                                                   (m/rotate [0 0 (/ Math/PI 2)])
                                                   (m/translate [x y 8]))))
        screw-holes                  (apply m/union
                                            (for [x [23.5 -23.5]
                                                  y [11.5 -11.5]]
                                              (->> (m/union (m/cylinder 1 5)
                                                            (->> (m/cylinder 1.5 2) ;; Screw head inset
                                                                 (m/translate [0 0 1.5])))
                                                   (m/rotate [0 0 (/ Math/PI 2)])
                                                   (m/translate [x y 0]))))
        model                        (m/difference
                                      (m/union
                                       main-dome
                                       cable-dome
                                       antenna-dome
                                       antenna-mount)
                                      threaded-inserts
                                      screw-holes
                                      screen-cutout
                                      main-board-cutout
                                      cable-board-cutout
                                      antenna-board-cutout
                                      antenna-mount-cutout)
        model-top                    (m/intersection ;; Choose model-top
                                      model
                                      (->> (m/cube 1000 1000 block-height)
                                           (m/translate [0 0 (+ back-thickness
                                                                0.1 (/ block-height 2))])))
        model-bottom                 (m/intersection ;; Choose model-bottom
                                      model
                                      (->> (m/cube 1000 1000 block-height)
                                           (m/translate [0 0 (/ block-height 2)]))
                                      (->> (m/cube 1000 1000 block-height)
                                           (m/translate [0 0 (- back-thickness (/ block-height 2))])))]
    (-> (m/union
         model-top
         (->> model-bottom
              (m/rotate [Math/PI 0 0])
              (m/translate [0 100 0])))
        (render))))

(comment
)