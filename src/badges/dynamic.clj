(ns badges.dynamic
  (:import (geomerative RFont)
           (geomerative RCommand)
           (geomerative RG))
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [badges.util :as util]))

(defn setup []
  (RG/init (quil.applet/current-applet))
  (RCommand/setSegmentLength 1)
  (RCommand/setSegmentator RCommand/UNIFORMLENGTH)
  (let [timestamp (format "%05d_%02d_%02d_%02d_%02d_%02d_%03d"
                            (q/year) (q/month) (q/day) 
                            (q/hour) (q/minute) (q/seconds)
                            (mod (System/currentTimeMillis) 1000))
        font (RFont. "Colfax-WebMedium.ttf" 100)
        group (.toGroup font "Quin Kennedy")
        bounds (util/getBounds (.getPoints group))]
    (.scale group 
            (min 1 
                 (/ (- (q/width) 
                       (* util/dpi (* 0.3 2))) 
                    (:width bounds))))
    (let [bounds2 (util/getBounds (.getPoints group))]
      (.translate group 
                  (- (* util/dpi 0.3) (:left bounds2)) 
                  (- (/ (- (q/height) 
                           (:height bounds2)) 
                        3) 
                     (:top bounds2)))
      (let [bounds3 (util/addPad (util/getBounds (.getPoints group)) (* util/dpi 0.1))
            badgeRect (geomerative.RPolygon/createRectangle
                        0 0 (q/width) (q/height))
            topPoly (.union
                      (.toPolygon group)
                      (.xor
                        badgeRect
                        (geomerative.RPolygon/createRectangle 
                          (:left bounds3) (:top bounds3) 
                          (:width bounds3) (/ (:height bounds3) 2))))
            bottomPoly (.union
                         (.toPolygon group)
                         (.xor 
                           badgeRect
                           (geomerative.RPolygon/createRectangle 
                             (:left bounds3) (+ (:top bounds3) (/ (:height bounds3) 2))
                             (:width bounds3) (/ (:height bounds3) 2))))]
        ; Set frame rate to 30 frames per second.
        (q/frame-rate 30)
        ; Set color mode to HSB (HSV) instead of default RGB.
        (q/color-mode :rgb)
        (RG/saveShape (format "output/%s_layer1.svg" timestamp) 
                      (.toShape bottomPoly))
        (RG/saveShape (format "output/%s_layer2.svg" timestamp) 
                      (.toShape topPoly))
        ; setup function returns initial state. It contains
        ; circle color and position.
        {:font font
         :name group
         :timestamp timestamp
         :top  (util/draw-it topPoly
                        [0 64 255])
         :bottom (util/draw-it bottomPoly
                          [64 255 0])
         :frame 0}
))))

(defn update-state [state]
  (update state :frame inc))

(defn draw-state [state]
  (when (= (:frame state) 1)
    (let [box (util/getBounds (.getPoints (:name state)))
          ]
      (q/background 255)
      (q/blend-mode :subtract)
      (q/image (:top state) 0 0)
      (q/image (:bottom state) 0 0)
      (q/save (format "output/%s_render" (:timestamp state)))
      )))
