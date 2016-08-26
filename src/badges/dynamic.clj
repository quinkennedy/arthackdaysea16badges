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
  (let [font (RFont. "Colfax-WebMedium.ttf" util/font-size)
        group (.toGroup font (first util/fullname))
        group2 (.toGroup font (second util/fullname))
        timestamp (util/get-timestamp)]
    (.translate group2 0 util/font-size)
    (.addGroup group group2)
    (let [bounds (util/getBounds (.getPoints group))]
      (.scale group 
              (min 1 
                   (/ (- (q/width) 
                         (* util/dpi (* 0.3 2))) 
                      (:width bounds))))
      (let [bounds2 (util/getBounds (.getPoints group))]
        (.translate group 
                    (- (* util/dpi 0.3) (:left bounds2)) 
                    (- (- (/ (q/height) 2)
                          (:bottom bounds2))
                       (* util/dpi 0.3)))
        (let [bounds3    (util/addPad 
                           (util/getBounds (.getPoints group)) 
                           (* util/dpi 0.1))
              badgeRect  (geomerative.RPolygon/createRectangle
                           0 0 (q/width) (q/height))
              topPoly    (.union
                           (.toPolygon group)
                           (.xor
                             badgeRect
                             (.union 
                               (geomerative.RPolygon/createRectangle 
                                 (:left bounds3) (:top bounds3) 
                                 (:width bounds3) (/ (:height bounds3) 4))
                               (geomerative.RPolygon/createRectangle
                                 (:left bounds3)
                                 (- (:bottom bounds3)
                                    (/ (:height bounds3) 4))
                                 (:width bounds3)
                                 (/ (:height bounds3) 4)))))
              bottomPoly (.union
                           (.toPolygon group)
                           (.xor 
                             badgeRect
                             (geomerative.RPolygon/createRectangle 
                               (:left bounds3) 
                               (+ (:top bounds3) (/ (:height bounds3) 4))
                               (:width bounds3) 
                               (/ (:height bounds3) 2))))
              polygons (util/add-e topPoly bottomPoly)]
          ; Set frame rate to 30 frames per second.
          (q/frame-rate 30)
          ; Set color mode to HSB (HSV) instead of default RGB.
          (q/color-mode :hsb)
          ; setup function returns initial state. It contains
          ; circle color and position.
          {:font font
           :ee "hi"
           :name group
           :topPoly (util/clean-up (first polygons))
           :bottomPoly (util/clean-up (second polygons))
           :topGraphic  (q/create-graphics (q/width) (q/height) :p2d)
           :bottomGraphic  (q/create-graphics (q/width) (q/height) :p2d)
           :altGraphic (q/create-graphics (q/width) (q/height) :p2d)
           :timestamp timestamp
           :pdf (q/create-graphics (* util/pdf-dpi (first util/pdf-size))
                                   (* util/pdf-dpi (second util/pdf-size))
                                   :pdf
                                   (format "pdf/%s_print.pdf" timestamp))
           :frame 0}
)))))

(defn update-state [state]
  (update
    (if (nil? (:ee state))
      (update 
        (update 
          state 
          :bottomPoly 
          util/add-e)
        :topPoly 
        util/add-e)
      state)
    :frame
    inc))

(defn draw-state [state]
  ;set up canvas
  (q/background 255)
  (q/blend-mode :subtract)
  ;draw to off-screen buffers
  (util/draw-it (:topGraphic state)
                (:topPoly state)
                [10 255 155])
  (util/draw-it (:bottomGraphic state)
                (:bottomPoly state)
                [200 255 55])
  ;combine buffers on-screen
  (q/image (:topGraphic state) 0 0)
  (q/image (:bottomGraphic state) 0 0)
  (when (= (:frame state) 1)
    (util/save state)))
