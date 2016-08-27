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
        timestamp (util/get-timestamp)]
    {:font font
     :graphics (take (count util/colors)
                     (repeatedly #(q/create-graphics (q/width) (q/height) :p2d)))
     :altGraphic (q/create-graphics (q/width) (q/height) :p2d)
     :timestamp timestamp
     :pdf (q/create-graphics (* util/pdf-dpi (first util/pdf-size))
                             (* util/pdf-dpi (second util/pdf-size))
                             :pdf
                             (format "pdf/%s_print.pdf" timestamp))
     :frame 0}))

(defn update-state [state]
  (let [name-group  (util/geoify-name (:font state) util/fullname)
        name-bounds (util/addPad 
                      (util/getBounds (.getPoints name-group)) 
                      (* util/dpi 0.1))
        badge-rect  (geomerative.RPolygon/createRectangle
                      0 0 (q/width) (q/height))]
    (merge state
           {:frame (inc (:frame state))
            :polygons [(.union
                         (.toPolygon name-group)
                         (.xor
                           badge-rect
                           (geomerative.RPolygon/createRectangle
                             (:left name-bounds)
                             (:top name-bounds)
                             (:width name-bounds)
                             (* (/ (:height name-bounds) 3) 2))))
                       (.union
                         (.toPolygon name-group)
                         (.xor
                           badge-rect
                           (geomerative.RPolygon/createRectangle
                             (:left name-bounds)
                             (+ (:top name-bounds)
                                (/ (:height name-bounds) 3))
                             (:width name-bounds)
                             (* (/ (:height name-bounds) 3) 2))))
                       (.union
                         (.toPolygon name-group)
                         (.xor
                           badge-rect
                           (.union
                             (geomerative.RPolygon/createRectangle
                               (:left name-bounds)
                               (:top name-bounds)
                               (:width name-bounds)
                               (/ (:height name-bounds) 3))
                             (geomerative.RPolygon/createRectangle
                               (:left name-bounds)
                               (- (:bottom name-bounds)
                                  (/ (:height name-bounds) 3))
                               (:width name-bounds)
                               (/ (:height name-bounds) 3)))))]})))

(defn draw-state [state]
  (when (not (zero? (:frame state)))
    ;set up canvas
    (q/background 255)
    ;draw to off-screen buffers
    (dorun
      (for [i (range (count (:graphics state)))]
        (util/draw-it (nth (:graphics state) i)
                      (nth (:polygons state) i)
                      (nth util/colors i))))
    ;combine buffers on-screen
    (q/blend-mode :subtract)
    (dorun
      (for [graphic (:graphics state)]
        (q/image graphic 0 0)))
    (when (= (:frame state) 1)
      (util/save state))))
