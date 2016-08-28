(ns badges.dynamic
  (:import (geomerative RFont)
           (geomerative RCommand)
           (geomerative RG))
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [badges.util :as util]))

(defn setup []
  (q/frame-rate 1)
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
     ; alternatively (q/frame-count)
     :frame 0}))

(defn get-block-polys [group]
  (let [raw-bounds  (util/getBounds (.getPoints group))
        border      (* util/dpi 0.1)
        name-bounds (util/addPad 
                      raw-bounds
                      border)
        badge-rect  (geomerative.RPolygon/createRectangle
                      0 0 (q/width) (q/height))
        short-height (/ (:height name-bounds) 4)
        top1        (:top name-bounds)
        top2        (+ (:top raw-bounds) (/ (:height raw-bounds) 4))
        top3        (- (:bottom name-bounds) short-height)
        half-width (/ (:width name-bounds) 2)
        third-width (/ (:width name-bounds) 3)
        quarter-width (/ (:width name-bounds) 4)
        tall-height (/ (:height raw-bounds) 2)
        rect-nums [[(:left name-bounds) 
                    top1
                    quarter-width 
                    short-height]
                   [(+ (:left name-bounds) third-width) 
                    top1
                    (* third-width 2) 
                    short-height]
                   [(:left name-bounds) 
                    top2
                    (* third-width 2) 
                    tall-height]
                   [(+ (:left name-bounds) (* quarter-width 3))
                    top2
                    quarter-width
                    tall-height]
                   [(:left name-bounds) 
                    top3
                    quarter-width 
                    short-height]
                   [(+ (:left name-bounds) third-width)
                    top3
                    (* third-width 2)
                    short-height]
                   [(:left name-bounds) 
                    top1
                    (:width name-bounds) 
                    short-height]
                   [(:left name-bounds) 
                    top2
                    (:width name-bounds) 
                    tall-height]
                   [(:left name-bounds) 
                    top3
                    (:width name-bounds) 
                    short-height]]]
    [(.union
       (.toPolygon group)
       (.xor
         badge-rect
         (util/union-all
           (mapv 
             #(apply 
                util/createPolyRect
                %)
             [(nth rect-nums 0)
              (nth rect-nums 4)
              (nth rect-nums 7)]))))
     (.union
       (.toPolygon group)
       (.xor
         badge-rect
         (util/union-all
           (mapv
             #(apply
                util/createPolyRect
                %)
             [(nth rect-nums 3)
              (nth rect-nums 6)
              (nth rect-nums 8)]))))
     (.union
       (.toPolygon group)
       (.xor
         badge-rect
         (util/union-all
           (mapv
             #(apply
                util/createPolyRect
                %)
             [(nth rect-nums 1)
              (nth rect-nums 2)
              (nth rect-nums 5)]))))]))

(defn translate-polygon [polygon x y]
  (let [clone (.toPolygon polygon)]
    (.translate clone x y)
    clone))

(defn get-offset-polys [group font num-colors]
  (let [line-width (:width (util/getBounds (.getPoints (.toGroup font "i"))))
        poly (.toPolygon group)
        polys (take num-colors (repeatedly #(.toPolygon group)))
        badge-rect  (geomerative.RPolygon/createRectangle
                      0 0 (q/width) (q/height))
        ]
    (map #(.xor
            badge-rect
            %)
         (doall
           (loop [accumulatedPolys (take num-colors 
                                         (repeatedly #(.toPolygon group)))
                  j                0]
             (if (= j 10)
               accumulatedPolys
               (let [offsets (map #(util/polar-to-cartesian
                                     (* line-width j)
                                     (/ (* Math/PI 2 %) num-colors))
                                  (range num-colors))]
                 (recur (map #(.union %1 (translate-polygon %2 (first %3) (second %3)))
                             accumulatedPolys
                             (repeat poly)
                             offsets)
                        (inc j)))))))))

(defn update-state [state]
  (let [name-group  (util/geoify-name (:font state) util/fullname)]
    (merge state
           {:frame (inc (:frame state))
            :polygons ;;(util/add-e
                      ;  (get-block-polys name-group);)
                      (get-offset-polys name-group (:font state) (count util/colors))
            })))

(defn key-typed [state event]
  (update state :frame #(if (= :s (:key event)) 0 %)))

(defn draw-state [state]
  (when (not (zero? (:frame state)))
    ;set up canvas
    (q/background 255)
    ;draw to off-screen buffers
    (dorun
      (for [i (range (count (:graphics state)))]
        (util/draw-it (nth (:graphics state) i)
                      (nth (:polygons state) i)
                      (util/get-color i))))
    ;combine buffers on-screen
    (q/blend-mode :subtract)
    (dorun
      (for [graphic (:graphics state)]
        (q/image graphic 0 0)))
    (when (= (:frame state) 1)
      (util/save state))))
