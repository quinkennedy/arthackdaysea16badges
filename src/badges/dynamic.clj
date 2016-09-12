(ns badges.dynamic
  (:import (geomerative RFont)
           (geomerative RCommand)
           (geomerative RG))
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [thi.ng.geom.polygon :as gp]
            [badges.util :as util]))

(defn setup []
  (q/frame-rate 15)
  (RG/init (quil.applet/current-applet))
  (RCommand/setSegmentLength 1)
  (RCommand/setSegmentator RCommand/UNIFORMLENGTH)
  (let [font      (RFont. "Colfax-WebMedium.ttf" util/font-size)
        timestamp (util/get-timestamp)
        patterns  (take 2 (repeatedly #(q/create-graphics 
                                         (q/width) 
                                         (q/height) 
                                         :p2d)))]
    (util/apply-dots (first patterns))
    (util/apply-lines (second patterns))
    {:font       font
     :graphics   (take (count util/colors)
                     (repeatedly 
                       #(q/create-graphics (q/width) (q/height) :p2d)))
     :patterns   patterns
     :altGraphic (q/create-graphics (q/width) (q/height) :p2d)
     :timestamp  timestamp
     :pdf        (q/create-graphics 
                   (* util/pdf-dpi (first util/pdf-size))
                   (* util/pdf-dpi (second util/pdf-size))
                   :pdf
                   (format "pdf/%s_print.pdf" timestamp))
     ; alternatively (q/frame-count)
     :frame      0
     :points     (util/get-rand-radial-points)}))

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
  (let [name-group  (util/geoify-name (:font state) util/fullname)
        name-poly   (.toPolygon name-group)
        badge-rect  (geomerative.RPolygon/createRectangle
                      0 0 (q/width) (q/height))
        event-poly  (.xor badge-rect (util/get-event-poly (:font state)))
        event-poly-ud (geomerative.RPolygon. event-poly)
        extra-points (concat (:points state) (util/rotate-points (:points state)))
        voronoi (util/voronoi extra-points)
        regions (.getRegions voronoi)
        polygons (map util/region-to-polygon regions extra-points)
        ; modulate polygons based on location
        grown-polygons (map
                         (fn [polygon center]
                           (gp/polygon2 
                             (thi.ng.geom.polygon/inset-polygon 
                               (:points polygon) 
                               (* (- (/ (second center) 
                                        (q/height)) 
                                     0.5) 
                                  20))))
                         polygons
                         extra-points)
        patterns  (take 2 (repeatedly #(q/create-graphics 
                                         (q/width) 
                                         (q/height) 
                                         :p2d)))]
    (util/apply-dots (first patterns))
    (util/apply-lines (second patterns))
    ; turn the other event poly upsidedown
    (.rotate event-poly-ud Math/PI (/ (q/width) 2) (/ (q/height) 2))
    (let [layer2 (.intersection
                   badge-rect
                   (.intersection
                     (.union
                       name-poly
                       (util/things-to-geom
                         (take-nth 2 (rest grown-polygons))))
                     event-poly-ud))]
      (if (:upside-down state)
        (.rotate layer2 Math/PI (/ (q/width) 2) (/ (q/height) 2)))
      (merge state
             {:frame (inc (:frame state))
              :polygons [
                         (.intersection
                           badge-rect
                           (.intersection
                             (.union
                               name-poly
                               (util/things-to-geom
                                 (take-nth 2 grown-polygons)))
                             event-poly))
                         layer2
                         (geomerative.RPolygon.)
                         ]
              :patterns patterns
              }))))

(defn key-typed [state event]
  (case (:key event)
    :s (merge state
              {:frame 0})
    :r (merge state
              {:points (util/get-rand-radial-points)})
    :u (merge state
              {:upside-down (not (:upside-down state))})
    state))

(defn draw-state [state]
  (when (not (zero? (:frame state)))
    ;set up canvas
    (q/background 255)
    ;get voronoi polygons
    (let [extra-points (concat (:points state) (util/rotate-points (:points state)))
          voronoi (util/voronoi extra-points)
          regions (.getRegions voronoi)
          polygons (map util/region-to-polygon regions extra-points)
          ; modulate polygons based on location
          grown-polygons (map
                           (fn [polygon center]
                             (gp/polygon2 
                               (thi.ng.geom.polygon/inset-polygon 
                                 (:points polygon) 
                                 (* (- (/ (second center) 
                                          (q/height)) 
                                       0.5) 
                                    20))))
                           polygons
                           extra-points)]
      ;draw to off-screen buffers
      (dorun
        (for [i (range (count (:graphics state)))]
          (util/draw-it
            (nth (:graphics state) i)
            (nth (:polygons state) i)
            [255 255 255])))
            ;(util/get-color i))))
      ;(util/draw-polygons
      ;  (first (:graphics state))
      ;  (util/get-color 0)
      ;  (take-nth 2 grown-polygons))
      ;(util/draw-polygons
      ;  (second (:graphics state))
      ;  (util/get-color 1)
      ;  (take-nth 2 (rest grown-polygons)))
      ;combine buffers on-screen
      (q/blend-mode :subtract)
      (dorun
        (for [i (range (count (:patterns state)))]
          ;(do
          ;  (q/with-graphics
          ;    (:altGraphic state)
          ;    (q/clear)
          ;    (q/blend-mode :blend)
          ;    (q/image (nth (:patterns state) 1) 0 0)
          ;    (q/blend-mode :subtract)
          ;    (q/image (nth (:graphics state) 1) 0 0))
          ;  ;(q/image (nth (:graphics state) i) 0 0))))
          ;  (q/blend-mode :blend)
          ;  (q/image (:altGraphic state) 0 0))))
          ;(q/image (nth (:graphics state) i) 0 0)))
          ;(q/image (nth (:patterns state) i) 0 0)))
          (let [img (.copy (nth (:patterns state) i))]
          (do
            (q/mask-image
              img
              ;(.copy (nth (:patterns state) i))
              (.copy (nth (:graphics state) i)))
            (q/image img 0 0)))))
      (when (= (:frame state) 1)
        (util/save state)))))
