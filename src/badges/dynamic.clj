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
  (let [font (RFont. "Colfax-WebMedium.ttf" util/font-size)
        timestamp (util/get-timestamp)
        ahd-font (RFont. "arthackday.ttf" (/ (q/width) 10))]
    {:font font
     :ahd-font ahd-font
     :ahd-font-info (util/measure-font ahd-font)
     :graphics (take (count util/colors)
                     (repeatedly #(q/create-graphics (q/width) (q/height) :p2d)))
     :altGraphic (q/create-graphics (q/width) (q/height) :p2d)
     :timestamp timestamp
     :pdf (q/create-graphics (* util/pdf-dpi (first util/pdf-size))
                             (* util/pdf-dpi (second util/pdf-size))
                             :pdf
                             (format "pdf/%s_print.pdf" timestamp))
     ; alternatively (q/frame-count)
     :frame 0
     :points (util/get-rand-radial-points)}))

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

(defn get-circle-pattern []
  (let [num-wide  15
        step-size (/ (q/width) num-wide)
        y-step    (Math/sqrt 
                    (-
                      (* step-size step-size)
                      (Math/pow 
                        (/ step-size 2) 
                        2)))]
    (loop [badge (geomerative.RPolygon.)
           x     0
           y     0
           even  false]
      (if (> y (+ (q/height) step-size))
        badge
        (let [next-badge (.union badge
                                 (geomerative.RPolygon/createCircle
                                   (+ x 
                                      (if even 
                                        (/ step-size 2)
                                        0))
                                   y
                                   (/ (* 2 step-size) 5)))]
          (if (> x (q/width))
            (recur next-badge 0 (+ y y-step) (not even))
            (recur next-badge (+ x step-size) y even)))))))

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
  (let [name-polys  (util/geoify-name (:font state) util/fullname)
        badge-rect  (geomerative.RPolygon/createRectangle
                      0 0 (q/width) (q/height))
        cubes (util/union-all (util/gen-cubes (:ahd-font state) (:ahd-font-info state)))
        inv-cubes (.xor badge-rect cubes)
        split-badge (util/split-times badge-rect 7)
        event-poly  (.xor badge-rect (util/get-event-poly (:font state)))
        event-poly-ud (geomerative.RPolygon. event-poly)
        extra-points (concat (:points state) (util/rotate-points (:points state)))
        voronoi (util/voronoi extra-points)
        regions (.getRegions voronoi)
        polygons (map util/region-to-polygon regions extra-points)
        text (util/build-font-cover 
               (:ahd-font state) 
               (:ahd-font-info state)
               (map #(util/getBounds (.getPoints %))
                    name-polys))
        ahd-font (RFont. "arthackday.ttf" (/ (q/width) 8))
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
    ; turn the other event poly upsidedown
    (.rotate event-poly-ud Math/PI (/ (q/width) 2) (/ (q/height) 2))
    (let [layer2 (util/union-all (conj (second text) (.intersection cubes (util/union-all name-polys))))]
                ; (.intersection
                ;   badge-rect
                ;   (.intersection
                ;     (.union
                ;       name-poly
                ;       (util/things-to-geom
                ;         (take-nth 2 (rest grown-polygons))))
                ;     event-poly-ud))]
      (if (:upside-down state)
        (.rotate layer2 Math/PI (/ (q/width) 2) (/ (q/height) 2)))
      (if (:flip-v state)
        (.scale layer2 1 -1 (/ (q/width) 2) (/ (q/height) 2)))
      (if (:flip-h state)
        (.scale layer2 -1 1 (/ (q/width) 2) (/ (q/height) 2)))
      (merge state
             {:frame (inc (:frame state))
              :ahd-font ahd-font
              :ahd-font-info (util/measure-font ahd-font)
              :polygons [(util/union-all (conj (first text) (.intersection inv-cubes (util/union-all name-polys))))
                         layer2

                         ;(util/union-all
                         ;  (conj (vec (take-nth 2 split-badge)) name-poly))
                         ;(util/union-all
                         ;  (conj (vec (take-nth 2 (rest split-badge))) name-poly))

                         ;(.intersection
                         ;  badge-rect
                         ;  (.intersection
                         ;    (.union
                         ;      name-poly
                         ;      (util/things-to-geom
                         ;        (take-nth 2 grown-polygons)))
                         ;    event-poly))
                         ;layer2
                         ]
              }))))

(defn key-typed [state event]
  (case (:key event)
    :s (merge state
              {:frame 0})
    :r (merge state
              {:points (util/get-rand-radial-points)})
    :u (merge state
              {:upside-down (not (:upside-down state))})
    :v (merge state
              {:flip-v (not (:flip-v state))})
    :h (merge state
              {:flip-h (not (:flip-h state))})
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
            (util/get-color i))))
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
        (for [graphic (:graphics state)]
          (q/image graphic 0 0)))
      (when (= (:frame state) 1)
        (util/save state)))))
