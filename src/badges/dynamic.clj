(ns badges.dynamic
  (:import (geomerative RFont)
           (geomerative RCommand)
           (geomerative RG))
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [thi.ng.geom.polygon :as gp]
            [badges.util :as util]))

(defn setup []
  (q/frame-rate 1)
  (RG/init (quil.applet/current-applet))
  (RCommand/setSegmentLength 1)
  (RCommand/setSegmentator RCommand/UNIFORMLENGTH)
  (let [font      (RFont. "Colfax-WebMedium.ttf" util/font-size)
        ahd-geo-font  (RFont. "arthackday.ttf" util/font-size)
        ahd-font  (q/create-font "arthackday.ttf" util/font-size)
        timestamp (util/get-timestamp)
        badge-rect  (geomerative.RPolygon/createRectangle
                      0 0 (first util/print-px) (second util/print-px))
        patterns  (take 2 (repeatedly #(apply
                                         q/create-graphics 
                                         (conj util/print-px :p2d))))]
    (util/apply-dots (first patterns) false)
    ;(util/apply-dots (second patterns) true)
    (util/apply-lines (second patterns))
    {:font       font
     :ahd-geo-font ahd-geo-font
     :ahd-font   ahd-font
     :graphics   (take (count util/colors)
                       (repeatedly 
                         #(apply q/create-graphics (conj util/print-px :p2d))))
     :patterns   patterns
     :altGraphic (apply q/create-graphics (conj util/print-px :p2d))
     :timestamp  timestamp
     :pdfs       (map #(apply q/create-graphics 
                         (conj (mapv * 
                                     util/pdf-size 
                                     (repeat util/pdf-dpi))
                               :pdf
                               (format "pdf/%s_print_%d.pdf"
                                       timestamp
                                       %)))
                      (range (count util/colors)))
     ; alternatively (q/frame-count)
     :frame      0
     :badge-rect  badge-rect
     :names       (map (fn [line] (take 2 (clojure.string/split line #",")))
                       (clojure.string/split-lines 
                         (slurp "/media/data/docs/me/art-hack-day/badges/data/participants.csv")))
     }))

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
  (let [];patterns  (take 2 (repeatedly #(apply
        ;                                 q/create-graphics 
        ;                                 (conj util/print-px :p2d))))]
    ;(util/apply-dots (first patterns) false)
    ;;(util/apply-dots (second patterns) true)
    ;(util/apply-lines (second patterns))
    (merge state
           {:frame (inc (:frame state))
            :names (:names state)
    ;        :patterns patterns
                         })))

(defn key-typed [state event]
  (case (:key event)
    :s (merge state
              {:frame 0})
    :r (merge state
              {:points (util/get-rand-radial-points)
               :event-poly  ;(.xor (:badge-rect state)
                                  (util/get-event-poly 
                                    (:ahd-geo-font state))})
    :u (merge state
              {:upside-down (not (:upside-down state))})
    state))

(defn draw-state [state]
  (when (not (zero? (:frame state)))
    ;set up canvas
    (q/blend-mode :blend)
    (q/background 255)
    (if (empty? (:names state))
      (do
        (q/blend-mode :blend)
        (q/with-fill [0]
          (q/text "done" (/ (q/width) 2) (/ (q/height) 2))))
      ;get voronoi polygons
      (let [name-group  (util/geoify-name (:font state) 
                                          (first (:names state))
                                          (first util/print-px)
                                          (second util/print-px)
                                          util/safety
                                          util/bleed
                                          util/print-dpi)
            name-poly   (.toPolygon name-group)
            badge-rect  (:badge-rect state)
            event-poly  (util/get-event-poly (:ahd-geo-font state)
                                             (first util/print-px)
                                             (second util/print-px)
                                             util/bleed
                                             util/print-dpi)
            event-poly-ud (geomerative.RPolygon. event-poly)
            points (apply util/get-rand-radial-points util/print-px)
            extra-points (concat points 
                                 (apply util/rotate-points 
                                        (concat [points]
                                                util/print-px)))
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
                                            (second util/print-px))
                                         0.5)
                                      (* util/print-dpi 0.1)))))
                             polygons
                             extra-points)]
        (.rotate event-poly-ud 
                 Math/PI 
                 (/ (first util/print-px) 2) 
                 (/ (second util/print-px) 2))
        ;(println (str "render" (first (first (:names state)))))
        ;draw to off-screen buffers
        (dorun
          (for [i (range (count (:graphics state)))]
            (do
              (util/draw-it
                (:altGraphic state)
                ;(util/things-to-geom
                ;  (take-nth 2 (drop i grown-polygons)))
                ;[255])
                (.intersection
                  badge-rect
                  (.union
                    (.diff
                      (util/things-to-geom
                        (take-nth 2 (drop i grown-polygons)))
                      name-poly)
                    (nth [event-poly event-poly-ud] i)))
                [255])
              (q/with-graphics
                (nth (:graphics state) i)
                (q/background 255)
                ;(q/image (:altGraphic state) 0 0)))))
                (let [img (.copy (nth (:patterns state) i))]
                (do
                  (q/mask-image
                    img
                    ;(.copy (nth (:patterns state) i))
                    (.copy (:altGraphic state)))
                    (q/image img 0 0)))))))

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
        (q/blend-mode :multiply)
        (let [g (first (:graphics state))
              width (.-width g)
              height (.-height g)]
          (q/with-graphics
            g
            (q/fill 0)
            (q/text-font (:ahd-font state) (* 40 
                                              (/ util/print-dpi 
                                                 util/screen-dpi)))
            (q/text "ART_\nHACK\n_DAY" 
                    (* util/print-dpi util/safety) 
                    (- height (+ (* 68 (/ util/print-dpi
                                          util/screen-dpi)) 
                                 (* util/print-dpi util/safety))))))
        ;(q/push-matrix)
        ;(q/scale (/ util/screen-dpi util/print-dpi))
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
            (q/image (nth (:graphics state) i) 0 0)))
        ;(q/pop-matrix)
        (q/blend-mode :blend)
        (let [d 0.28]
          (q/with-fill [0 255 255]
            (q/ellipse (/ (q/width) 2) 
                       (* util/screen-dpi (+ 0.23 (/ d 2) util/bleed)) 
                       (* util/screen-dpi d) 
                       (* util/screen-dpi d))
            (q/ellipse (/ (q/width) 2) 
                       (- (q/height) (* util/screen-dpi (+ 0.23 (/ d 2) util/bleed))) 
                       (* util/screen-dpi d) 
                       (* util/screen-dpi d))))
        (let [bleed-px (* util/screen-dpi util/bleed)
              safe-px  (* util/screen-dpi util/safety)]
          (q/with-fill [0 0]
            (q/with-stroke [255 0 0]
              (q/rect bleed-px
                      bleed-px
                      (- (q/width) (* bleed-px 2))
                      (- (q/height) (* bleed-px 2))))
            (q/with-stroke [0 255 0]
              (q/rect safe-px
                      safe-px
                      (- (q/width) (* safe-px 2))
                      (- (q/height) (* safe-px 2))))))
        (when (= (:frame state) 1)
          (util/save state))))))
