(ns badges.util
  (:import (geomerative RFont)
           (megamu.mesh Voronoi)
           (geomerative RCommand)
           (geomerative RG))
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [thi.ng.math.core :as mc]
            [thi.ng.geom.vector :as gv]
            [thi.ng.geom.polygon :as gp]
            ))

(def print-dpi 600)
(def screen-dpi 138)
(def finish-size [3 4.5])
(def bleed 0.25)
(def margin 0.2)
(def safety (+ bleed margin))
(def inch-size (mapv + finish-size (repeat (* bleed 2))))
(def print-px (mapv * inch-size (repeat print-dpi)))
(def pdf-dpi 72)
(def pdf-size [8.5 11])
(def font-size 100)
(def fullname '("Quin" "Kennedy"))
(def print-margin 0.25)
(def colors [[255 93 48]
             [1 163 160]
             ])

(defn add-corner-cut-marks [pdf]
  (let [out-line (* pdf-dpi bleed)
        start-line (* pdf-dpi (- (/ bleed 2)))
        end-line (+ start-line (* pdf-dpi bleed))]
    (q/line out-line start-line out-line end-line)
    (q/line start-line out-line end-line out-line)))

(defn add-cut-marks [pdf]
  (add-corner-cut-marks pdf)
  (q/push-matrix)
  (q/translate (* (first inch-size) pdf-dpi) 0)
  (q/scale -1 1)
  (add-corner-cut-marks pdf)
  (q/pop-matrix)
  (q/push-matrix)
  (q/translate 0 (* (second inch-size) pdf-dpi))
  (q/scale 1 -1)
  (add-corner-cut-marks pdf)
  (q/push-matrix)
  (q/translate (* (first inch-size) pdf-dpi) 0)
  (q/scale -1 1)
  (add-corner-cut-marks pdf)
  (q/pop-matrix)
  (q/pop-matrix))

(defn to-pdf [state]
  (let [index (dec (:frame state))
        place (mod index 4)]
    (doall
      (for [i (range (count (:pdfs state)))]
        (q/with-graphics
          (nth (:pdfs state) i)
          (when (and (zero? place) (not (zero? index)))
            (.nextPage (nth (:pdfs state) i)))
            (q/with-translation [(* pdf-dpi (+ print-margin 
                                               (* (mod place 2)
                                                  (first inch-size))))
                                 (* pdf-dpi (+ print-margin 
                                               (* (int (/ place 2))
                                                  (second inch-size))))]
              (q/push-matrix)
              (q/scale (/ pdf-dpi print-dpi))
              (q/image (nth (:graphics state) i) 0 0)
              (q/pop-matrix)
              (add-cut-marks (nth (:pdfs state) i))
              ))))))

(defn finish-pdf [state]
  (doall
    (for [pdf (:pdfs state)]
      (q/with-graphics
        pdf
        (.dispose pdf)))))

(defn get-honeycomb-centers [num-wide width height]
  (let [x-step (/ width num-wide)
        y-step (Math/sqrt
                 (-
                   (* x-step x-step)
                   (Math/pow 
                     (/ x-step 2) 
                     2)))]
    (for [x (range num-wide)
          y (range (inc (* (/ (* (/ num-wide y-step) x-step) width) height)))]
      [(+ (* x x-step) (if (even? y) (/ x-step 2) 0))
       (* y y-step)])))

(defn apply-dots [graphics inv]
  (let [num-wide 100
        width (.-width graphics)
        height (.-height graphics)
        centers (get-honeycomb-centers num-wide width height)
        diameter (/ width num-wide 2)]
    (q/with-graphics
      graphics
      (q/clear)
      (if inv
        (q/background 0)
        (q/background 255 0))
      ;(q/no-stroke)
      (q/with-fill (if inv [255] [0])
        (q/with-stroke (if inv [255] [0])
        (doall
          (for [center centers]
            (q/ellipse
              (first center)
              (second center)
              diameter diameter))))))))

(defn apply-lines [graphics]
    (q/with-graphics
      graphics
      (let [num-wide 120
            width (.-width graphics)
            height (.-height graphics)
            step (int (/ width num-wide))
            weight (/ step 2)
            offset (int (- (/ width 2)))]
        (q/clear)
        (q/background 255 0)
        (q/stroke 0)
        (q/stroke-weight weight)
        (dorun
          (for [i (take-nth step (range (+ width (Math/abs offset))))]
            (q/line i 0 (+ i offset) height))))))

(defn voronoi [points] 
  (Voronoi. (into-array (map float-array points))))

(defn region-to-polygon [region center]
  (let [all-points (mapv vec (.getCoords region))
        cross (mc/cross 
                (gv/vec2 
                  (map - 
                       (first all-points) 
                       center)) 
                (gv/vec2 
                  (map -
                       (second all-points)
                       center)))
        cw-points (if (> cross 0) (reverse all-points) all-points)]
    (gp/polygon2 cw-points)))

(defn thing-to-geom [polygon]
  (geomerative.RPolygon. (into-array 
                           (map (fn [[x y]] 
                                  (geomerative.RPoint. x y)) 
                                (:points polygon)))))

(defn things-to-geom [polygons]
  (reduce #(.union %1 %2) 
          (map thing-to-geom polygons)))

(defn draw-polygon [polygon]
   (q/begin-shape)
   (doall
     (for [point (:points polygon)]
       (apply q/vertex point)))
   (q/end-shape))

(defn draw-polygons [graphic color polygons]
  (q/with-graphics 
    graphic
    (q/clear)
    ;(q/background 255 255 255 0)
    (q/no-stroke)
    (q/with-fill color
      (dorun
        (for [polygon polygons]
          (draw-polygon polygon))))))

(defn polar-to-cartesian [radius angle]
  [(* radius (Math/cos angle))
   (* radius (Math/sin angle))])

(defn cartesian-to-polar [[x y]]
  [(Math/sqrt (+ (* x x) (* y y)))
   (Math/atan2 y x)])

(defn get-rand-radial-points [width height]
  (loop [i 0
         points []]
    (if (= i 12)
      points
      (let [point (map + 
                       (polar-to-cartesian 
                         (q/random (/ height 2))
                         (q/random (* Math/PI 2)))
                       [(/ width 2) 
                        (/ height 2)])]
        (if (and (and (> (first point) 0) 
                      (> width (first point)))
                 (and (> (second point) 0)
                      (> height (second point))))
          (recur (inc i)
                 (conj points point))
          (recur i points))))))

(defn rotate-points [points width height]
  (let [center [(/ width 2) (/ height 2)]]
    (mapv #(let [polar (cartesian-to-polar (mapv - % center))]
             (mapv + 
                   (polar-to-cartesian 
                     (first polar)
                     (mod (+ (second polar) Math/PI) (* Math/PI 2)))
                   center))
          points)))

(defn get-color [n]
  (mapv #(- 255 %)
        (nth colors n)))

(defn createPolyRect [x y w h]
  (geomerative.RPolygon/createRectangle
    x y w h))

(defn union-all [polys]
  (reduce #(.union %1 %2) polys))

(defn get-timestamp []
  (format "%05d_%02d_%02d_%02d_%02d_%02d_%03d"
          (q/year) (q/month) (q/day) 
          (q/hour) (q/minute) (q/seconds)
          (mod (System/currentTimeMillis) 1000)))

(defn getBounds [points]
  (let [top (apply min (map #(.-y %) points))
        bottom (apply max (map #(.-y %) points))
        left (apply min (map #(.-x %) points))
        right (apply max (map #(.-x %) points))]
    {:top    top
     :bottom bottom
     :left   left
     :right  right
     :width  (- right left)
     :height (- bottom top)}
))

;http://dacamo76.com/blog/2014/11/28/updating-map-values-in-clojure/
(defn map-values [m keys f & args]
  (reduce #(apply update-in %1 [%2] f args) m keys))

;http://stackoverflow.com/a/9638672
(defn update-vals [map vals f]
    (reduce #(update-in % [%2] f) map vals))

(defn addPad [bounds padding]
  (map-values
    (map-values 
      (map-values 
        bounds 
        [:bottom :right] 
        + padding)
      [:top :left]
      - padding)
    [:width :height]
    + (* padding 2)))

(defn polar-to-cartesian [radius angle]
  [(* radius (Math/cos angle))
   (* radius (Math/sin angle))])

(defn get-event-poly [font width height side-space dpi]
  (let [text "ERASURE"
        order (shuffle (take 10 (concat (repeat (count text) true) (repeat false))))
        us-text (apply str 
                       (first
                         (reduce #(if %2 
                                      [(conj (first %1) (first (second %1)))
                                       (rest (second %1))]
                                      [(conj (first %1) \_)
                                       (second %1)])
                                   [[] text]
                                   order)))
        polygon (.toPolygon (.toGroup font us-text))
        bounds (getBounds (.getPoints polygon))]
    ; scale name to fit horizontally
    (.scale polygon
            (/ (- width (* side-space dpi 2))
               (:width bounds)))
    (let [bounds2 (getBounds (.getPoints polygon))]
      ; align top of name with top of badge
      (.translate polygon
                  (+ (/ (- width (:width bounds2)) 2)
                     (- (:left bounds2)))
                  (+ (- (:top bounds2))
                     (/ height 2)))
                  ;(+ (- (/ (q/height) 2)
                  ;      (:top bounds2))
                  ;   (* dpi 0.3)))
      polygon)))

(defn geoify-name [font fullname width height side-padding top-padding dpi]
  ; render first and last name
  (let [group (.toGroup font (or (first fullname) " "))
        group2 (.toGroup font (or (second fullname) " "))]
    ; add last name under first name
    (.translate group2 0 font-size)
    (.addGroup group group2)
    (let [bounds (getBounds (.getPoints group))]
      ; scale name to fit horizontally
      (.scale group 
              (min (/ print-dpi screen-dpi) 
                   (/ (- width 
                         (* dpi side-padding 2))
                      (:width bounds))))
      (let [bounds2 (getBounds (.getPoints group))]
        ; align top of name with top of badge
        (.translate group 
                    (- (* dpi side-padding) (:left bounds2)) 
                    (+ (- (:top bounds2))
                       (* dpi (+ top-padding 0.23 0.28 0.1))))
                    ;(+ (- (/ (q/height) 2)
                    ;      (:top bounds2))
                    ;   (* dpi 0.3)))
        group))))

(defn draw-it [graphics polygon color]
  (q/with-graphics 
    graphics
    ;(q/clear)
    (q/background 0)
    ;(q/background 255)
    ;(q/background 255 255 255 0)
    (q/no-stroke)
    (q/with-fill color
      (.draw 
        (.toShape polygon)
        graphics)))
  graphics)

(defn save [state]
  (let [timestamp (get-timestamp)]
    ;(dorun
    ;  (for [i (range (count (:polygons state)))]
    ;    (RG/saveShape (format "output/%s_layer%d.svg" 
    ;                          timestamp
    ;                          i)
    ;                (.toShape (nth (:polygons state) i)))))

    ;;
    ;; Print out individual contours of Polygon
    ;;
    ;(let [contours (.-contours (:topPoly state))]
    ;  (doall
    ;  (for [i (range (count contours))]
    ;    (let [contour (get contours i)]
    ;      (RG/saveShape (format "output/%s_layer2_%02d_%b.svg"
    ;                            timestamp
    ;                            i
    ;                            (.isHole contour))
    ;                    (.toShape (.toPolygon contour)))))))
    (q/save (format "output/%s_render" 
                    timestamp))
    ;(doall
    ;  (for [i (range (count (:patterns state)))]
    ;    (.save (nth (:patterns state) i)
    ;           (format "output/%s_pattern_%d"
    ;                   timestamp
    ;                   i))))
    (doall
      (for [i (range (count (:graphics state)))]
        (.save (nth (:graphics state) i)
               (format "output/%s_graphic_%d"
                       timestamp
                       i))))))

(defn clean-up [polygon]
  (let [updated (.update polygon)
        positive (.toPolygon
                   (reduce (fn [a b] 
                             (let [boxA (getBounds (.getPoints a))
                                   boxB (getBounds (.getPoints b))]
                               (if (> (* (:width boxA) (:height boxA))
                                      (* (:width boxB) (:height boxB)))
                                 a
                                 b)))
                           (.-contours updated)))]
    (doall
    (for [contour (filter (fn [c] (.isHole c)) (.-contours updated))]
      (.addContour positive contour)))
    positive))
