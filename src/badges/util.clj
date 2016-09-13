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

(def dpi 138)
(def inch_size [3 4])
(def pdf-dpi 72)
(def pdf-size [11 8.5])
(def font-size 100)
(def fullname '("Quin" "Kennedy"))
(def colors [[255 93 48]
             [1 163 160]
             [0 0 0]
             ])

(defn get-honeycomb-centers [num-wide]
  (let [x-step (/ (q/width) num-wide)
        y-step (Math/sqrt
                 (-
                   (* x-step x-step)
                   (Math/pow 
                     (/ x-step 2) 
                     2)))]
    (for [x (range num-wide)
          y (range (inc (* (/ (* (/ num-wide y-step) x-step) (q/width)) (q/height))))]
      [(+ (* x x-step) (if (even? y) (/ x-step 2) 0))
       (* y y-step)])))

(defn get-circle-pattern [num-wide]
  (let [
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

(defn apply-dots [graphics inv]
  (let [centers (get-honeycomb-centers 100)]
    (q/with-graphics
      graphics
      ;(q/clear)
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
              2 2))))))))

(defn apply-lines [graphics]
  (q/with-graphics
    graphics
    ;(q/clear)
    (q/background 255 0)
    (q/stroke 0)
    ;(q/stroke-weight 2)
    (dorun
      (for [i (filter even? (range (q/width)))]
        (q/line i 0 i (q/height))))))

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

(defn get-rand-points []
  (loop [i 0
         points []]
    (if (= i 26)
      points
      (recur (inc i)
             (conj points 
                   [(q/random (q/width)) 
                    (q/random (q/height))])))))

(defn get-rand-radial-points []
  (loop [i 0
         points []]
    (if (= i 12)
      points
      (let [point (map + 
                       (polar-to-cartesian 
                         (q/random (/ (q/height) 2))
                         (q/random (* Math/PI 2)))
                       [(/ (q/width) 2) 
                        (/ (q/height) 2)])]
        (if (and (and (> (first point) 0) 
                      (> (q/width) (first point)))
                 (and (> (second point) 0)
                      (> (q/height) (second point))))
          (recur (inc i)
                 (conj points point))
          (recur i points))))))

(defn flip-points [points]
  (let [half-width (/ (q/width) 2)]
    (mapv (fn [[x y]] [(+ half-width (- half-width x)) y])
          points)))

(defn rotate-points [points]
  (let [center [(/ (q/width) 2) (/ (q/height) 2)]]
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

(defn get-brand-poly [font]
  (let [p1 (.toPolygon (.toGroup font "ART_"))
        p2 (.toPolygon (.toGroup font "HACK"))
        p3 (.toPolygon (.toGroup font "_DAY"))]
    (.translate p2 0 font-size)
    (.translate p3 0 (+ font-size font-size))
    ;(.scale polygon 0.6)
    (let [full-poly (union-all [p1 p2 p3])
          bounds (getBounds (.getPoints full-poly))]
      (.translate full-poly
                  (- (/ (- (q/width) (:width bounds)) 2) (:left bounds))
                  ;center
                  (- (/ (- (q/height) (:height bounds)) 2) (:top bounds)))
                  ;(/ (* (q/height) 9) 10))
      full-poly)))

(defn get-event-poly [font]
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
        polygon (.toPolygon (.toGroup font (apply str (take 5 us-text))))
        polygon2 (.toPolygon (.toGroup font (apply str (drop 5 us-text))))]
    (.translate polygon2 0 font-size)
    ;(.scale polygon 0.6)
    (let [full-poly (.union polygon polygon2)
          bounds (getBounds (.getPoints full-poly))]
      (.translate full-poly
                  (- (/ (- (q/width) (:width bounds)) 2) (:left bounds))
                  ;center
                  (- (/ (- (q/height) (:height bounds)) 2) (:top bounds)))
                  ;(/ (* (q/height) 9) 10))
      full-poly)))

(defn geoify-name [font fullname]
  ; render first and last name
  (let [group (.toGroup font (first fullname))
        group2 (.toGroup font (second fullname))]
    ; add last name under first name
    (.translate group2 0 font-size)
    (.addGroup group group2)
    (let [bounds (getBounds (.getPoints group))]
      ; scale name to fit horizontally
      (.scale group 
              (min 1 
                   (/ (- (q/width) 
                         (* dpi (* 0.3 2))) 
                      (:width bounds))))
      (let [bounds2 (getBounds (.getPoints group))]
        ; align top of name with top of badge
        (.translate group 
                    (- (* dpi 0.3) (:left bounds2)) 
                    (+ (- (:top bounds2))
                       (* dpi 0.3)))
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
    (dorun
      (for [i (range (count (:polygons state)))]
        (RG/saveShape (format "output/%s_layer%d.svg" 
                              timestamp
                              i)
                    (.toShape (nth (:polygons state) i)))))
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
                    timestamp))))
;  ;;
;  ;; Render to PDF to match screen colors
;  ;;
;  (q/with-graphics
;    (:altGraphic state)
;    (q/background 255)
;    (q/blend-mode :subtract)
;    (q/image
;      (:topGraphic state)
;      0 0))
;  (q/with-graphics
;    (:pdf state)
;    (q/translate pdf-dpi (* pdf-dpi 0.25))
;    (q/scale (/ pdf-dpi dpi))
;    (q/image
;      (:altGraphic state)
;      0 0))
;  (q/with-graphics
;    (:altGraphic state)
;    (q/background 255)
;    (q/blend-mode :subtract)
;    (q/image
;      (:bottomGraphic state)
;      0 0))
;  (q/with-graphics
;    (:pdf state)
;    (q/translate pdf-dpi (* pdf-dpi 0.25))
;    (q/scale (/ pdf-dpi dpi))
;    (q/translate 0 (q/height))
;    (q/image
;      (:altGraphic state)
;      0 0))
;  ;;
;  ;; Render to PDF using Cyan and Magenta
;  ;;
;  (q/with-graphics
;    (:altGraphic state)
;    (q/background 255)
;    (q/blend-mode :blend))
;  (draw-it (:altGraphic state)
;                (:topPoly state)
;                [0 255 255])
;  (q/with-graphics
;    (:pdf state)
;    (q/translate pdf-dpi (* pdf-dpi 0.25))
;    (q/scale (/ pdf-dpi dpi))
;    (q/translate (q/width) 0)
;    (q/image
;      (:altGraphic state)
;      0 0))
;  (q/with-graphics
;    (:altGraphic state)
;    (q/background 255)
;    (q/blend-mode :blend))
;  (draw-it (:altGraphic state)
;                (:bottomPoly state)
;                [255 0 255])
;  (q/with-graphics
;    (:pdf state)
;    (q/translate pdf-dpi (* pdf-dpi 0.25))
;    (q/scale (/ pdf-dpi dpi))
;    (q/translate (q/width) (q/height))
;    (q/image
;      (:altGraphic state)
;      0 0))
;  ;;
;  ;; Render to PDF with Red Blue
;  ;;
;  (q/with-graphics
;    (:altGraphic state)
;    (q/background 255)
;    (q/blend-mode :blend))
;  (draw-it (:altGraphic state)
;                (:topPoly state)
;                [255 0 0])
;  (q/with-graphics
;    (:pdf state)
;    (q/translate pdf-dpi (* pdf-dpi 0.25))
;    (q/scale (/ pdf-dpi dpi))
;    (q/translate (* (q/width) 2) 0)
;    (q/image
;      (:altGraphic state)
;      0 0))
;  (q/with-graphics
;    (:altGraphic state)
;    (q/background 255)
;    (q/blend-mode :blend))
;  (draw-it (:altGraphic state)
;                (:bottomPoly state)
;                [0 0 255])
;  (q/with-graphics
;    (:pdf state)
;    (q/translate pdf-dpi (* pdf-dpi 0.25))
;    (q/scale (/ pdf-dpi dpi))
;    (q/translate (* (q/width) 2) (q/height))
;    (q/image
;      (:altGraphic state)
;      0 0)
;  ;;
;  ;; Render to PDF with inverted screen colors
;  ;;
;  ;(q/with-graphics
;  ;  (:pdf state)
;  ;  (q/translate pdf-dpi (* pdf-dpi 0.25))
;  ;  (q/scale (/ pdf-dpi dpi))
;  ;  (q/translate (* (q/width) 2) 0)
;  ;  (q/image
;  ;    (:topGraphic state)
;  ;    0 0)
;  ;  (q/translate 0 (q/height))
;  ;  (q/image
;  ;    (:bottomGraphic state)
;  ;    0 0)
;    (.dispose (:pdf state))))

(def the-e [[0 0 0 0 0 0 0 0 0 0]
            [0 0 1 1 1 1 1 1 0 0]
            [0 0 1 1 1 1 1 1 0 0]
            [0 0 1 1 1 1 1 1 0 0]
            [0 0 1 1 1 1 1 1 0 0]
            [0 0 1 1 0 0 0 0 0 0]
            [0 0 1 1 0 0 0 0 0 0]
            [0 0 1 1 0 0 0 0 0 0]
            [0 0 1 1 1 1 0 0 0 0]
            [0 0 1 1 1 1 0 0 0 0]
            [0 0 1 1 1 1 0 0 0 0]
            [0 0 1 1 1 1 0 0 0 0]
            [0 0 1 1 0 0 0 0 0 0]
            [0 0 1 1 0 0 0 0 0 0]
            [0 0 1 1 0 0 0 0 0 0]
            [0 0 1 1 1 1 1 1 0 0]
            [0 0 1 1 1 1 1 1 0 0]
            [0 0 1 1 1 1 1 1 0 0]
            [0 0 1 1 1 1 1 1 0 0]
            [0 0 0 0 0 0 0 0 0 0]])

(defn in-e? [pt]
  (not (zero? (get
                (get
                  the-e
                  (int (/ (second pt) 2)))
                (int (/ (first pt) 2))))))

(defn add-e [polygons]
  (let [size     (/ (q/height) 2)
        paddingW (/ (- (q/width) 
                       size)
                    2)
        paddingH (/ (q/height) 2)]
    (mapv #(.translate % 
                       (- paddingW)
                       (- paddingH))
           polygons)
    (let [polys
           (let [font (RFont. "Colfax-WebMedium.ttf" 300)
                 ePoly (.toPolygon (.toGroup font "E"))
                 numWide 20
                 numHigh 40
                 allPoints (for [x (range numWide)
                                 y (range numHigh)]
                             (list x y))
                 e-points (shuffle (filter in-e? allPoints))
                 not-e-points (shuffle (filter #(not (in-e? %)) allPoints))
                 points (concat 
                          (take 
                            (/ (count e-points) 3)
                            ;(/ (count not-e-points) (count polygons)) 
                            e-points) 
                          not-e-points)
                 totalHeight (/ (q/height) 2)
                 totalWidth totalHeight
                 eachWide (/ totalWidth numWide)
                 eachHigh (/ totalHeight numHigh)]
             (loop [ps points
                    pgs polygons]
               (if (empty? ps)
                 pgs
                 (let [x (first (first ps))
                       y (second (first ps))
                       rect (geomerative.RPolygon/createRectangle
                              (* x eachWide) (* y eachHigh)
                              eachWide eachHigh)]
                   (if (in-e? (first ps))
                     (recur (rest ps) 
                            (mapv #(.xor % rect) pgs))
                     (let [select (int (rand (count pgs)))]
                       (if true;(< 0.5 (rand))
                         (recur (rest ps)
                                (update pgs
                                        select
                                        #(.xor % rect)))
                         (recur (rest ps)
                                (update-vals 
                                  pgs 
                                  [select (mod (inc select) (count pgs))]
                                  #(.xor % rect))))))))))]
      (mapv #(.translate %
                         paddingW
                         paddingH)
            polys)
      polys)))

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
