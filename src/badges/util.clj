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
(def colors [[0 0 0]
             [0 0 0]
             ])

(defn intersect? [bound1 bound2]
  (and (and (> (:right bound1) (:left bound2))
            (> (:right bound2) (:left bound1)))
       (and (> (:bottom bound1) (:top bound2))
            (> (:bottom bound2) (:top bound1)))))

(defn split-poly [polys amount]
  (let [split-point (+ (/ (- (q/width) (q/height)) 2) 
                       (* (q/height) amount))
        left (geomerative.RPolygon/createRectangle
               (- (q/height))
               (- (q/height))
               (- split-point (- (q/height)))
               (* (q/height) 3))
        right (geomerative.RPolygon/createRectangle
                split-point
                (- (q/height))
                (- (* (q/height) 2) split-point)
                (* (q/height) 3))]
    (filter #(not (nil? %))
            (concat (map #(try
                            (.intersection left %)
                            (catch Exception e nil))
                         polys)
                    (map #(try
                            (.intersection right %)
                            (catch Exception e nil))
                         polys)))))

(defn split-times [poly n]
  (if (zero? n)
    [poly]
    (let [step (/ (* Math/PI 2) n)
          center [(/ (q/width) 2)
                  (/ (q/height) 2)]]
      (loop [i n
             polys [poly]]
        (if (zero? i)
          polys
          (recur (dec i)
                 (split-poly
                   (map (fn [poly]
                          (.rotate poly step (first center) (second center))
                          poly)
                        polys)
                   (q/random 0.3 0.7))))))))

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
    (if (= i 26)
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

(defn translate [poly x y]
  (.translate poly x y)
  poly)

(defn get-text-polys [font font-info text-lines]
  (flatten
    (map (fn [line y]
           (map (fn [character x]
                  (translate
                    (.toPolygon font
                                character)
                    (* x (:h-spacing font-info))
                    (* y (:v-spacing font-info))))
                line
                (range)))
           text-lines
           (range))))

;  (reduce (fn [poly [line index]]
;            (let [line-polys (map #(translate (.toPolygon font %1) (* %2 (:h-spacing font-info)) 0) line (range))]
;              (.translate line-poly 0 (* (:v-spacing font-info) index))
;              (.union
;                poly
;                line-poly)))
;          (geomerative.RPolygon.)
;          (map list text-lines (range))))

(defn measure-font [font]
  (let [test-char \u00AC
        one-char (.toGroup font (str test-char))
        two-char (.toGroup font (str test-char test-char))
        one-char-bounds (getBounds (.getPoints one-char))
        two-char-bounds (getBounds (.getPoints two-char))]
    {:width (:width one-char-bounds)
     :height (:height one-char-bounds)
     :v-spacing (.getLineSpacing font)
     :h-spacing (- (:width two-char-bounds) (:width one-char-bounds))}))

(defn get-rand-text-bg [num-wide num-high]
  (for [y (range num-high)]
    (apply str 
           (map (fn [a] 
                  (if (> (q/random 1) 0.03)
                    \_
                    \u00AC))
                (range num-wide)))))

(defn merge-text [text other]
  (apply str 
         (map (fn [a b]
                (if (= b \ )
                  a
                  b))
              text
              (concat other (repeat \ )))))

(defn add-ahd-brand [text]
  (let [branding [""
                  " ART_"
                  " HACK"
                  " _DAY"]]
    (map merge-text
         text
         (concat branding (repeat "")))))

(defn get-rand-string [brand num-wide]
  (apply str
         (reduce (fn [curr-text sym]
                   (if (= sym \_)
                     (apply str 
                            (concat (rest curr-text)
                                    (list (first curr-text))))
                     (str curr-text \ )))
                 brand
                 (shuffle
                   ;get string of '_' and ' ' characters
                   ; '_' will be replaced by branding letters
                   (take num-wide 
                         (concat (repeat (count brand) \_) 
                                 (repeat \ )))))))

(defn add-event-brand [text num-wide]
  (let [brand-string "ERASURE"
        brand-text (get-rand-string brand-string (+ (count brand-string) 1))
        branding [""""""""
                  (apply str (concat (repeat
                                       (- num-wide
                                          (inc (/ (count brand-text) 2)))
                                       \ )
                                     (take (/ (count brand-text) 2) brand-text)))
                  (apply str (concat (repeat 
                                       (- num-wide
                                          (inc (/ (count brand-text) 2)))
                                       \ )
                                     (drop (/ (count brand-text) 2) brand-text)))]]
    (map merge-text
         text
         (concat branding (repeat "")))))

(defn rotate-char [char-polys x y num-wide num-high]
  (let [char-index (+ (* y num-wide) x)
        opp-index  (- (* num-wide num-high) char-index 1)]
    (.translate 
      (nth char-polys char-index) 
      (/ (q/width) 2) 
      (/ (q/height) 2))
    char-polys))

(defn build-full-font-cover [font font-info num-wide num-high]
  ; get text to start with
  (map
    translate
    (get-text-polys font
                    font-info
                    (add-event-brand
                      (add-ahd-brand
                        (get-rand-text-bg num-wide num-high)) num-wide))
    ; center horizontally
    (repeat (/ (- (q/width) 
                  (+ (* (dec num-wide) (:h-spacing font-info)) 
                     (:width font-info))) 
               2))
    ; center vertically
    ;  taking into account that 
    ;  the font is rendered with 
    ;  the left baseline edge at 0,0
    (repeat (+ (:height font-info)
               (/ (- (q/height) 
                     (+ (* (dec num-high) (:v-spacing font-info)) 
                        (:height font-info))) 
                  2)))))

(defn incl-point-mirrors [points num-wide]
  (reduce
    (fn [out [x y]]
      (conj out [x y] [(- num-wide x 1) y]))
    (list)
    points))

(defn incl-point-rots [points num-wide num-high]
  (reduce
    (fn [out [x y]]
      (conj out [x y] [(- num-wide x 1) (- num-high y 1)]))
    (list)
    points))

(defn migrate-by-pos [source indices]
  (let [dest (map #(nth source %)
                  indices)]
    [(map (fn [p i]
            (if (some #(= i %)
                      indices)
              (geomerative.RPolygon.)
              p))
          source
          (range))
     dest]))

(defn scale 
  ([poly s]
    (.scale poly s))
  ([poly sx sy cx cy]
    (.scale poly sx sy cx cy)
    poly))

(defn build-font-cover [font font-info avoid-bounds]
  (let [num-wide (+ (int (/ (q/width) (:h-spacing font-info))) 2)
        num-high (+ (int (/ (q/height) (:v-spacing font-info))) 1)
        ;get characters to cover badge
        all-chars (build-full-font-cover font font-info num-wide num-high)
        ;get set of flipp-able (and rotatable) event characters
        to-flip ;(incl-point-rots
                  (incl-point-mirrors
                    (take 4 (shuffle (for [x (range 8 (dec num-wide))
                                           y (range 4 6)]
                                       [x y])))
                    num-wide)
                  ;num-wide
                  ;num-high)
        ;get set of rotatable ahd characters
        to-rotate (incl-point-rots
                    (take 4 (shuffle (for [x (range 1 5)
                                           y (range 1 4)]
                                       [x y])))
                    num-wide
                    num-high)
        filtered-a (migrate-by-pos all-chars 
                                   (map (fn [[x y]]
                                          (+ (* y num-wide) x))
                                        to-rotate))
        filtered-b (migrate-by-pos (first filtered-a)
                                   (map (fn [[x y]]
                                          (+ (* y num-wide) x))
                                        to-flip))
        bg-chars (filter (fn [chr]
                            (not
                              (let [bounds (getBounds (.getPoints chr))]
                                (reduce 
                                  #(or %1 %2)
                                  (map
                                    #(intersect?
                                       (merge bounds
                                              {})
                                              ;{:top (- (:bottom bounds) (:height font-info))
                                              ; :right (+ (:left bounds) (:width font-info))})
                                       %)
                                    avoid-bounds)))))
                         (filter #(not (zero? (.countContours %)))
                                 (first filtered-b)))]
    [bg-chars
     (map #(scale % -1 1 (/ (q/width) 2) (/ (q/height) 2))
          (concat
            (map #(scale % 1 -1 (/ (q/width) 2) (/ (q/height) 2))
                 (second filtered-a))
            (second filtered-b)))]))

(defn get-event-poly [font]
  (let [polygon (.toPolygon (.toGroup font "Erasure"))]
    (.scale polygon 0.5)
    (let [bounds (getBounds (.getPoints polygon))]
      (.translate polygon
                  (- (/ (- (q/width) (:width bounds)) 2) (:left bounds))
                  (/ (* (q/height) 9) 10))
      polygon)))

(defn polygon [contours]
  (reduce (fn [p c] 
            (.addContour p c)
            p)
          (geomerative.RPolygon. (first contours))
          (rest contours)))

(defn geoify-name [font fullname]
  ; render first and last name
  (let [groups [(.toPolygon (.toGroup font (first fullname)))
                ; put last name under first name
                (translate (.toPolygon (.toGroup font (second fullname))) 0 font-size)]]
    (let [bounds (map #(getBounds (.getPoints %))
                      groups)
          scale-amt (min 1 
                      (/ (- (q/width) 
                            (* dpi (* 0.3 2))) 
                         (reduce max
                                 (map #(:width %) 
                                      bounds))))]
      ; scale name to fit horizontally
      (doall
        (map #(scale % scale-amt) groups))
      (let [bounds2 (getBounds (.getPoints (union-all groups)))]
        ; align top of name with middle of badge
        (map #(translate %
                         (- (* dpi 0.3) (:left bounds2)) 
                         (- (- (/ (q/height) 2)
                               (:top bounds2))
                            (* dpi 0.3)))
             groups)))))

(defn draw-it [graphics polygon color]
  (q/with-graphics 
    graphics
    (q/clear)
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
