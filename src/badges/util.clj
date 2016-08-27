(ns badges.util
  (:import (geomerative RFont)
           (geomerative RCommand)
           (geomerative RG))
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def dpi 138)
(def inch_size [3 4])
(def pdf-dpi 72)
(def pdf-size [11 8.5])
(def font-size 100)
(def fullname '("Quin" "Kennedy"))
(def colors [[255 0 0]
             [0 255 0]
             [0 0 255]])

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
        ; align bottom of name with middle of badge
        (.translate group 
                    (- (* dpi 0.3) (:left bounds2)) 
                    (- (- (/ (q/height) 2)
                          (:bottom bounds2))
                       (* dpi 0.3)))
        group))))

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
  (dorun
    (for [i (range (count (:polygons state)))]
      (RG/saveShape (format "output/%s_layer%d.svg" 
                            (:timestamp state)
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
                  (:timestamp state))))
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

(defn add-e [pa pb]
  (let [size (/ (q/height) 2)
        paddingW (/ (- (q/width) 
                       size)
                   2)
        paddingH (/ (q/height) 2)]
    (mapv #(.translate % 
                       (- paddingW)
                       (- paddingH))
           [pa pb])
    (let [polygons
           (let [font (RFont. "Colfax-WebMedium.ttf" 300)
                 ePoly (.toPolygon (.toGroup font "E"))
                 numWide 20
                 numHigh 40
                 allPoints (for [x (range numWide)
                                 y (range numHigh)]
                             (list x y))
                 e-points (shuffle (filter in-e? allPoints))
                 not-e-points (shuffle (filter #(not (in-e? %)) allPoints))
                 points (concat (take 150 e-points) (take 50 not-e-points))
                 totalHeight (/ (q/height) 2)
                 totalWidth totalHeight
                 eachWide (/ totalWidth numWide)
                 eachHigh (/ totalHeight numHigh)]
             (loop [ps points
                    p1 pa
                    p2 pb]
               (if (empty? ps)
                 [p1 p2]
                 (let [x (first (first ps))
                       y (second (first ps))
                       rect (geomerative.RPolygon/createRectangle
                              (* x eachWide) (* y eachHigh)
                              eachWide eachHigh)]
                   (if (in-e? (first ps))
                     (recur (rest ps) 
                            (.xor p1 rect)
                            (.xor p2 rect))
                     (if (<= 0.5 (q/random 1)) 
                       (recur (rest ps)
                              (.xor p1 rect)
                              p2)
                       (recur (rest ps)
                              p1
                              (.xor p2 rect))))))))]
      (mapv #(.translate %
                         paddingW
                         paddingH)
            polygons)
      polygons)))

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
