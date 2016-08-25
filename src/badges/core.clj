(ns badges.core
  (:import (geomerative RFont)
           (geomerative RCommand)
           (geomerative RG))
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def dpi 138)
(def inch_size [3 4])

(defn getBounds [points]
  {:top (apply min (map #(.-y %) points))
   :bottom (apply max (map #(.-y %) points))
   :left (apply min (map #(.-x %) points))
   :right (apply max (map #(.-x %) points))}
)

(defn draw-it [polygon color]
  (let [graphics (q/create-graphics (q/width) (q/height) :p2d)]
    (q/with-graphics 
      graphics
      (q/no-stroke)
      (q/with-fill color
        (.draw 
          (.toShape polygon)
          graphics)))
    graphics))

(defn setup []
  (RG/init (quil.applet/current-applet))
  (RCommand/setSegmentLength 1)
  (RCommand/setSegmentator RCommand/UNIFORMLENGTH)
  (let [timestamp (format "%05d_%02d_%02d_%02d_%02d_%02d_%03d"
                            (q/year) (q/month) (q/day) 
                            (q/hour) (q/minute) (q/seconds)
                            (mod (System/currentTimeMillis) 1000))
        font (RFont. "Colfax-WebMedium.ttf" 100)
        group (.toGroup font "Quin Kennedy")
        bounds (getBounds (.getPoints group))]
    (.scale group 
            (min 1 
                 (/ (- (q/width) 
                       (* dpi (* 0.3 2))) 
                    (- (:right bounds) 
                       (:left bounds)))))
    (let [bounds2 (getBounds (.getPoints group))]
      (.translate group 
                  (- (* dpi 0.3) (:left bounds2)) 
                  (- (/ (- (q/height) 
                           (- (:bottom bounds2) 
                              (:top bounds2))) 
                        2) 
                     (:top bounds2)))
      (let [topPoly (.union
                      (.toPolygon group)
                      (geomerative.RPolygon/createRectangle 
                            0 0 (q/width) (/ (q/height) 2)))
            bottomPoly (.union
                         (.toPolygon group)
                         (geomerative.RPolygon/createRectangle 
                            0 (/ (q/height) 2) (q/width) (/ (q/height) 2)))]
        ; Set frame rate to 30 frames per second.
        (q/frame-rate 30)
        ; Set color mode to HSB (HSV) instead of default RGB.
        (q/color-mode :rgb)
        (RG/saveShape (format "output/%s_layer1.svg" timestamp) 
                      (.toShape bottomPoly))
        (RG/saveShape (format "output/%s_layer2.svg" timestamp) 
                      (.toShape topPoly))
        ; setup function returns initial state. It contains
        ; circle color and position.
        {:font font
         :name group
         :timestamp timestamp
         :top  (draw-it topPoly
                        [255 0 0])
         :bottom (draw-it bottomPoly
                          [0 0 255])
         :frame 0}
))))

(defn update-state [state]
  (update state :frame inc))
;  ; Update sketch state by changing circle color and position.
;  {:color (mod (+ (:color state) 0.7) 255)
;   :angle (+ (:angle state) 0.1)})

;http://dacamo76.com/blog/2014/11/28/updating-map-values-in-clojure/
(defn map-values [m keys f & args]
  (reduce #(apply update-in %1 [%2] f args) m keys))

(defn addPad [bounds padding]
  (map-values 
    (map-values bounds [:bottom :right] + padding)
    [:top :left]
    - padding))

(defn draw-state [state]
  (when (= (:frame state) 1)
    (let [box (getBounds (.getPoints (:name state)))
          ]
      (q/background 255)
      (q/blend-mode :subtract)
      (q/image (:top state) 0 0)
      (q/image (:bottom state) 0 0)
      (q/save (format "output/%s_render" (:timestamp state)))
      )))
    ;(q/no-stroke)
    ;(q/blend-mode :subtract)
    ;;(q/with-stroke [0 0]
    ;  (q/with-fill [255 0 0 100]
    ;    (.draw 
    ;      (.toShape
    ;        (.union 
    ;          (.toPolygon (:name state)) 
    ;          (geomerative.RPolygon/createRectangle 0 0 (q/width) (/ (q/height) 2))))))
    ;  (q/with-fill [0 0 255 100]
    ;    (.draw 
    ;      (.toShape
    ;        (.union 
    ;          (.toPolygon (:name state)) 
    ;          (geomerative.RPolygon/createRectangle 0 (/ (q/height) 2) (q/width) (/ (q/height) 2))))))));)
    ;(q/with-fill [0 0]
     ; (q/rect (:left box) 
      ;        (:top box) 
       ;       (- (:right box) (:left box))
        ;      (- (:bottom box) (:top box))))))
    ;(q/with-translation [(- 0 (:left box)) (- (/ (- (q/height) (- (:bottom box) (:top box))) 2) (:top box))];[0 (/ (q/height) 2)];
    ;  (q/scale (min 1 (/ (q/width) (- (:right box) (:left box)))))
    ;  (.draw (.toShape (.union (.toPolygon (:name state)) (RPolygon/createRectangle. 0 0 (q/width) (/2 (q/height)))))))))
;  ; Clear the sketch by filling it with light-grey color.
;  (q/background 240)
;  ; Set circle color.
;  (q/fill (:color state) 255 255)
;  ; Calculate x and y coordinates of the circle.
;  (let [angle (:angle state)
;        x (* 150 (q/cos angle))
;        y (* 150 (q/sin angle))]
;    ; Move origin point to the center of the sketch.
;    (q/with-translation [(/ (q/width) 2)
;                         (/ (q/height) 2)]
;      ; Draw the circle.
;      (q/ellipse x y 100 100))))

(q/defsketch badges
  :title "You spin my circle right round"
  :size (map #(* dpi %) inch_size)
  :renderer :p2d
  ; setup function called only once, during sketch initialization.
  :setup setup
;  :settings #(q/no-smooth)
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
