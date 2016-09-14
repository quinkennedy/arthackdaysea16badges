(ns badges.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [badges.dynamic :as dynamic]
            [badges.util :as util]))

(q/defsketch badges
  :title "Subtractive blended badges"
  :size (map * 
             util/inch-size 
             (repeat util/screen-dpi))
  :renderer :p2d
  ; setup function called only once, during sketch initialization.
  :setup dynamic/setup
;  :settings #(q/no-smooth)
  ; update-state is called on each iteration before draw-state.
  :update dynamic/update-state
  :key-typed dynamic/key-typed
  :draw dynamic/draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
