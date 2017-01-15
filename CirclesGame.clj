; The the circles get deleted if they touch the left or right side without the rectangular border present.
; You can only have the left or right border present at a time.
; Press the "a" key to toggle between the sides.
; The game starts paused so press "p" to play. This is so that you have time to prepare.
; The game lasts for 30 seconds and the number of balls left on the screen is the score.




; Go to http://quil.info/sketches/show/-KaWKwkIdpBc66mG9DgF to play.
; The source code can also be seen below.




(ns circles.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))



;;; Constants
(def speed 5)                          ;maximm speed circles move
;(def starttime (q/seconds))

;---------------------------------------------------------------------
; Setuppp
;---------------------------------------------------------------------

(defn make-circle
  "Creates a circle with a random color and set speed and heading."
   [x y]
  (let [angle (rand q/TWO-PI)          ;random angle
        cur-speed (+ (rand speed) 1)]  ;random speed up to our constant
       {:x x                           ;set this circle's x
    	:y y                           ;set this circle's y
        :size (+ 10 (rand 15))         ;set random diameter
    	:color (rand 255)              ;make this colorful
    	:speed cur-speed               ;set this circle's speed
    	:heading angle}                ;set this circle's heading
    ))                                 ;returns circle

(defn setup
  "Set up a sketch and return initial state."
  []
  (q/frame-rate 30)                    ;frequency update and draw functions
  (q/color-mode :hsb)                  ;how we represent colors
  (let [size (q/width)
        n 20
        bg 250]
       (q/background bg)               ;nice light grey color for the bg
       ;; need to make n circles of random sizes
       ;; here we make only one circle in a list

       {:circles (loop [a (list) i 0]
      			(if (< i 20)(recur (conj a (make-circle (rand size) (rand size))) (inc i)) a ))
        :running? false                 ;so we can pause and unpause in update
        :n n                           ;how many circles
        :size size                     ;how big is the sketch
        :bg bg                         ;we might want to change this later
        :left/right true
        :time 30
        }))

;---------------------------------------------------------------------
; Update functions
;---------------------------------------------------------------------
(defn bounce-back [c size]
  (if (or (<= (:x c) 0) (>= (:x c) size))
    (merge c {:heading (- q/PI (:heading c))})
    (if (or (<= (:y c) 0) (>= (:y c) size))
      (merge c {:heading (- q/TWO-PI (:heading c))})
      c

      )
    )
  )
(defn move-circle [c state]
 	(let [x (:x c) y (:y c) speed (:speed c) angle (:heading c) nextx (+ x (* (q/cos angle) speed) )]
     (if (and (> nextx 0)(< nextx (:size state)))
       (bounce-back (merge c {:x (+ x (* (q/cos angle) speed) ) :y  (+ y (* (q/sin angle) speed))  }) (:size state))
       (if (and (<= nextx 0) (:left/right state))
     (bounce-back (merge c {:x (+ x (* (q/cos angle) speed) ) :y  (+ y (* (q/sin angle) speed))  }) (:size state))
       (if (and (>= nextx (:size state)) (not (:left/right state)) )
      (bounce-back (merge c {:x (+ x (* (q/cos angle) speed) ) :y  (+ y (* (q/sin angle) speed))  }) (:size state))

         )))
      ))




(defn update-circles
  "Moves each circle and returns updated vector of circles."
  [circles state]
  (map (fn [c] (move-circle c state)) circles))

(defn update-state
  "Updates sketch state. If it is paused, then the state is returned unmodified."
  [state]

  (if (and (> (:time state) 0) (:running? state))
      ;add some movement and update functions so the next line moves circles

      ;(assoc state :circles (update-circles (:circles state) state))
    ;(assoc state :circles (update-circles (:circles state) state))
      (merge state {:time (- (:time state) .03333) :circles (filter (complement nil?) (update-circles (:circles state) state))})
    (merge state {:circles (filter (complement nil?) (:circles state))})
    )
  )

;---------------------------------------------------------------------
; Draw functions
;---------------------------------------------------------------------

(defn draw-circle
  "Draws an individual circle with correct color, location, and size."
  [c]
  (q/fill (:color c) 255 255)
  (q/ellipse (:x c) (:y c) (:size c) (:size c)))

(defn draw-state
  "Draws the sketch state."
  [state]
  (q/background (:bg state))                    ;update the background
  (q/stroke 1)                                  ;how wide should the lines be
  (dorun (map draw-circle (:circles state)))    ;map is lazy
  (q/fill 0)
  (if (:left/right state) (q/rect 0 0 2 500) (q/rect 495 0 2 500))
    ;left
    ;right
  (q/rect 0 0 500 2)  ;top
  (q/rect 0 496 500 2)  ;bottom

  (q/text "Score:" 20 20)
  (q/text (count (:circles state)) 60 20)
  (if (<= (:time state) 0)
    (q/text "Game Over" 210 270)
    )
  (q/text "Time:" 20 35)
  (q/text (:time state) 60 35)
  )

;---------------------------------------------------------------------
; User interaction functions
;---------------------------------------------------------------------

(defn mouse-clicked
  "Changes background color to different shades of grey."
  [state event]
  (update-in state [:bg] (fn [n] (rand-int 255))))

(defn key-pressed
  "Process key event.  p will pause/unpause everything."
  [state event]
  (condp = (:key event)
    :p (update-in state [:running?] not)
    :a (merge state {:left/right (not (:left/right state)) :circles (filter (complement nil?) (:circles state))})
    state)

  )

(q/defsketch circles
    :host "host"
    :size [500 500]                ;we need a square canvas
    :setup setup                   ;getting things started, setting initial state
    :update update-state           ;the function to update the state
    :draw draw-state               ;the necessary draw function
    ;:mouse-clicked mouse-clicked   ;this is our mouse click event
    :key-pressed key-pressed       ;this is our keyboard input event
    :middleware [m/fun-mode])      ;this gives us the ability to have state


