#lang racket
(require 2htdp/universe 2htdp/image)

; Constants

#| put WIDTH variable here |#
(define HEIGHT 200)
(define ENDGAME-TEXT-SIZE 12)
(define BIRD (circle 10 "solid" "red"))
(define random-height (random (- HEIGHT 100)))
(define UP-PIPE (rectangle 50 random-height "solid" "green"))
(define DOWN-PIPE (rectangle 50 (+ 50 (- (- HEIGHT 100) random-height)) "solid" "blue"))
(define GROUND (- HEIGHT 10))
(define SKY 10)
(define bird-x (/ (image-width BIRD) 2))
(define bird-width (image-width BIRD))

;; contains the changing variables for the game-state
(struct game-state (bird-pos pillar-pos jump-count))

;; updates the variables for the state
(define (update state)
  (game-state (add1 (game-state-bird-pos state))
        (sub1 (game-state-pillar-pos state))
        (game-state-jump-count state)))

;; draws the state onto the screen
(define (render state)
  (place-image BIRD  (+ bird-x bird-x) (game-state-bird-pos state) (draw-pillars state)))

;; draws the pillars onto the screen
(define (draw-pillars state)
  (place-image/align UP-PIPE (game-state-pillar-pos state) 0 "left" "top"
                  (place-image/align DOWN-PIPE (game-state-pillar-pos state) HEIGHT "left" "bottom" (empty-scene 120 HEIGHT))))

;; determines if a key has been pressed and move the bird up by 20
(define (key-pressed state key)
  (define jump-count (game-state-jump-count state))
  (cond
    [(key=? key "up") (game-state (- (game-state-bird-pos state) 20)
        (game-state-pillar-pos state)  #| increase jump count here |# jump-count)]
    [(key=? key " ") (game-state (- (game-state-bird-pos state) 20)
        (game-state-pillar-pos state) #| increase jump count here |# jump-count)]
    ;;;; Add another button here
    [else state]))

;; renders the game-over screen
(define (render-end state)
 (overlay (text (string-append "Jumps: " (number->string (game-state-jump-count state))) ENDGAME-TEXT-SIZE "black") (empty-scene 120 HEIGHT)))

;; determines if the game should be ended
(define (end? state)
  (or (hit-ground state)
      (collision? state)
      #| call hit-sky function here |#
      ))

;; determines if the bird has hit the ground
(define (hit-ground state)
  (>= (game-state-bird-pos state) GROUND))

;; determine if the bird hit the top

#| Put hit-sky function here |#

;; determies if a collisions has occured or not
(define (collision? state)
  (define pipe-x-min (game-state-pillar-pos state))
  (define pipe-x-max (+ pipe-x-min 50))
  (define pipe-y-min random-height)
  (define pipe-y-max (- HEIGHT (+ 50 (- (- HEIGHT 100) random-height))))
  (define y (game-state-bird-pos state))
    (and (> (+ bird-x bird-width) pipe-x-min)
       (< bird-x pipe-x-max)
       (or (< y pipe-y-min) (> y pipe-y-max))))
  


;; Main Program -- starts the whole game
(define (start-game)
  (big-bang (game-state (/ HEIGHT 2) 120 0)
    [on-tick update]
    [to-draw render]
    [on-key key-pressed]
    [stop-when end? render-end]
    ))