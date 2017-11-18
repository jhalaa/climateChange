;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname world-after-mouse-event) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide
 world-after-mouse-event)

;; CONSTANTS

(define BUTTON1H 50)
(define BUTTON1W 50)
(define BUTTON1X 100)
(define BUTTON1Y 100)

(define BUTTON2H 50)
(define BUTTON2W 50)
(define BUTTON2X 300)
(define BUTTON2Y 100)

(define BUTTON3H 50)
(define BUTTON3W 50)
(define BUTTON3X 500)
(define BUTTON3Y 100)

(define MOUSE-UP "button-up")
(define MOUSE-DOWN "button-down")

;;FUNCTIONS:

;; world-after-mouse-event: World MouseEvent Int Int -> World
;; GIVEN: A world, a mouse event and its coordinates
;; RETURNS: A world after the given mouse event
;; DESIGN STRATEGY: Divide into cases

(define (world-after-mouse-event w mev x y)
  (cond
    [(and (< (- BUTTON1X (/ BUTTON1W 2)) x (+ BUTTON1X (/ BUTTON1W 2)))
          (< (- BUTTON1Y (/ BUTTON1H 2)) y (+ BUTTON1Y (/ BUTTON1H 2)))
          (mouse=? mev MOUSE-UP))
     (world-after-b1-press w)]
    [(and (< (- BUTTON2X (/ BUTTON2W 2)) x (+ BUTTON2X (/ BUTTON2W 2)))
          (< (- BUTTON2Y (/ BUTTON2H 2)) y (+ BUTTON2Y (/ BUTTON2H 2)))
          (mouse=? mev MOUSE-UP))
     (world-after-b2-press w)]
    [(and (< (- BUTTON3X (/ BUTTON3W 2)) x (+ BUTTON3X (/ BUTTON3W 2)))
          (< (- BUTTON3Y (/ BUTTON3H 2)) y (+ BUTTON3Y (/ BUTTON3H 2)))
          (mouse=? mev MOUSE-UP))
     (world-after-b3-press w)]
    [else w]))

;; world-after-b1-press: World -> World
;; GIVEN: A world
;; RETURNS: A world after button1 is pressed in the given world
;; DESIGN STRATEGY: Use Constructor template on World

(define (world-after-b1-press w)
  (make-world (cars-after-tick (world-cars w))
              (world-trees w)
              (world-factories w)
              (world-state w)))

;; world-after-b2-press: World -> World
;; GIVEN: A world
;; RETURNS: A world after button2 is pressed in the given world
;; DESIGN STRATEGY: Use Constructor template on World

(define (world-after-b2-press w)
  (make-world (world-cars w)
              (trees-after-tick (world-trees w))
              (world-factories w)
              (world-state w)))

;; world-after-b3-press: World -> World
;; GIVEN: A world
;; RETURNS: A world after button3 is pressed in the given world
;; DESIGN STRATEGY: Use Constructor template on World

(define (world-after-b3-press w)
  (make-world (world-cars w)
              (world-trees w)
              (factories-after-tick (world-factories w))
              (world-state w)))