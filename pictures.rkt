;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pictures) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require 2htdp/image) ; draw a picture
(provide SCENE3
         TREE
         CLOUD)

;;; SCENE is a skyblue bacjground with grey frame
;;; size : 640 * 360
;;; Useful size: 620 * 290
;;; Button dashboard size: 620 * 50
(define SCENE (overlay (above (rectangle 620 50 'solid 'Silver)
                (rectangle 620 290 'solid 'DeepSkyBlue))
         (rectangle 640 360 'solid 'Silver)))
;;; BUTTON is just a black square outline
;;; BUTTON is 40 * 40
(define BUTTON (rectangle 40 40 'outline 'black))

;;; store positions of buttons
(define BUTTON1-POS (make-posn 40 30))
(define BUTTON2-POS (make-posn 90 30))
(define BUTTON3-POS (make-posn 140 30))

;;; A scene with 3 buttons
(define SCENE3 (place-images (list BUTTON BUTTON BUTTON)
              (list BUTTON1-POS BUTTON2-POS BUTTON3-POS)
              SCENE))

;;; TREE size : 60 * 80
(define BRANCH (rectangle 15 50 'solid 'brown))
(define LEAVES (circle 30 'solid 'green))
(define TREE (overlay/offset BRANCH 0 -25 LEAVES))

(define cld (circle 20 'solid 'white))
(define clds (beside cld cld))

(define CLOUD (overlay/offset (overlay/offset cld 0 20 cld) 0 0 clds))


