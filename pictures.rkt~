;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Untitled) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image) ; draw a picture
(provide SCENE3)

;;; SCENE is a skyblue bacjground with grey frame
(define SCENE (overlay (above (rectangle 620 50 'solid 'Silver)
                (rectangle 620 290 'solid 'DeepSkyBlue))
         (rectangle 640 360 'solid 'Silver)))
;;; BUTTON is just a black square outline
(define BUTTON (rectangle 40 40 'outline 'black))

;;; store positions of buttons
(define BUTTON1-POS (make-posn 40 30))
(define BUTTON2-POS (make-posn 90 30))
(define BUTTON3-POS (make-posn 140 30))

;;; 
(define SCENE3 (place-images (list BUTTON BUTTON BUTTON)
              (list BUTTON1-POS BUTTON2-POS BUTTON3-POS)
              SCENE))