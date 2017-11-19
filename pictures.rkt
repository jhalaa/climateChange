;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pictures) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require 2htdp/image) ; draw a picture
(provide SCENE
         TREE
         CLOUD
         SUN
         WATER-BLUE
         WATER-CFBLUE
         WATER-PURPLE
         new-car
         CAR
         FACTORY
         FRAME
         CLOUD-G)

;;; SUN size : 87 * 100
(define SUN (let ([petal (put-pinhole
                5 50
                (isosceles-triangle 20 30 "solid" 'yellow))])
    (clear-pinhole
     (overlay/pinhole
      (circle 20 "solid" "yellow")
      (rotate (* 60 0) petal)
      (rotate (* 60 1) petal)
      (rotate (* 60 2) petal)
      (rotate (* 60 3) petal)
      (rotate (* 60 4) petal)
      (rotate (* 60 5) petal)))))

;;; MOUNTAINS
(define MOUNTAIN1 (overlay/align
 "middle" "top"
 (isosceles-triangle 120 110 "solid" 'white)
 (isosceles-triangle 600 110 "solid" 'LimeGreen)))

(define MOUNTAIN2 (overlay/align
 "middle" "top"
 (isosceles-triangle 80 80 "solid" 'white)
 (isosceles-triangle 600 80 "solid" 'ForestGreen)))

(define MOUNTAIN3
 (isosceles-triangle 600 120 "solid" 'DarkOliveGreen))

;;; SCENE is a skyblue bacjground with grey frame
;;; size : 640 * 360
;;; Useful size: 620 * 290
;;; Button dashboard size: 620 * 50
(define SCENE0 (overlay (above (rectangle 620 50 'solid 'Silver)
                (place-image
                 (scale 0.8 SUN)
                 400 50
                 (place-image
                 MOUNTAIN1
                 300 230
                 (place-image
                  MOUNTAIN2
                  500 250
                  (place-image
                   MOUNTAIN3
                   120 250
                   (rectangle 620 290 'solid 'DeepSkyBlue))))))
         (rectangle 640 360 'solid 'Silver)))

(define SCENE (overlay/align/offset "left" "bottom" (crop 360 0 360 50 (ellipse 720 100 "solid" 'DarkGoldenrod))
                                     -10 10
                               SCENE0))

;;; BUTTON is just a black square outline
;;; BUTTON is 40 * 40
(define BUTTON 
                (overlay 
                (rectangle 40 40 'outline 'black)
                (add-line
                 (add-line
                (rectangle 40 40 'solid 'DarkGray)
                0 0 0 37.5
                (make-pen "LemonChiffon" 5 "solid" "round" "round"))
                0 0 37.5 0
                (make-pen "LemonChiffon" 5 "solid" "round" "round"))))

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


;;; CLOUD
(define cld (circle 20 'solid 'WhiteSmoke))
(define clds (beside cld cld))
(define CLOUD (overlay/offset (overlay/offset cld 0 20 cld) 0 0 clds))

(define cld-g (circle 20 'solid 'DarkGray))
(define cldgs (beside cld-g cld-g))
(define CLOUD-G (overlay/offset (overlay/offset cld-g 0 20 cld-g) 0 0 cldgs))



;;; CAR :size : 60 * 40
;;; as (new-car 0)

;;; random-color : int -> Radndom Color
(define (random-color _)
  (make-color (random 255) (random 255) (random 255)))

(define (new-car _)
  (let ([main-color (random-color 0)])
    (above (overlay/align
            "center" "center"
            (underlay/offset (rectangle 15 10 "solid" 'LightCyan)
                             -20 0
                             (rectangle 15 10 "solid" 'LightCyan))
            (rectangle 40 20 'solid main-color))
           (overlay/offset
            (underlay/offset (circle 8 "solid" 'DimGray)
                             -30 0
                             (circle 8 "solid" 'DimGray))
            0 -5
            (rectangle 60 15 'solid main-color)))))
(define CAR (new-car 0))

;;; WATER
(define (water color)
  (let ([wave (triangle 100 "solid" color)])
    (local ((define (draw/a water/a t)
            (if (= t 0)
                water/a
                (draw/a (overlay/align/offset
                         "right" "bottom"
                         water/a
                         40 0
                         wave)
                        (sub1 t)))))
      (draw/a wave 10))))
(define WATER-BLUE (water "blue"))
(define WATER-CFBLUE (water "CornflowerBlue"))
(define WATER-PURPLE (water "Indigo"))

;;; FACTORY
(define FACTORY (overlay/align/offset
 "right" "top"
 (rectangle 20 40 'solid 'DimGray)
 25 40
 (overlay/align/offset
 "middle" "bottom"
 (rectangle 20 30 'solid 'black)
 20 0
 (rectangle 120 50 'solid 'DimGray))))


(define BUTTON1 (overlay (rectangle 40 40 'outline 'black)
                         (place-image/align
                 (overlay (scale 0.53 CAR) BUTTON)
                 40 40 "right" "bottom"
                 (empty-scene 40 40))))
(define BUTTON2 (overlay (rectangle 40 40 'outline 'black)
                         (place-image/align
                 (overlay (scale 0.43 TREE) BUTTON)
                 40 40 "right" "bottom"
                 (empty-scene 40 40))))
(define BUTTON3 (overlay (rectangle 40 40 'outline 'black)
                         (place-image/align
                 (overlay (scale 0.27 FACTORY) BUTTON)
                 40 40 "right" "bottom"
                 (empty-scene 40 40))))



;;; FRAME
(define FRAME0
  (above
 (rectangle 640 60 'solid "Silver")
 (overlay/offset (rectangle 10 290 'solid "Silver")
                630 0
                (rectangle 10 290 'solid "Silver"))
 (rectangle 640 10 'solid "Silver")))

;;; A scene with 3 buttons
(define FRAME01 (place-images (list BUTTON1 BUTTON2 BUTTON3)
              (list BUTTON1-POS BUTTON2-POS BUTTON3-POS)
              FRAME0))

(define FRAME FRAME01)