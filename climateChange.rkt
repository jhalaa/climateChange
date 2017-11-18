;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname climateChange) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(define BOUNDRY-CAR-LEFT 15)
(define BOUNDRY-CAR-RIGHT 35)

(define BOUNDRY-CLOUD-LEFT 15)
(define BOUNDRY-CLOUD-RIGHT 35)

(define BOUNDRY-WATER-LEFT 580)
(define BOUNDRY-WATER-RIGHT 620)

(define POSITIVE-VELOCITY 5)
(define NEGATIVE-VELOCITY -5)

;; Car is represented as a struct
;; (car Integer Integer)

;; INTERPRETATION:
;; x : Integer is the x-coordinate of the car
;; vx : Velocity in the x direction

;; CONSTRUCTOR:
(define-struct car-struct (x vx))

;; OBSERVER TEMPLATE:
(define (car-fn car)
  (cond [(car-struct-x car) ...]
        [(car-struct-vx car) ...]))

;;cars is a list of car structs which each have;
;; x position -> Int
;; y position -> Int


;; Functions ;;

;; cars-on-tick : CarList -> CarList
;; GIVEN   : a list of car
;; RETURNS : a list of car after-tick
;; DESIGN STRATEGY : use HOF map and lambda

(define (cars-on-tick cars)
  (map (lambda (car)
         (car-after-tick car))
       cars))

;; car-after-tick : Car -> Car
;; GIVEN   : a car
;; RETURNS : a car after-tick
;; DESIGN STRATEGY : call other function

(define (car-after-tick car)
  (if(car-test-boundry car)
     (make-car-struct (+ (car-struct-x car) (car-struct-vx car)) (car-struct-vx car))
     (car-negate-velocity car)))


;; test_boundry : Car -> Boolean
;; GIVEN :A Car
;; RETURNS : if the car is in the boundry condition

;; DESIGN STRATEGY: check for conditions
(define (car-test-boundry car)
  (and (> (+ (car-struct-x car) (car-struct-vx car)) BOUNDRY-CAR-LEFT) (< (+ (car-struct-x car) (car-struct-vx car)) BOUNDRY-CAR-RIGHT)))


;; negate-velocity : Car -> Car
;; GIVEN :A Car
;; RETURNS : it returns car with the x velocity negated

;; DESIGN STRATEGY:negte and send
(define (car-negate-velocity car)
  (make-car-struct (car-struct-x car) (* -1 (car-struct-vx car))))

;; TESTS for Cars

(define TEST-CAR-LEFT-BOUND  (make-car-struct 17 NEGATIVE-VELOCITY))
(define TEST-CAR-RIGHT-BOUND (make-car-struct 33 POSITIVE-VELOCITY))
(define TEST-CAR-IN-BOUND    (make-car-struct 20 POSITIVE-VELOCITY))

(define TEST-CAR-LIST (list TEST-CAR-LEFT-BOUND
                     TEST-CAR-RIGHT-BOUND
                     TEST-CAR-IN-BOUND))

(define TEST-CAR-LEFT-BOUND-AFTER-TICK  (make-car-struct 17 POSITIVE-VELOCITY))
(define TEST-CAR-RIGHT-BOUND-AFTER-TICK (make-car-struct 33 NEGATIVE-VELOCITY))
(define TEST-CAR-IN-BOUND-AFTER-TICK    (make-car-struct 25 POSITIVE-VELOCITY))

(define TEST-CAR-LIST-AFTER-TICK (list TEST-CAR-LEFT-BOUND-AFTER-TICK
                                       TEST-CAR-RIGHT-BOUND-AFTER-TICK
                                       TEST-CAR-IN-BOUND-AFTER-TICK))

(begin-for-test
  (check-equal? (cars-on-tick TEST-CAR-LIST)
                TEST-CAR-LIST-AFTER-TICK
                "the list of cars is updated incorrectly"))


;; Cloud is represented as a struct
;; (cloud Integer Integer)

;; INTERPRETATION:
;; x : Integer is the x-coordinate of the cloud
;; vx : Velocity in the x direction

;; CONSTRUCTOR:
(define-struct cloud (x vx))

;; OBSERVER TEMPLATE:
(define (cloud-fn cld)
  (cond [(cloud-x cld) ...]
        [(cloud-vx cld) ...]))

;; Clouds is a list of cloud structs which each have;
;; x position -> Int
;; y position -> Int


;; Functions ;;

;; clouds-on-tick : CloudList -> CloudList
;; GIVEN   : a list of cloud
;; RETURNS : a list of cloud after-tick
;; DESIGN STRATEGY : use HOF map and lambda

(define (clouds-on-tick clds)
  (map (lambda (cld)
         (cloud-after-tick cld))
       clds))

;; cloud-after-tick : Cloud -> Cloud
;; GIVEN   : a cloud
;; RETURNS : a cloud after-tick
;; DESIGN STRATEGY : call other function

(define (cloud-after-tick cld)
  (if(cloud-test-boundry cld)
     (make-cloud (+ (cloud-x cld) (cloud-vx cld)) (cloud-vx cld))
     (cloud-negate-velocity cld)))


;; test_boundry : Cloud -> Boolean
;; GIVEN :A Cloud
;; RETURNS : if the car is in the boundry condition

;; DESIGN STRATEGY: check for conditions
(define (cloud-test-boundry cld)
  (and (> (+ (cloud-x cld) (cloud-vx cld)) BOUNDRY-CLOUD-LEFT) (< (+ (cloud-x cld) (cloud-vx cld)) BOUNDRY-CLOUD-RIGHT)))


;; negate-velocity : Cloud -> Cloud
;; GIVEN : a cloud
;; RETURNS : it returns cloud with the x velocity negated
;; DESIGN STRATEGY:negte and send

(define (cloud-negate-velocity cld)
  (make-cloud (cloud-x cld) (* -1 (cloud-vx cld))))

;; TESTS for Clouds

(define TEST-CLOUD-LEFT-BOUND  (make-cloud 17 NEGATIVE-VELOCITY))
(define TEST-CLOUD-RIGHT-BOUND (make-cloud 33 POSITIVE-VELOCITY))
(define TEST-CLOUD-IN-BOUND    (make-cloud 20 POSITIVE-VELOCITY))

(define TEST-CLOUD-LIST (list TEST-CLOUD-LEFT-BOUND
                              TEST-CLOUD-RIGHT-BOUND
                              TEST-CLOUD-IN-BOUND))

(define TEST-CLOUD-LEFT-BOUND-AFTER-TICK  (make-cloud 17 POSITIVE-VELOCITY))
(define TEST-CLOUD-RIGHT-BOUND-AFTER-TICK (make-cloud 33 NEGATIVE-VELOCITY))
(define TEST-CLOUD-IN-BOUND-AFTER-TICK    (make-cloud 25 POSITIVE-VELOCITY))

(define TEST-CLOUDS-LIST-AFTER-TICK (list TEST-CLOUD-LEFT-BOUND-AFTER-TICK
                                         TEST-CLOUD-RIGHT-BOUND-AFTER-TICK
                                         TEST-CLOUD-IN-BOUND-AFTER-TICK))

(begin-for-test
  (check-equal? (clouds-on-tick TEST-CLOUD-LIST)
                TEST-CLOUDS-LIST-AFTER-TICK
                "the list of clouds is updated incorrectly"))


;; Water is represented as Struct
;; (Water x)

;; INTERPRETATION:
;; x : Integer is the x-coordinate of WATER_IMAGE

;; CONSTRUCTOR TEMPLATE:
(make-struct water (x))

;; water-on-tick : Water -> Water

(define (water-on-tick wtr)
  (if (= (water-x wtr) BOUNDRY-WATER-LEFT)
      (make-water BOUNDRY-WATER-RIGHT)
      (make-water BOUNDRY-WATER-LEFT)))







