;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname climateChange) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "pictures.rkt")
(require 2htdp/universe)
(require "MouseEvents.rkt")


(define BOUNDRY-CAR-LEFT 15)
(define BOUNDRY-CAR-RIGHT 35)

(define BOUNDRY-CLOUD-LEFT 15)
(define BOUNDRY-CLOUD-RIGHT 35)

(define BOUNDRY-WATER-LEFT 580)
(define BOUNDRY-WATER-RIGHT 620)

(define POSITIVE-VELOCITY 5)
(define NEGATIVE-VELOCITY -5)

(define CAR-Y 300)
(define WATER-Y 330)
(define CLOUD-Y 70)
(define TREE-Y 310)


(define INITIAL-CLOUDS (list (make-cloud 5 5) (make-cloud 20 -5)))
(define INITIAL-TREES (list (make-tree 30) (make-tree 40)))
(define INITIAL-WATER (make-water 7))

;; World is represented as Struct
;; (world Cars Clouds Factories Trees Water)

;; INTERPRETATION:
;; cars      : CarList is cars in the world
;; clouds    : CloudList is clouds in the world
;; factories : FactoryList is factories in the world
;; trees     : TreeList is trees in the world
;; water     : Water is a water in the world

;; CONSTRUCTOR TEMPLATE:
(define-struct world (crs clds fctrs trs wtr))


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
(define-struct water (x))

;; water-on-tick : Water -> Water

(define (water-on-tick wtr)
  (if (= (water-x wtr) BOUNDRY-WATER-LEFT)
      (make-water BOUNDRY-WATER-RIGHT)
      (make-water BOUNDRY-WATER-LEFT)))



;; simulation : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds per tick (so larger numbers run slower)
;; EFFECT: runs the simulation, starting with the initial world
;; RETURNS: the final state of the world

;; EXAMPLES:
;; (simulation 1) runs in super slow motion
;; (simulation 1/24) runs at a more realistic speed
          
(define (simulation speed-of-simulation)
  (big-bang (initial-world speed-of-simulation)
            (on-tick world-after-tick)
            (on-mouse world-after-mouse-event)
            (on-draw world-to-scene)))


;; initial-world : PosReal -> World
;; GIVEN : the speed of ssimulation in seconds
;; RETURNS : the initial world

(define (initial-world speed)
  (make-world empty
              INITIAL-CLOUDS
              empty
              INITIAL-TREES
              INITIAL-WATER))


;; world-after-tick : World -> World
;; GIVEN : the World
;; RETURNS : the world after a tick
;; DESIGN STRATEGY : Use constructor template of world

(define (world-after-tick world)
  (make-world (cars-on-tick (world-crs world))
              (clouds-on-tick (world-clds world))
              (world-fctrs world)
              (world-trs world)
              (water-on-tick (world-wtr world))))


;; CONTRACT AND PURPOSE STATEMENT
;; world-to-scene : World -> Scene
;; GIVEN: a World
;; RETURNS: a Scene that portrays the given world.

;; DESIGN STRATEGY
;; Place objects of world in position

;; FUNCTION DEFINITION
(define (world-to-scene world)
  (scene-water INITIAL-WATER
               (scene-clouds INITIAL-CLOUDS
                             (scene-trees INITIAL-TREES
                                          (scene-cars INITIAL-CARS
                                                      SCENE3)))))
;; draws water

(define (scene-water water scene)
  (place-image WATER (water-x water) WATER-Y scene))

;; draws cars

(define (scene-cars cars-lst scene)
  (foldl (lambda (cr curr_scn)
           (place-image CAR (car-x cr) CAR-Y curr_scn))
         scene
         cars-lst))

;; draws clouds
  
(define (scene-clouds clouds-lst scene)
  (foldl (lambda (cld curr_scn)
           (place-image CLOUD (cloud-x cld) CLOUD-Y curr_scn))
         scene
         clouds-lst))

;; draws trees
  
(define (scene-trees trees-lst scene)
  (foldl (lambda (tr curr_scn)
           (place-image TREE (tree-x tr) TREE-Y curr_scn))
         scene
         trees-lst))


;; Tree is represented as Struct
;; (Tree x)

;; INTERPRETATION:
;; x : Integer is the x-coordinate of TREE_IMAGE

;; CONSTRUCTOR TEMPLATE:
(define-struct tree (x))

