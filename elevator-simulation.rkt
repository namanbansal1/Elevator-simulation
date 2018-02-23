;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname elevator-simulation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
An elevator simulation.
By Naman Bansal and Cameron Watts.

(require 2htdp/image)
(require 2htdp/universe)

;; Elevator that moves up and down depeding upon the target floor and stops on the given floor.

;; ==================
;; Constants:

(define width 300) ; Background Width
(define height 310); Background Height

(define CTR-X (/ width 2)) ; Elevator x-cordinate
(define MTS (above(rectangle width 70 "outline" "black")
                  (rectangle width 10 "outline" "black")
                  (rectangle width 70 "outline" "black")
                  (rectangle width 10 "outline" "black")
                  (rectangle width 70 "outline" "black")
                  (rectangle width 10 "outline" "black")
                  (rectangle width 70 "outline" "black"))); MTS with 4 floors

(define e-img (rectangle 50 70 "outline" "black")); Elevator Image


(define f1 35 ) ; Floor 1 y-cordinate 
(define f2 115) ; Floor 2 y-cordinate
(define f3 195) ; Floor 3 y-cordinate
(define f4 275) ; Floor 4 y-cordinate

(define SPEED 3) ; Elevator Movement Speed


;; =================
;; Data definitions:

(define-struct elevator (y dir dest))
;; Elevator is (make-elevator Natural [0, Height] Integer [-1, 1] Natural [0, 4])
;; interp. y as the elevators y-cordinate. dir as the direction of movement -1 as up, 0 as stopped and 1 as down. dest as 0 being no floor selected, 1=f1, 2=f2, 3=f3, 4=f4
;;         y is the center of the elevator
;;         y is in sceen coordinates (pixels)

(define E1 (make-elevator 17 -1 0)) ; elevator moving up at y-cordinate 17 with no destination floor
(define E2 (make-elevator 17 0 0))  ; elevator stopped at y-cordinate 17 with no destination floor
(define E3 (make-elevator 17 -1 0)) ; elevator moving down at y-cordinate 17 with no destination floor

#;
(define (fn-for-elevator e)
  (... (elevator-y e)      ;Natural [0, Height]
       (elevator-dir e)    ;Integer [-1, 1]
       (elevator-dest e))) ;Natural [0, 4]   
;; Template rules used:
;;  - compound: 3 fields



;; =================
;; Functions:

;; Elevator -> Elevator
;; start the world with ...
;; 
(define (main e)
  (big-bang e                              ; Elevator
            (on-tick   move-elevator)      ; Elevator -> Elevator
            (to-draw   render-elevator)    ; Elevator -> Image
            (on-key    control-elevator)))   ; Elevator KeyEvent -> Elevator
            
;; Elevator -> Elevator
;; increases Elevator by multiplying speed by dir; doesn't leave the screen; stops moving at dest 

(check-expect (move-elevator (make-elevator 80 1 0)) (make-elevator (+ 80 (* SPEED 1)) 1 0))
(check-expect (move-elevator (make-elevator 80 -1 0)) (make-elevator (+ 80 (* SPEED -1)) -1 0))
(check-expect (move-elevator (make-elevator 80 0 0)) (make-elevator (+ 80 (* SPEED 0)) 0 0))
(check-expect (move-elevator (make-elevator (- height f1) 1 0)) (make-elevator (- height f1) -1 0))
(check-expect (move-elevator (make-elevator (+ 0 f1) -1 0)) (make-elevator (+ 0 f1) 1 0))
(check-expect (move-elevator (make-elevator f1 1 f1)) (make-elevator f1 0 0))
(check-expect (move-elevator (make-elevator (- f1 1) 1 f1)) (make-elevator f1 0 0))
(check-expect (move-elevator (make-elevator (+ f1 1) -1 f1)) (make-elevator f1 0 0))
(check-expect (move-elevator (make-elevator f2 1 f2)) (make-elevator f2 0 0))
(check-expect (move-elevator (make-elevator (- f2 1) 1 f2)) (make-elevator f2 0 0))
(check-expect (move-elevator (make-elevator (+ f2 1) -1 f2)) (make-elevator f2 0 0))
(check-expect (move-elevator (make-elevator f3 1 f3)) (make-elevator f3 0 0))
(check-expect (move-elevator (make-elevator (- f3 1) 1 f3)) (make-elevator f3 0 0))
(check-expect (move-elevator (make-elevator (+ f3 1) -1 f3)) (make-elevator f3 0 0))
(check-expect (move-elevator (make-elevator f4 1 f4)) (make-elevator f4 0 0))
(check-expect (move-elevator (make-elevator (- f4 1) 1 f4)) (make-elevator f4 0 0))
(check-expect (move-elevator (make-elevator (+ f4 1) -1 f4)) (make-elevator f4 0 0))
;(define (move-elevator e) e) ;stub

;took template from elevator
(define (move-elevator e)
  (cond [(and (= (elevator-dest e) f1) (>= (+ (elevator-y e) (* SPEED (elevator-dir e))) (- f1 SPEED)) (<= (+ (elevator-y e) (* SPEED (elevator-dir e))) (+ f1 SPEED)))
         (make-elevator f1 0 0)]
        [(and (= (elevator-dest e) f2) (>= (+ (elevator-y e) (* SPEED (elevator-dir e))) (- f2 SPEED)) (<= (+ (elevator-y e) (* SPEED (elevator-dir e))) (+ f2 SPEED)))
         (make-elevator f2 0 0)]
        [(and (= (elevator-dest e) f3) (>= (+ (elevator-y e) (* SPEED (elevator-dir e))) (- f3 SPEED)) (<= (+ (elevator-y e) (* SPEED (elevator-dir e))) (+ f3 SPEED)))
         (make-elevator f3 0 0)]
        [(and (= (elevator-dest e) f4) (>= (+ (elevator-y e) (* SPEED (elevator-dir e))) (- f4 SPEED)) (<= (+ (elevator-y e) (* SPEED (elevator-dir e))) (+ f4 SPEED)))
         (make-elevator f4 0 0)]      
        [(> (+ (elevator-y e) (* SPEED (elevator-dir e))) (- height f1))(make-elevator (- height f1) -1 (elevator-dest e))]
        [(< (+ (elevator-y e) (* SPEED (elevator-dir e))) (+ 0 f1)) (make-elevator (+ 0 f1) 1 (elevator-dest e))]
        [else (make-elevator (+ (elevator-y e) (* SPEED (elevator-dir e))) (elevator-dir e) (elevator-dest e))]))

;; Elevator -> Image
;; render-elevator elevator image on mts at (elevator-y e) and CTR-X
(check-expect (render-elevator (make-elevator 99 1 0)) (place-image e-img CTR-X 99 MTS)) 
(check-expect (render-elevator (make-elevator 140 1 0)) (place-image e-img CTR-X 140 MTS))

;(define (render-elevator e) MTS) ;stub

(define (render-elevator e)
  (place-image e-img CTR-X (elevator-y e) MTS))   

;; Elevator KeyEvent -> Elevator
;; changes direction of movement with the up and down key. sets target floor with keys 1,2,3 and 4
(check-expect (control-elevator (make-elevator 99 1 0) "up") (make-elevator 99 -1 0))
(check-expect (control-elevator (make-elevator 99 -1 0) "down") (make-elevator 99 1 0))
(check-expect (control-elevator (make-elevator 275 1 0) "1") (make-elevator 275 1 f1)) 
(check-expect (control-elevator (make-elevator 275 1 0) "2") (make-elevator 275 1 f2))
(check-expect (control-elevator (make-elevator 275 1 0) "3") (make-elevator 275 1 f3))
(check-expect (control-elevator (make-elevator 275 1 0) "4") (make-elevator 275 1 f4)) 
;(define (control-elevator e ke) e) ;stub

;<template according to KeyEvent>

(define (control-elevator e ke)
  (cond [(key=? ke "1") (make-elevator (elevator-y e)  (elevator-dir e) f1)]
        [(key=? ke "2") (make-elevator (elevator-y e)  (elevator-dir e) f2)]
        [(key=? ke "3") (make-elevator (elevator-y e)  (elevator-dir e) f3)]
        [(key=? ke "4") (make-elevator (elevator-y e)  (elevator-dir e) f4)]
        [(key=? ke "up") (make-elevator (elevator-y e) -1 (elevator-dest e)) ]
        [(key=? ke "down") (make-elevator (elevator-y e) 1 (elevator-dest e))]
        [else e]))


