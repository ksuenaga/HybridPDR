;; x>>> 
;; Transition: "1" --> "2"
;; Give an interpolant for the following problem:
;; Pre:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; ------
;; CEX:(or (and (= y (- (/ 5433.0 3125.0))) (= x (/ 990679.0 1000000.0))) false)
;; >>>

(>= y 0.0)

;; Transition: "1" --> "2"
;; Give an interpolant for the following problem:
;; Pre:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; ------
;; CEX:(or (and (= y (/ 6721.0 1000000.0)) (= x (- (/ 2000991.0 1000000.0)))) false)
;; >>>

(>= x -0.70710678118)

;; >>> 
;; Transition: "1" --> "2"
;; Give an interpolant for the following problem:
;; Pre:(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;     (<= x 1.0))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:true
;; Init:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; ------
;; CEX:(or (and (= y (- (/ 5433.0 3125.0))) (= x (/ 990679.0 1000000.0))) false)
;; >>> 

(>= y 0.0)

;; >>> 
;; Transition: "1" --> "2"
;; Give an interpolant for the following problem:
;; Pre:(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;     (<= x 1.0))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (<= x (/ 1.0 2.0)) (<= y (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0))
;; ------
;; CEX:(or (and (= y (/ 1573.0 125000.0)) (= x (- (/ 1339907.0 500000.0)))) false)
;; >>>

(or (>= x 0.0) (<= y 0.0))