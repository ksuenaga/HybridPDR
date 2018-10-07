;; Pre:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;; ------
;; CEX:(or (and (= y (- (/ 5433.0 3125.0))) (= x (/ 990679.0 1000000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (= y 0.0) (>= 0.0 x) (<= -0.70710678118 x)))

;; Pre:(and (<= x 1.0)
;;      (or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;          (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Continuous:true
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 5433.0 3125.0))) (= x (/ 990679.0 1000000.0))) false)
(and (= y 0.0) (<= x 0.70710678118) (>= x 0.0))

;; Pre:(and (<= x 1.0)
;;      (or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;          (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x)))
;;      (= y 0.0)
;;      (<= x (/ 707107.0 1000000.0))
;;      (>= x 0.0))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Guard:(>= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 526683.0 500000.0))) (= x (/ 495299.0 500000.0))) false)
(and (= y 0.0) (<= x 0.70710678118) (>= x 0.0))

;; Pre:(let ((a!1 (and (or (and (>= x 0.0)
;;                          (<= x (/ 1.0 2.0))
;;                          (>= y 0.0)
;;                          (<= y (/ 1.0 2.0)))
;;                     (<= x 1.0))
;;                 (= y 0.0)
;;                 (<= x (/ 707107.0 1000000.0))
;;                 (>= x 0.0))))
;;   (or (and (<= y (/ 1.0 2.0)) (<= x (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0)) a!1))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (<= y (/ 1.0 2.0)) (<= x (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0))
;; ------
;; CEX:(or (and (= y (- (/ 869559.0 500000.0))) (= x (/ 197599.0 200000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (= y 0.0) (>= 0.0 x) (<= -0.70710678118 x)))

;; Pre:(and (<= x 1.0)
;;      (or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;          (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Continuous:true
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 869559.0 500000.0))) (= x (/ 197599.0 200000.0))) false)
(and (= y 0.0) (<= x 0.70710678118) (>= x 0.0))

;; Pre:(and (<= x 1.0)
;;      (or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;          (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x)))
;;      (= y 0.0)
;;      (<= x (/ 707107.0 1000000.0))
;;      (>= x 0.0))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Guard:(>= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 901687.0 500000.0))) (= x (/ 982597.0 1000000.0))) false)
(and (= y 0) (>= x 0) (<= x 0.5))

;; Pre:(let ((a!1 (and (<= x (/ 1.0 2.0)) (<= y (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0))))
;;   (or a!1 (and (or a!1 (<= x 1.0)) (= y 0.0) (>= x 0.0) (<= x (/ 1.0 2.0)))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(and (<= x (/ 1.0 2.0)) (<= y (/ 1.0 2.0)) (>= x 0.0) (>= y 0.0))
;; ------
;; CEX:(or (and (= y (- (/ 901687.0 500000.0))) (= x (/ 982597.0 1000000.0))) false)
(or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
    (and (= y 0.0) (>= 0.0 x) (<= -0.70710678118 x)))

;; Pre:(and (<= x 1.0)
;;      (or (and (>= x 0.0) (<= x (/ 1.0 2.0)) (>= y 0.0) (<= y (/ 1.0 2.0)))
;;          (and (= y 0.0) (>= 0.0 x) (<= (- (/ 707107.0 1000000.0)) x))))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Continuous:true
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 901687.0 500000.0))) (= x (/ 982597.0 1000000.0))) false)
(and (= y 0.0) (<= x 0.70710678118) (>= x 0.0))
