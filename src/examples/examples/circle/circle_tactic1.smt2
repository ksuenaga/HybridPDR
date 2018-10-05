; Tactics for circle.xml with the initial condition x <= 0.5 and safety region x <= 1.0

;; Pre:(<= x (/ 1.0 2.0))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(<= x (/ 1.0 2.0))
;; ------
;; CEX:(or (and (= y (- (/ 5433.0 3125.0))) (= x (/ 990679.0 1000000.0))) false)

(<= x (/ 1.0 2.0))

;; Pre:(and (<= x 1.0) (<= x (/ 1.0 2.0)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Guard:(>= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 377873.0 200000.0))) (= x (/ 249629.0 250000.0))) false)

(>= y 0.0)

;; Pre:(or (<= x (/ 1.0 2.0)) (and (or (<= x (/ 1.0 2.0)) (<= x 1.0)) (>= y 0.0)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(<= x (/ 1.0 2.0))
;; ------
;; CEX:(or (and (= y (- (/ 377873.0 200000.0))) (= x (/ 249629.0 250000.0))) false)
(<= x (/ 1.0 2.0))

;; Pre:(and (<= x 1.0) (<= x (/ 1.0 2.0)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(<= y 0.0)
;; Guard:(>= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:false
;; ------
;; CEX:(or (and (= y (- (/ 201261.0 200000.0))) (= x (/ 199819.0 200000.0))) false)
(>= y 0.0)

;; Pre:(or (<= x (/ 1.0 2.0)) (and (or (<= x (/ 1.0 2.0)) (<= x 1.0)) (>= y 0.0)))
;; Flow:[(y, x); (x, (* y (- 1.0)))]
;; Inv:(>= y 0.0)
;; Guard:(<= y 0.0)
;; Command:[]
;; Continuous:false
;; Init:(<= x (/ 1.0 2.0))
;; ------
;; CEX:(or (and (= y (- (/ 201261.0 200000.0))) (= x (/ 199819.0 200000.0))) false)
(<= x (/ 1.0 2.0))
