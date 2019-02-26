(* (+ 22 42) (* 54 99))

(* (* (+ 22 42) 54) 99)

(+ (* 64 102) (* 16 (/ 44 22)))

(+ (+ (+ 12 144) 20) (* 3 (sqrt 4)))

(= (+ (/ (+ (+ (+ 12 144) 20) (* 3 (sqrt 4))) 7) (* 5 11)) 81)

;;The difference between the two is the order some of the operations take place. Thus, the SCHEME expression would show, just like on paper, the change of some parenthesis.
;;Since SCHEME uses prefix notation with it's parenthesis, it is up to the user to use the rules of precidence in his code. I.e making sure he understand how SCHEME evaluates its expressions.

(define (cube x) (* (* x x ) x))

(define (^4 x) (* (* x x) (* x x)))

(define (^5 x) (* (^4 x) x))

(define (p x)  (cube (+ (- (+ (+ (^5 x) (* 11 (^4 x))) (* 24 (cube x))) x) 21)))

(define (tenth x) (* (cube (cube x)) x))

(define (hundredth x) (tenth (tenth x)))

;I can calculate the answer on a calculator and see if my function gives the same value using the same number.
; The difficulty would immediatly lie in the amount of code that would have to be produced if you only use *. Think of all those parenthesis too!

(define (y-value x b m) (+ (* m x ) b))

(define (points-slope x1 y1 x2 y2) (/ (- y2 y1) (- x2 x1)))

(define (points-intercept x1 y1 x2 y2) (- y1 (* (points-slope x1 y1 x2 y2) x1)))

(define (on-parallels? x1 y1 x2 y2 x3 y3 x4 y4) (cond
                                                      ((or (= (- x2 x1) 0 )(= (- x4 x3) 0 )) (and (= (- x2 x1) 0 )(= (- x4 x3) 0 )))
                                                      ((or (= (- y2 y1) 0 )(= (- y4 y3) 0 )) (and (= (- y2 y1) 0 )(= (- y4 y3) 0 )))
                                                      
                                                    ((= (points-slope x1 y1 x2 y2) (points-slope x3 y3 x4 y4)) #t)
                                                    (#f) 
                                                    ))

(define (root1 a b c) (/ (+ (* b -1) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))

(define (root2 a b c) (/ (- (* b -1) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))

(define  (number-of-roots a b c) (if (= (root1 a b c) (root2 a b c))
                                     1
                                     2))

(define (real-roots? a b c) (if (or (> (- (* b b) (* 4 a c)) 0) (= (- (* b b) (* 4 a c)) 0))
                                #t
                                #f))