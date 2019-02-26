(define (pi) (* 6 (asin 1/2)))

(define (area-of-circle r) (* (pi) r r))

(define (surface-area-of-sphere r) (* 4 (area-of-circle r)))

(define (volume-of-sphere r) (* (/ (surface-area-of-sphere r) 3) r))

(define (s n) (cond
                ((= n 1) 1)
                ((= n 2) 2)
                ((= n 3) 3)
                ((> n 3) (+ (- (s (- n 3)) (s (- n 2))) (s (- n 1))))))

(define (zeno n) (if (= n 1)
                     1/2
                     (+ (/ 1 (expt 2 n)) (zeno (- n 1)))))



(define (even-nn-int? n) (cond
                           ((= n 0) #t)
                           ((= n 1) #f)
                           ((> n 1) (even-nn-int? (- n 2)))))



(define (even-int? n) (even-nn-int? (abs n)))

(define (odd-nn-int? n) (cond
                           ((= n 0) #f)
                           ((= n 1) #t)
                           ((> n 1) (odd-nn-int? (- n 2)))))

(define (odd-int? n) (odd-nn-int? (abs n)))

                   

                