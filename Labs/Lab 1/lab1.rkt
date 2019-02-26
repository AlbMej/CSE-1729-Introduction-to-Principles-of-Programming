(define (usd-gbp dollars) ( * dollars 0.772))

(define (gbp-eur pounds) ( * pounds 1.092))

(define (eur-sek euros) ( * euros 9.485))

(define (usd-sek dollars )(eur-sek(gbp-eur(usd-gbp dollars))))

(define (det2x2 a b c d) (- (* a d) ( * b c)))

(define (invertible? a b c d) 
  (if (= (- (* a d) ( * b c)) 0)
                                  #f
                                  #t
                                ) )

(define (prod-inv-direct? a1 b1 c1 d1 a2 b2 c2 d2)
  (invertible? (+ (* a1 a2) (* b1 c2)) (+ (* a1 b2) (* b1 d2)) (+ (* c1 a2) (* d1 c2)) (+ (* c1 b2) (* d1 d2))
               ))
(define (prod-inv-indirect? a1 b1 c1 d1 a2 b2 c2 d2)
  (if (= (* (det2x2 a1 b1 c1 d1) (det2x2 a2 b2 c2 d2)) 0)
           #f
           #t
  ))
(define (det3x3 a b c d e f g h i)
( + (- (* a (det2x2 e f h i)) (* b (det2x2 d f g i))) (* c (det2x2 d e g h))
))

