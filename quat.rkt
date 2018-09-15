#lang racket

(provide rotate vsum norm vscale normalze vsub dot reflect)

(define (qrot a b c d)

  ;; [[(a * a + b * b - c * c - d * d), (2 * b * c - 2 * a * d), (2 * b * d + 2 * a * c)],
  ;;  [(2 * b * c + 2 * a * d), (a * a - b * b + c * c - d * d), (2 * c * d - 2 * a * b)],
  ;;  [(2 * b * d - 2 * a * c), (2 * c * d + 2 * a * b), (a * a - b * b - c * c + d * d)]]

  (let ([aa (* a a)] [ab (* a b)] [ac (* a c)] [ad (* a d)] [bb (* b b)]
        [bc (* b c)] [bd (* b d)] [cc (* c c)] [cd (* c d)] [dd (* d d)])
    `#(#(,(- (+ aa bb) cc dd)      ,(* 2 (- bc ad))      ,(* 2 (+ bd ac)))
      #(,(* 2 (+ bc ad))      ,(- (+ aa cc) bb dd)      ,(* 2 (- cd ab)))
      #(,(* 2 (- bd ac))      ,(* 2 (+ cd ab))      ,(- (+ aa dd) bb cc)))))

(define (rotate qq vec)
  (let* ([qr (apply qrot (vector->list qq))]
        [qx (vector-ref qr 0)]
        [qy (vector-ref qr 1)]
        [qz (vector-ref qr 2)])
    `#(,(dot qx vec)
       ,(dot qy vec)
       ,(dot qz vec))))

(define (reflect dd nn)
  (vsub dd (vscale (* 2 (dot dd nn))
                   (normalze nn))))

(define (normalze vv)
  (vector-map (lambda (zz) (/ zz (norm vv)))
              vv))

(define (norm va)
  (sqrt (dot va va)))

(define (dot va vb)
  (+ (* (vector-ref va 0) (vector-ref vb 0))
     (* (vector-ref va 1) (vector-ref vb 1))
     (* (vector-ref va 2) (vector-ref vb 2))))

(define (vsum va vb)
  `#(,(+ (vector-ref va 0) (vector-ref vb 0))
     ,(+ (vector-ref va 1) (vector-ref vb 1))
     ,(+ (vector-ref va 2) (vector-ref vb 2))))

(define (vsub va vb)
  `#(,(- (vector-ref va 0) (vector-ref vb 0))
     ,(- (vector-ref va 1) (vector-ref vb 1))
     ,(- (vector-ref va 2) (vector-ref vb 2))))

(define (vscale s vv)
  (vector-map (lambda (xx) (* s xx))
              vv))

(define myq `#(,(- 1 (* 0.1 0.1)) ,(* 0.1 0.1) 0 0))

(define myv #(0.0 1.0 0.0))

(println myq)
(println (apply qrot (vector->list myq)))
(println (rotate myq myv))
