#lang racket

(provide final-file-contents pix)
(require "quat.rkt")
(require rnrs/arithmetic/flonums-6)

(define IW 1024)
(define IH 798)
(define oc `#(,(+ 0.5 (/ IW -2)) ,(+ 0.5 (/ IH -2)) ,(/ IW 2)))
(define tilt 0.6)

(define sky-color #(63 126 226))
(define plane-white #(255 255 255))
(define plane-black #(0 0 0))
(define error-color #(255 0 0))

(define oo1 #(0 -1.5 -0.6))
(define oo2 #(1.2 -1.5 -0.6))
(define oo3 #(-1.2 -1.5 -0.6))
(define rr 0.6)

(define (pix)
  (for*/list ([j (range IH)]
             [k (range IW)])
    (find-pixel-color `#(,j ,k))))

(define (find-pixel-color pix)
  (find-ray-color (vec-from-pixel pix)))

(define (vec-from-pixel jk)
  (let* ([cw #(0 0 -2.0)]
         [qq `#(,(- 1 (* tilt tilt)) ,(* tilt tilt) 0 0)]
         [vv (rotate qq
                     (vsum `#(,(vector-ref jk 1) ,(vector-ref jk 0) 0)
                           oc))])
    (make-ray cw vv)))

(define (make-ray cw vv) (hash 'orig cw 'dir vv))

(define (find-ray-color ray)
  (let* ([tests (test-ray ray)]
         [valid-tests (filter (lambda (hh) (not (void? (hash-ref hh 'dist)))) (vector->list tests))]
         [the-hit ((smallest-head? valid-tests))])
     (if (void? the-hit)
        sky-color
        the-hit)))

(define (test-ray ray)
  `#(,(hit-sphere? ray oo1)
     ,(hit-sphere? ray oo2)
     ,(hit-sphere? ray oo3)
     ,(hit-plane? ray)))

(define (hit-sky) sky-color)

(define (min-by-key k a b)
  (if (< (hash-ref a k) (hash-ref b k))
      a
      b))

(define (reduce func ll) (foldl func (car ll) (cdr ll)))

(define (min-key k yaya)
  (reduce ((curry min-by-key) k) yaya))

(define (smallest-head? pairs)
  (if (empty? pairs)
      (void)
      (hash-ref (min-key 'dist pairs) 'func)))

(define (int x) (inexact->exact (floor x)))


(define (plane-texture ray t)
  (if (< t 0)
      (void)
      (let ([px (+ (vector-ref (hash-ref ray 'orig) 0) (* t (vector-ref (hash-ref ray 'dir) 0)))]
            [py (+ (vector-ref (hash-ref ray 'orig) 1) (* t (vector-ref (hash-ref ray 'dir) 1)))])
        (if (> (norm `#(,px ,py 0)) 50)
            (void)
            (if (or (and (< (flmod px 0.4) 0.2) (< (flmod py 0.4) 0.2))
                    (and (> (flmod px 0.4) 0.2) (> (flmod py 0.4) 0.2)))
                (vector-map int (vscale (/ 0.001 (* t t)) plane-white))
                (vector-map int (vscale (/ 0.001 (* t t)) plane-black)))))))

(define (hit-plane? ray)
  (let* ([t (/ (* -1 (vector-ref (hash-ref ray 'orig) 2))
               (vector-ref (hash-ref ray 'dir) 2))]
         [yaya (plane-texture ray t)])
    (if yaya
        (hash 'dist 1000 'func (lambda () yaya))
        (void))))

(define (touch-sphere? ray oo)
  (let* ([ll (normalze (hash-ref ray 'dir))]
         [oc (vsub (hash-ref ray 'orig) oo)]
         [bb (dot ll oc)]
         [cc (- (dot oc oc) (* rr rr))]
         [delt (- (* bb bb) cc)]
         [dd (- (* -1 bb) (sqrt delt))])
    (if (and (> delt 0)
             (> 0 (+ (vector-ref (hash-ref ray 'orig) 2) (* (vector-ref ll 2) dd)))
             (< 0.01 dd))
        dd
        (void))))

(define (trace-reflection ray dd oo)
  (let* ([newcw (vsum (hash-ref ray 'orig) (vscale dd (normalze (hash-ref ray 'dir))))]
         [nn (vsub newcw oo)])
    (find-ray-color (make-ray newcw (reflect (hash-ref ray 'dir) nn)))))

(define (hit-sphere? ray oo)
  (let ([dd (touch-sphere? ray oo)])
    (if dd
        ;; [dd (fn [] (trace-reflection cw dd vv oo))]
        (hash 'dist dd 'func (lambda () (trace-reflection ray dd oo)))
        (void))))

(define (joined-values vv)
  `(,(string-join vv " ")))

(define p6-header `("P3" ,(string-append (number->string IW) " " (number->string IH)) "255"))

(define aaa
  (for*/list ([a (pix)]
             [b a])
    (number->string b) ))

(define final-file-rows
  (append p6-header (joined-values aaa) '() ))

(define final-file-contents
  (string-join final-file-rows "\n" #:after-last "\n"))

(with-output-to-file "spheres.pnm" #:exists 'replace
  (lambda () (display final-file-contents)))
