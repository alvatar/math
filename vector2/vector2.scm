;;; Copyright (c) 2012-2019 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vector (dimension 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;! vector2 type
(define-structure vector2 x y)

;;! Vector addition
(define (vector2+ v1 v2)
  (make-vector2 (+ (vector2-x v1)
                   (vector2-x v2))
                (+ (vector2-y v1)
                   (vector2-y v2))))

;;! Vector substraction
(define (vector2- v1 v2)
  (make-vector2 (- (vector2-x v1)
                   (vector2-x v2))
                (- (vector2-y v1)
                   (vector2-y v2))))

;;! Vector dot product
(define (vector2* v1 v2)
  (make-vector2 (* (vector2-x v1)
                   (vector2-x v2))
                (* (vector2-y v1)
                   (vector2-y v2))))

;;! Vector cross product
(define (vector2x v1 v2)
  (- (* (vector2-x v1)
        (vector2-y v2))
     (* (vector2-y v1)
        (vector2-x v2))))

;;! Vector * scalar
(define (vector2*scalar v a)
  (make-vector2 (* (vector2-x v) a)
                (* (vector2-y v) a)))

;;! Vector / scalar
(define (vector2/scalar v a)
  (make-vector2 (/ (vector2-x v) a)
                (/ (vector2-y v) a)))

;;! Are these vectors equal? (with epsilon)
(define (vector2=? v1 v2)
  (and (= (vector2-x v1)
          (vector2-x v2))
       (= (vector2-y v1)
          (vector2-y v2))))

;;! Are these vectors proportional?
(define (vector2.proportional? v1 v2)
  (let ((v1x (vector2-x v1)) (v1y (vector2-y v1))
        (v2x (vector2-x v2)) (v2y (vector2-y v2)))
    (cond ((zero? v2x) (zero? v1x))
          ((zero? v2y) (zero? v1y))
          (else (= (/ v1x v2x)
                   (/ v1y v2y))))))

;;! Exact conversion
(define (vector2.inexact->exact v)
  (make-vector2 (inexact->exact (vector2-x v))
                (inexact->exact (vector2-y v))))

;;! Zero vector
(define (vector2.zero)
  (make-vector2 0 0))

(define (vector2.zero? v)
  (and (zero? (vector2-x v))
       (zero? (vector2-y v))))

;;! Random exact vector (range -1 -> 1)
(define (vector2.random)
  (make-vector2 (+ -1 (* (inexact->exact (random-real)) 2))
                (+ -1 (* (inexact->exact (random-real)) 2))))

;;! Calculate squared vector length
(define (vector2.squaredmagnitude vec)
  (+ (square (vector2-x vec))
     (square (vector2-y vec))))

;;! Calculate the symmetric vector
(define (vector2.symmetric vec)
  (make-vector2 (- (vector2-x vec))
                (- (vector2-y vec))))

;;! Calculate x projection
(define (vector2.x-projection vec)
  (make-vector2 (vector2-x vec)
                0))

;;! Calculate y projection
(define (vector2.y-projection vec)
  (make-vector2 0
                (vector2-y vec)))

;;! Get the component that is max
(define (vector2.max-component vec)
  (max (vector2-x vec)
       (vector2-y vec)))

;;! Get the component that is min
(define (vector2.min-component vec)
  (min (vector2-x vec)
       (vector2-y vec)))

;;! Calculate x/y ratio
(define (vector2.x/y vec)
  (/ (vector2-x vec)
     (vector2-y vec)))

;;! Calculate y/x ratio
(define (vector2.y/x vec)
  (/ (vector2-y vec)
     (vector2-x vec)))

;;! Absolute vector
(define (vector2.abs vec)
  (make-vector2 (abs (vector2-x vec))
                (abs (vector2-y vec))))

;;! Utility procedure to make a vector with each component 1.0 divided by the
;; component of the given one
(define (vector2.inverse vec)
  (make-vector2 (/ 1 (vector2-x vec))
                (/ 1 (vector2-y vec))))

;;! Clamp to low and high vector2
(define (vector2.clamp-vector2 vec lo-vec hi-vec)
  (let ((x (vector2-x vec))
        (y (vector2-y vec))
        (lox (vector2-x lo-vec))
        (loy (vector2-y lo-vec))
        (hix (vector2-x hi-vec))
        (hiy (vector2-y hi-vec)))
    (make-vector2
     (cond ((< x lox) lox)
           ((> x hix)  hix)
           (else x))
     (cond ((< y loy) lox)
           ((> y hiy) hiy)
           (else y)))))

;;! Clamp to low and high values
(define (vector2.clamp-scalar vec lo hi)
  (let ((x (vector2-x vec))
        (y (vector2-y vec)))
    (make-vector2
     (cond ((< x lo) lo)
           ((> x hi) hi)
           (else x))
     (cond ((< y lo) lo)
           ((> y hi) hi)
           (else y)))))

