(define-library (github.com/alvatar/math vector2)

  (import gambit)

  (export
   make-vector2
   vector2?
   vector2-x
   vector2-y
   vector2-x-set!
   vector2-y-set!
   vector2+
   vector2-
   vector2*
   vector2x
   vector2*scalar
   vector2/scalar
   vector2=?
   vector2.proportional?
   vector2.inexact->exact
   vector2.zero
   vector2.zero?
   vector2.random
   vector2.squaredmagnitude
   vector2.symmetric
   vector2.x-projection
   vector2.y-projection
   vector2.max-component
   vector2.min-component
   vector2.x/y
   vector2.y/x
   vector2.abs
   vector2.inverse
   vector2.clamp-vector2
   vector2.clamp-scalar
   )

  (include "vector2.scm"))
