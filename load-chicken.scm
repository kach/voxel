(use srfi-1)

(define (echo-bytes b)
  (display
    (list->string
      (map integer->char
           (map inexact->exact b)))))

(include "voxel.scm")
