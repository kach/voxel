#lang racket

(define delete remove)

(define (echo-bytes b)
    (for-each write-byte (map inexact->exact b)))

(include "voxel.scm")
