; Test with
;   time biwas voxel.scm
;   time csi -q -b -r5rs-syntax voxel.scm
;   time racket -q -I r5rs -e '(load "voxel.scm")'


; I'm applying for a villain loan. I go by the name of Vector. It's a
; mathematical term, represented by an arrow with both direction and magnitude.
; Vector! That's me, because I commit crimes with both direction and magnitude.
; Oh yeah! 
;   -- Victor, Despicable Me (2010)

(define (vec x y z)
  (vector x y z))

(define vec-0 (vec 0 0 0))
(define vec-1 (vec 1 1 1))

(define vec-i (vec 1 0 0))
(define vec-j (vec 0 1 0))
(define vec-k (vec 0 0 1))

(define (vec-x v) (vector-ref v 0))
(define (vec-y v) (vector-ref v 1))
(define (vec-z v) (vector-ref v 2))

(define (vec+ a b)
  (vector
    (+ (vec-x a) (vec-x b))
    (+ (vec-y a) (vec-y b))
    (+ (vec-z a) (vec-z b))))

(define (vec-- a)
  (vector
    (- (vec-x a))
    (- (vec-y a))
    (- (vec-z a))))

(define (vec- a b)
  (vec+ a (vec-- b)))

(define (vec* a x)
  (vector
    (* (vec-x a) x)
    (* (vec-y a) x)
    (* (vec-z a) x)))

(define (vec/ a x)
  (vec* a (/ 1 x)))

(define (vec-dot v w)
  (+
    (* (vec-x v) (vec-x w))
    (* (vec-y v) (vec-y w))
    (* (vec-z v) (vec-z w))))

(define (vec-norm v)
  (vec-dot v v))

(define (vec-len v)
  (sqrt (vec-norm v)))

(define (vec-normalize v)
  (vec/ v (sqrt (vec-len v))))

(define (vec-resize v x)
  (vec* (vec-normalize v) x))

(define (vec-angle v w)
  (acos (/ (vec-dot v w) (* (vec-len v) (vec-len w)))))

(define (vec-proj v w)
  (vec-resize w (vec-dot v (vec-normalize w))))


; "Clouds come floating into my life, no longer to carry rain or usher storm,
; but to add color to my sunset sky."
;   -- Rabindranath Tagore, Stray Birds

(define (color r g b)
  (vector r g b))

(define (color-r c) (vector-ref c 0))
(define (color-g c) (vector-ref c 1))
(define (color-b c) (vector-ref c 2))

(define (color->string c)
  (list
    (floor (* 255 (color-b c)))
    (floor (* 255 (color-g c)))
    (floor (* 255 (color-r c)))
    ))

(define color-white (color 1 1 1))
(define color-black (color 0 0 0))

; "Because I'm not normal," I said.
; "You say that as if it's a bad thing, Percy. But you don't realize how
; important you are." 
;   -- Rick Riordan, The Lightning Thief

(define (square pos size normal)
  (define (intersect x v)
    (define (query axis)
      (let* ((diff (- (axis pos) (axis x)))
             (ratio (/ diff (axis v)))
             (newlen (* (vec-len v) ratio))
             (newpos (vec+ x (vec-resize v newlen))))
        (and
          (positive? newlen)
;         (> size (abs (- (vec-x pos) (vec-x newpos))))
;         (> size (abs (- (vec-y pos) (vec-y newpos))))
;         (> size (abs (- (vec-z pos) (vec-z newpos))))
          (> size (vec-len (vec- pos newpos)))
          newlen)))

    (cond
      ((positive? (vec-x normal)) (query vec-x))
      ((positive? (vec-y normal)) (query vec-y))
      ((positive? (vec-z normal)) (query vec-z))))

  ; interface object:
  ;   'intersect : vec -> vec -> [#f if fail, <distance> if intersect]
  ;   'normal    : exactly what you think it is
  ;   'material  : exactly what you think it is

  (define (dispatch msg)
    (cond
      ((eq? msg 'intersect) intersect)))
  dispatch)

; The Cube is an imitation of life itself - or even an improvement on life.
;   -- Erno Rubik

(define (make-voxel pos size)
  (list
    (square (vec+ pos (vec-resize vec-i size)) size vec-i)
    (square (vec- pos (vec-resize vec-i size)) size vec-i)
    (square (vec+ pos (vec-resize vec-j size)) size vec-j)
    (square (vec- pos (vec-resize vec-j size)) size vec-j)
    (square (vec+ pos (vec-resize vec-k size)) size vec-k)
    (square (vec- pos (vec-resize vec-k size)) size vec-k)
    ))


; Bitmaps!


; So, just to make things clear, not only does the BMP format reverse network
; byte order, it *also* reverses RGB to BGR and (just for fun) the Y axis of
; the image.
(define (integer->dword x)
  (list
    (modulo (quotient x (expt 2  0)) 256)
    (modulo (quotient x (expt 2  8)) 256)
    (modulo (quotient x (expt 2 16)) 256)
    (modulo (quotient x (expt 2 24)) 256)
    ))

(define (integer->word x)
  (list
    (modulo (quotient x (expt 2  0)) 256)
    (modulo (quotient x (expt 2  8)) 256)
    ))


; DIB header in BITMAPINFOHEADER format
(define (bmp-info w h)
  (append
    (integer->dword 40)    ; header size
    (integer->dword w)     ; width
    (integer->dword h)     ; height
    (integer->word  1)     ; number of color planes
    (integer->word 24)     ; bits per pixel
    (integer->dword 0)     ; compression
    (integer->dword 0)     ; image size, 0 for no compression
    (integer->dword 0)     ; horizontal resolution
    (integer->dword 0)     ; vertical resolution
    (integer->dword 0)     ; number of colors
    (integer->dword 0)     ; number of colors used
    ))

(define (bmp-create img)
  (define w (length (car img)))
  (define h (length img))
  (define info (bmp-info w h))
  (define dump (bmp-dump img))
  (append
    '(66 77)                              ; magic
    (integer->dword                       ; file size
      (+ 14
         (length info)
         (length dump)))
    (integer->word 0)                     ; reserved
    (integer->word 0)                     ; reserved
    (integer->dword (+ 14 40))            ; data offset

    info                                  ; DIB header

    dump                                  ; pixel data
    ))

(define (bmp-dump img)
  (define (make-junk size)
    (if (= size 0) '()
      (cons 0 (make-junk (- size 1)))))
  (apply append
         (map
           (lambda (row)
             (define data
               (apply append
                      (map color->string row)))
             (append data
                     (make-junk (modulo (- (length data)) 4))))
           (reverse img))))



; And now some actual rendering fun
(define (range from to step)
  (if (> from to)
    '()
    (cons from
          (range (+ from step) to step))))

(define (shiny-image get-pixel)
  (map (lambda (y)
         (map (lambda (x)
                (get-pixel (vec 0 0 0) (vec (asin x) (asin y) 1)))
              (range -0.5 0.5 0.002)))
       (range -0.3 0.3 0.002)))



(define (ray-get-intersection pos dir objects)
  (define intersections
    (map (lambda (o)
           (cons ((o 'intersect) pos dir) o))
         objects))
  (define (choose-closer a b)
    (cond ((and (not (car a)) (not (car b)))
           (cons #f #f))
          ((not (car a)) b)
          ((not (car b)) a)
          ((< (car a) (car b)) a)
          (else b)))
  (define (get-min-intersection ixs current-best)
    (if (null? ixs) current-best
      (get-min-intersection (cdr ixs)
                            (choose-closer current-best (car ixs)))))
  (define min-intersection (get-min-intersection intersections (cons #f #f)))
  min-intersection)

(define objects
  (append
;   (make-voxel (vec -8 9 0) 10)
    (make-voxel (vec  0  0 24) 2)
    (make-voxel (vec  4  4 20) 2)
    (make-voxel (vec -4 -4 20) 2)
    ))

(define (get-pixel pos dir)
  (define r (ray-get-intersection pos dir objects))
  (if (car r)
    (color 0 (expt 0.9 (car r)) 0)
    color-black))





(define (chicken-echo-bytes b)
  (display
    (list->string
      (map integer->char
           (map inexact->exact b)))))

(define (racket-echo-bytes b)
    (for-each write-byte (map inexact->exact b)))

(chicken-echo-bytes (bmp-create (shiny-image get-pixel)))
