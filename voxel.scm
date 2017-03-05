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


(define (vec-point from to)
    (vec- to from))


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
  (vec/ v (vec-len v)))

(define (vec-resize v x)
  (vec* (vec-normalize v) x))

(define (vec-angle v w)
  (acos (/ (vec-dot v w) (* (vec-len v) (vec-len w)))))

(define (vec-project v w)
  (vec-resize w (vec-dot v (vec-normalize w))))

(define pi 3.14159265359)

(define (// x y)
    (/ x (if (= y 0) 0.0000001 y)))

(define (atan2 y x)
  (define atan1 (atan (// y x)))
  (cond [(> x 0) atan1]
        [(> y 0) (- pi atan1)]
        [(< y 0) (- (- pi) atan1)]
        [(< x 0) (+ pi atan1)]
        ; Else I was given (0, 0). This was not part of the job description. I
        ; promptly resign.
        ))



; TODO: Spherical coordinates


; Young me was so much smarter than old me.
(define (vec-with-angles x y)
  (vec
    (sin x)
    (sin y)
    (* (cos x) (cos y))))

(define (vec-angle-x v)
    (atan2 (vec-x v) (vec-z v)))

(define (vec-angle-y v)
    (atan2 (vec-y v) (vec-z v)))

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
    (min 255 (floor (* 255 (color-b c))))
    (min 255 (floor (* 255 (color-g c))))
    (min 255 (floor (* 255 (color-r c))))
    ))

(define color-white (color 1 1 1))
(define color-black (color 0 0 0))

(define (color* x y)
  (color
    (* (color-r x) (color-r y))
    (* (color-g x) (color-g y))
    (* (color-b x) (color-b y))))

(define (color% c x)
  (color
    (* (color-r c) x)
    (* (color-g c) x)
    (* (color-b c) x)))

(define (color+ x y)
  (color
    (+ (color-r x) (color-r y))
    (+ (color-g x) (color-g y))
    (+ (color-b x) (color-b y))))

; "Because I'm not normal," I said.
; "You say that as if it's a bad thing, Percy. But you don't realize how
; important you are." 
;   -- Rick Riordan, The Lightning Thief


(define (square pos size normal material)
  (define (nz x)
    (if (= x 0) 0.000001 x))
  (define (intersect x v)
    (define (query axis)
      (let* ((diff (- (axis pos) (axis x)))
             (ratio (/ diff (nz (axis v))))
             (newlen (* (vec-len v) ratio))
             ;            (newpos (vec+ x (vec-resize v newlen)))
             (newpos (vec+ x (vec* v ratio)))
             )
        (and
          (positive? newlen)
          (> size (abs (- (vec-x pos) (vec-x newpos))))
          (> size (abs (- (vec-y pos) (vec-y newpos))))
          (> size (abs (- (vec-z pos) (vec-z newpos))))
          ;         (> (* 1 size) (vec-len (vec- pos newpos)))
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
      ((eq? msg 'intersect) intersect)
      ((eq? msg 'normal) normal)
      ((eq? msg 'material) material)
      ))
  dispatch)

(define (material reflectivity lambert phong shininess)
  (define (dispatch msg)
    (cond
      ((eq? msg 'reflectivity) reflectivity)
      ((eq? msg 'lambert) lambert)
      ((eq? msg 'phong) phong)
      ((eq? msg 'shininess) shininess)))
  dispatch)

(define material-metal
  (material (color 1.0 1.0 1.0)
            (color 0.2 0.2 0.2)
            (color 1.0 1.0 1.0)
            5))
(define material-plastic
  (material (color 0.0 0.0 0.0)
            (color 0.2 0.2 1.0)
            (color 1.0 1.0 1.0)
            5))

; The Cube is an imitation of life itself - or even an improvement on life.
;   -- Erno Rubik

(define (make-voxel pos size material)
  (list
    (square (vec+ pos (vec-resize vec-i size)) size vec-i material)
    (square (vec- pos (vec-resize vec-i size)) size vec-i material)
    (square (vec+ pos (vec-resize vec-j size)) size vec-j material)
    (square (vec- pos (vec-resize vec-j size)) size vec-j material)
    (square (vec+ pos (vec-resize vec-k size)) size vec-k material)
    (square (vec- pos (vec-resize vec-k size)) size vec-k material)
    ))


(define (point-light pos color)
  (define (dispatch msg)
    (cond
      ((eq? msg 'position) pos)
      ((eq? msg 'color) color)))
  dispatch)

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




; "Stuff your eyes with wonder, he said, live as if you'd drop dead in ten
; seconds. See the world. It's more fantastic than any dream made or paid for
; in factories."
;   -- Ray Bradbury, Fahrenheit 451

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

(define (ray-trace pos dir objects lights depth)
  (define intersection (ray-get-intersection pos dir objects))
  (define object (cdr intersection))

  (if (not object) color-black
    (let ()
      (define reflect-pos
        (vec+ pos (vec-resize dir (car intersection))))
      (define reflect-dir
        (vec+ dir (vec* (vec-project dir (object 'normal)) -2)))

      (define reflect-color
        (if (or (= depth 0)
                (equal? ((object 'material) 'reflectivity)
                        color-black))
          color-black
          (color*
            ((object 'material) 'reflectivity)
            (ray-trace
              reflect-pos
              reflect-dir
              (delete
                object
                objects)
              lights
              (- depth 1)))))

      (define shade-color
        (foldl
          color+ color-black
          (map
            (lambda (light)
              (define light-dir
                (vec-point reflect-pos (light 'position)))
              (define light-dir-refl
                (vec+ (vec-- light-dir)
                      (vec* (vec-project
                              (vec-- light-dir)
                              (object 'normal))
                            -2)))
              (define shadow-ixn
                (ray-get-intersection
                  reflect-pos
                  light-dir
                  (delete
                    object
                    objects)))
              (if (and (car shadow-ixn)
                       (< (car shadow-ixn) (vec-len light-dir)))
                color-black
                (let ()
                  (define normal (object 'normal))
                  (define lambert-intensity
                    (abs
                      (vec-dot (vec-normalize light-dir)
                               (vec-normalize normal))))
                  (define lambert (color% (light 'color) lambert-intensity))
                  (define phong-intensity
                    (max 
                      0
                      (expt (vec-dot
                              (vec-normalize light-dir-refl)
                              (vec-normalize (vec-- dir)))
                            ((object 'material) 'shininess))))
                  (define phong (color% (light 'color) phong-intensity))

                  (color+
                    (color* lambert ((object 'material) 'lambert))
                    (color* phong   ((object 'material) 'phong)))
                  )))
            lights)))


      (color+ reflect-color shade-color)
      )))
















; This is my sandbox. MINE.
(define (range from to step)
  (if (> from to)
    '()
    (cons from
          (range (+ from step) to step))))

(define (shiny-image get-pixel)
  (define camera-pos (vec 0 0 0))
  (map (lambda (y)
         (map (lambda (x)
                (get-pixel
                  camera-pos
                  (vec (asin x) (asin y) 1)))
              (range -0.5 0.5 0.002)))
       (range -0.3 0.3 0.002)))

(define (get-pixel pos dir)
    (ray-trace pos dir objects lights 2))



; Believe it or not, Stanford has a CT scan of their bunny.
; http://graphics.stanford.edu/data/voldata/voldata.html#bunny
(define objects
  (append
    (list (square (vec 0 2  18) 20 vec-j material-metal)) ; ground
    (list (square (vec 0 2  20) 20 vec-k material-metal)) ; wall
    ;   (apply append
    ;       (map
    ;           (lambda (x)
    ;               (make-voxel
    ;                   (vec (modulo (* 2 x) 5) (+ 1 (modulo (* 2 x) 4)) x)
    ;                   0.5 material-plastic))
    ;           (range 10 20 1)))

    ;   (map
    ;     (lambda (x)
    ;       (square (vec x 1 8) 0.7 vec-i material-plastic))
    ;     (range -3 3 0.2))
    (list
      (square (vec -0.805 -0.122248 10.856856) 0.1 vec-k material-plastic)
      (square (vec -1.025 -1.1324 10.62871) 0.1 vec-k material-plastic)
      (square (vec -1.47 -1.5242 10.742334) 0.1 vec-k material-plastic)
      (square (vec 0.27 0.298624 11.010832) 0.1 vec-k material-plastic)
      (square (vec 0.375 -0.326802 10.95889) 0.1 vec-k material-plastic)
      (square (vec 0.23 -0.69744 10.793862) 0.1 vec-k material-plastic)
      (square (vec -1.635 -0.8909 10.98345) 0.1 vec-k material-plastic)
      (square (vec 0.145 -0.169808 11.121922) 0.1 vec-k material-plastic)
      (square (vec -1.555 -1.31586 10.953482) 0.1 vec-k material-plastic)
      (square (vec 0.505 -0.76296 10.694808) 0.1 vec-k material-plastic)
      (square (vec -0.89 0.67804 10.871932) 0.1 vec-k material-plastic)
      (square (vec -1.16 0.700202 10.930354) 0.1 vec-k material-plastic)
      (square (vec -1.03 0.445804 10.633148) 0.1 vec-k material-plastic)
      (square (vec -0.27 -1.74228 9.76636) 0.1 vec-k material-plastic)
      (square (vec -1.39 -0.93572 11.062456) 0.1 vec-k material-plastic)
      (square (vec -1.65 -0.93316 10.985866) 0.1 vec-k material-plastic)
      (square (vec -0.91 -1.89452 9.99012158) 0.1 vec-k material-plastic)
      (square (vec -1.14 -1.4352 10.63584) 0.1 vec-k material-plastic)
      (square (vec -1.47 -0.85138 11.055238) 0.1 vec-k material-plastic)
      (square (vec -0.07 0.176538 11.131018) 0.1 vec-k material-plastic)
      (square (vec 0.105 0.631692 10.909858) 0.1 vec-k material-plastic)
      (square (vec -0.28 -0.479958 10.93354) 0.1 vec-k material-plastic)
      (square (vec -0.725 -0.17978 10.87474) 0.1 vec-k material-plastic)
      (square (vec -0.62 0.350976 10.744698) 0.1 vec-k material-plastic)
      (square (vec -0.53 -0.83184 10.649164) 0.1 vec-k material-plastic)
      (square (vec -0.78 0.653906 10.83128) 0.1 vec-k material-plastic)
      (square (vec 0.79 0.590696 10.610516) 0.1 vec-k material-plastic)
      (square (vec 0.95 0.389388 10.633134) 0.1 vec-k material-plastic)
      (square (vec 0.935 0.10725 10.46795) 0.1 vec-k material-plastic)
      (square (vec 0.065 0.24594 11.13092) 0.1 vec-k material-plastic)
      (square (vec -0.84 0.288762 10.810678) 0.1 vec-k material-plastic)
      (square (vec 0.545 -0.239244 10.918092) 0.1 vec-k material-plastic)
      (square (vec -0.2 -0.5068 10.913208) 0.1 vec-k material-plastic)
      (square (vec 0.025 0.658114 10.927692) 0.1 vec-k material-plastic)
      (square (vec -1.175 0.20403 10.655936) 0.1 vec-k material-plastic)
      (square (vec -0.775 -1.6117 10.1039956) 0.1 vec-k material-plastic)
      (square (vec 0.245 -1.06942 10.499262) 0.1 vec-k material-plastic)
      (square (vec -0.41 -0.58812 10.859862) 0.1 vec-k material-plastic)
      (square (vec -1.175 -0.57236 10.834416) 0.1 vec-k material-plastic)
      (square (vec 1.14 0.097828 10.41204) 0.1 vec-k material-plastic)
      (square (vec -1.525 -1.20376 11.023734) 0.1 vec-k material-plastic)
      (square (vec -1.265 -0.76656 10.760894) 0.1 vec-k material-plastic)
      (square (vec -0.985 -0.79092 10.684682) 0.1 vec-k material-plastic)
      (square (vec -1.245 -0.036388 10.836412) 0.1 vec-k material-plastic)
      (square (vec 0.07 0.318256 11.099612) 0.1 vec-k material-plastic)
      (square (vec 0.095 -0.142714 11.131468) 0.1 vec-k material-plastic)
      (square (vec -1.365 -1.03332 11.019258) 0.1 vec-k material-plastic)
      (square (vec -0.795 -0.95532 10.537538) 0.1 vec-k material-plastic)
      (square (vec 0.4 0.018424 11.023334) 0.1 vec-k material-plastic)
      (square (vec -1.775 -1.41944 10.58762) 0.1 vec-k material-plastic)
      (square (vec -1.635 -0.30374 10.64837) 0.1 vec-k material-plastic)
      (square (vec -1.035 -0.377456 10.880034) 0.1 vec-k material-plastic)
      (square (vec 0.52 -0.338256 10.923266) 0.1 vec-k material-plastic)
      (square (vec 0.56 0.461608 10.764908) 0.1 vec-k material-plastic)
      (square (vec 0.74 0.359556 10.650878) 0.1 vec-k material-plastic)
      (square (vec -0.585 -1.6697 10.0202564) 0.1 vec-k material-plastic)
      (square (vec 0.715 -0.450626 10.709644) 0.1 vec-k material-plastic)
      (square (vec -0.57 0.436566 10.731982) 0.1 vec-k material-plastic)
      (square (vec 0.16 0.378016 11.056304) 0.1 vec-k material-plastic)
      (square (vec -0.345 -0.335852 11.105318) 0.1 vec-k material-plastic)
      (square (vec -1.835 -0.82654 10.821954) 0.1 vec-k material-plastic)
      (square (vec 0.305 -1.067 10.440842) 0.1 vec-k material-plastic)
      (square (vec -0.235 -1.87808 9.649876) 0.1 vec-k material-plastic)
      (square (vec -1.01 -0.138316 10.882312) 0.1 vec-k material-plastic)
      (square (vec 0.66 0.68338 10.5447) 0.1 vec-k material-plastic)
      (square (vec -0.845 -0.038482 10.861834) 0.1 vec-k material-plastic)
      (square (vec -0.43 -0.18386 11.124338) 0.1 vec-k material-plastic)
      (square (vec -0.275 0.542172 10.957778) 0.1 vec-k material-plastic)
      (square (vec -0.26 -0.403682 11.069414) 0.1 vec-k material-plastic)
      (square (vec 1.175 0.250402 10.450374) 0.1 vec-k material-plastic)
      (square (vec 0.725 0.668244 10.552292) 0.1 vec-k material-plastic)
      (square (vec -0.34 -0.240192 11.136398) 0.1 vec-k material-plastic)
      (square (vec -1.38 -0.73052 10.87778) 0.1 vec-k material-plastic)
      (square (vec 0.78 -0.56762 10.531792) 0.1 vec-k material-plastic)
      (square (vec -1.495 0.101846 10.531456) 0.1 vec-k material-plastic)
      (square (vec -0.95 0.063168 10.821058) 0.1 vec-k material-plastic)
      (square (vec -0.125 -0.471618 11.031122) 0.1 vec-k material-plastic)
      (square (vec -0.175 0.164304 11.111842) 0.1 vec-k material-plastic)
      (square (vec -0.285 -0.254226 11.138814) 0.1 vec-k material-plastic)
      (square (vec -1.75 -0.392978 10.495852) 0.1 vec-k material-plastic)
      (square (vec -1.765 -0.80226 10.898172) 0.1 vec-k material-plastic)
      (square (vec -0.36 -0.78144 10.77362) 0.1 vec-k material-plastic)
      (square (vec -1.155 -0.8523 10.771158) 0.1 vec-k material-plastic)
      (square (vec -0.89 -0.135668 10.849242) 0.1 vec-k material-plastic)
      (square (vec 0.975 0.556618 10.51921) 0.1 vec-k material-plastic)
      (square (vec -0.51 -0.34088 10.958898) 0.1 vec-k material-plastic)
      (square (vec -0.99 -1.77528 10.0956504) 0.1 vec-k material-plastic)
      (square (vec -0.885 0.219206 10.803132) 0.1 vec-k material-plastic)
      (square (vec 0.585 -0.235682 10.872272) 0.1 vec-k material-plastic)
      (square (vec -1.185 -0.99692 10.822002) 0.1 vec-k material-plastic)
      (square (vec 0.375 0.580738 10.85392) 0.1 vec-k material-plastic)
      (square (vec 0.8 0.389388 10.633134) 0.1 vec-k material-plastic)
      (square (vec -0.76 -0.99568 10.468886) 0.1 vec-k material-plastic)
      (square (vec -0.06 -0.81198 10.814316) 0.1 vec-k material-plastic)
      (square (vec -1.18 -0.42002 10.882612) 0.1 vec-k material-plastic)
      (square (vec -1.41 -0.045 10.770324) 0.1 vec-k material-plastic)
      (square (vec 0.8 0.560076 10.630752) 0.1 vec-k material-plastic)
      (square (vec 0.285 -1.01526 10.585714) 0.1 vec-k material-plastic)
      (square (vec -0.56 -1.83356 9.839027) 0.1 vec-k material-plastic)
      (square (vec -0.425 -1.87846 9.741696) 0.1 vec-k material-plastic)
      (square (vec -0.98 -1.05952 10.608506) 0.1 vec-k material-plastic)
      (square (vec -1.7 -0.84826 10.971458) 0.1 vec-k material-plastic)
      (square (vec 0.285 0.113316 11.056746) 0.1 vec-k material-plastic)
      (square (vec -1.59 -1.44016 10.816118) 0.1 vec-k material-plastic)
      (square (vec -0.875 0.747446 10.884808) 0.1 vec-k material-plastic)
      (square (vec -1.1 0.005938 10.833822) 0.1 vec-k material-plastic)
      (square (vec -1.145 -1.44952 10.633278) 0.1 vec-k material-plastic)
      (square (vec -0.435 -0.98152 10.476528) 0.1 vec-k material-plastic)
      (square (vec -0.25 -0.81038 10.783818) 0.1 vec-k material-plastic)
      (square (vec -1.29 -0.90496 10.983456) 0.1 vec-k material-plastic)
      (square (vec 0.475 -0.70902 10.748116) 0.1 vec-k material-plastic)
      (square (vec -1.27 -1.52378 10.724562) 0.1 vec-k material-plastic)
      (square (vec 0.46 -0.64026 10.786172) 0.1 vec-k material-plastic)
      (square (vec -1.04 -0.53006 10.83692) 0.1 vec-k material-plastic)
      (square (vec -1.165 -1.30614 10.653826) 0.1 vec-k material-plastic)
      (square (vec 0.68 0.065536 10.793112) 0.1 vec-k material-plastic)
      (square (vec -0.65 -1.86402 9.8902632) 0.1 vec-k material-plastic)
      (square (vec 0.065 -1.09838 10.499218) 0.1 vec-k material-plastic)
      (square (vec -0.28 0.039262 11.112028) 0.1 vec-k material-plastic)
      (square (vec -0.51 -0.61468 10.831914) 0.1 vec-k material-plastic)
      (square (vec -0.88 0.733302 10.88477) 0.1 vec-k material-plastic)
      (square (vec 0.165 -0.62914 10.837014) 0.1 vec-k material-plastic)
      (square (vec 0.565 0.074184 10.856606) 0.1 vec-k material-plastic)
      (square (vec 0.52 0.339148 10.874016) 0.1 vec-k material-plastic)
      (square (vec 0.4 -0.06566 11.02828) 0.1 vec-k material-plastic)
      (square (vec 0.725 0.517164 10.630686) 0.1 vec-k material-plastic)
      (square (vec -0.795 -0.094392 10.859364) 0.1 vec-k material-plastic)
      (square (vec -1.375 0.620822 10.1592798) 0.1 vec-k material-plastic)
      (square (vec -1.8 -1.04234 10.880348) 0.1 vec-k material-plastic)
      (square (vec -1.18 -1.49442 10.699222) 0.1 vec-k material-plastic)
      (square (vec -1.4 -1.13196 10.990342) 0.1 vec-k material-plastic)
      (square (vec -1.04 -1.42722 10.34638) 0.1 vec-k material-plastic)
      (square (vec -1.115 -0.82126 10.7203) 0.1 vec-k material-plastic)
      (square (vec 0.795 0.375982 10.625524) 0.1 vec-k material-plastic)
      (square (vec -1.865 -1.09644 10.45096) 0.1 vec-k material-plastic)
      (square (vec 0.18 -0.6164 10.859886) 0.1 vec-k material-plastic)
      (square (vec -1.325 -1.00428 10.997768) 0.1 vec-k material-plastic)
      (square (vec 0.46 0.363696 10.912004) 0.1 vec-k material-plastic)
      (square (vec -0.46 0.549612 11.02293) 0.1 vec-k material-plastic)
      (square (vec -0.915 -1.12406 10.41536) 0.1 vec-k material-plastic)
      (square (vec 0.315 -0.303232 11.031042) 0.1 vec-k material-plastic)
      (square (vec 0.08 -0.94826 10.697602) 0.1 vec-k material-plastic)
      (square (vec -0.46 -0.477384 10.89538) 0.1 vec-k material-plastic)
      (square (vec 0.615 0.69695 10.552326) 0.1 vec-k material-plastic)
      (square (vec -0.77 -0.58366 10.788654) 0.1 vec-k material-plastic)
      (square (vec 0.065 -0.39052 11.08378) 0.1 vec-k material-plastic)
      (square (vec -0.375 0.399618 10.977032) 0.1 vec-k material-plastic)
      (square (vec -1.23 0.705544 10.879618) 0.1 vec-k material-plastic)
      (square (vec -0.98 -1.19854 10.466054) 0.1 vec-k material-plastic)
      (square (vec 0.88 -0.109196 10.518542) 0.1 vec-k material-plastic)
      (square (vec -0.855 0.748262 10.877196) 0.1 vec-k material-plastic)
      (square (vec -0.68 0.005938 10.833822) 0.1 vec-k material-plastic)
      (square (vec -0.51 -1.7436 9.906734) 0.1 vec-k material-plastic)
      (square (vec -0.32 0.352882 11.027538) 0.1 vec-k material-plastic)
      (square (vec -1.28 0.038286 10.782972) 0.1 vec-k material-plastic)
      (square (vec -0.545 0.515452 10.942484) 0.1 vec-k material-plastic)
      (square (vec -0.325 0.592066 11.020658) 0.1 vec-k material-plastic)
      (square (vec 0.05 -0.92122 10.72805) 0.1 vec-k material-plastic)
      (square (vec -0.52 0.23393 10.795508) 0.1 vec-k material-plastic)
      (square (vec -1.825 -1.02696 10.854972) 0.1 vec-k material-plastic)
      (square (vec -0.12 -0.43197 11.074216) 0.1 vec-k material-plastic)
      (square (vec 0.2 -0.374246 11.045448) 0.1 vec-k material-plastic)
      (square (vec -0.56 0.579488 11.006206) 0.1 vec-k material-plastic)
      (square (vec -1.76 -0.8173 10.915972) 0.1 vec-k material-plastic)
      (square (vec 0.005 -0.00633 11.162448) 0.1 vec-k material-plastic)
      (square (vec -0.39 -0.69862 10.8142) 0.1 vec-k material-plastic)
      (square (vec -0.89 -1.74562 10.119996) 0.1 vec-k material-plastic)
      (square (vec -0.57 0.55429 10.977302) 0.1 vec-k material-plastic)
      (square (vec -0.455 -0.05548 11.078604) 0.1 vec-k material-plastic)
      (square (vec -0.43 -0.071532 11.107396) 0.1 vec-k material-plastic)
      (square (vec -0.625 -0.32113 10.882532) 0.1 vec-k material-plastic)
      (square (vec 0.37 -0.91784 10.656914) 0.1 vec-k material-plastic)
      (square (vec 0.45 -0.03522 10.994662) 0.1 vec-k material-plastic)
      (square (vec -0.565 -1.75866 9.9067006) 0.1 vec-k material-plastic)
      (square (vec 0.77 0.390322 10.622994) 0.1 vec-k material-plastic)
      (square (vec -0.27 -2.19892 9.473228) 0.1 vec-k material-plastic)
      (square (vec -0.91 -0.61374 10.816656) 0.1 vec-k material-plastic)
      (square (vec -0.575 0.51915 10.904464) 0.1 vec-k material-plastic)
      (square (vec 0.45 0.655472 10.816062) 0.1 vec-k material-plastic)
      (square (vec 0.23 -0.97506 10.654368) 0.1 vec-k material-plastic)
      (square (vec -1.47 -1.5519 10.699124) 0.1 vec-k material-plastic)
      (square (vec -0.825 -0.7086 10.74049) 0.1 vec-k material-plastic)
      (square (vec -0.27 -1.92368 9.636236) 0.1 vec-k material-plastic)
      (square (vec 0.855 0.486874 10.64838) 0.1 vec-k material-plastic)
      (square (vec -1.27 -0.84512 10.908368) 0.1 vec-k material-plastic)
      (square (vec -0.62 0.509426 10.858806) 0.1 vec-k material-plastic)
      (square (vec -1.0 -0.66774 10.770948) 0.1 vec-k material-plastic)
      (square (vec -1.07 -0.64086 10.796342) 0.1 vec-k material-plastic)
      (square (vec -0.695 -1.84924 9.947005) 0.1 vec-k material-plastic)
      (square (vec -1.675 -0.088102 10.432072) 0.1 vec-k material-plastic)
      (square (vec 0.995 0.435712 10.590058) 0.1 vec-k material-plastic)
      (square (vec -0.3 -1.98456 9.619844) 0.1 vec-k material-plastic)
      (square (vec -0.325 -0.087896 11.14098) 0.1 vec-k material-plastic)
      (square (vec -0.21 0.319824 11.082806) 0.1 vec-k material-plastic)
      (square (vec -0.915 -0.151526 10.872156) 0.1 vec-k material-plastic)
      (square (vec -0.905 -1.58354 10.1659172) 0.1 vec-k material-plastic)
      (square (vec -0.335 0.025558 11.10965) 0.1 vec-k material-plastic)
      (square (vec -0.415 -0.08631 11.119404) 0.1 vec-k material-plastic)
      (square (vec -1.535 -0.110594 10.71187) 0.1 vec-k material-plastic)
      (square (vec -0.53 -0.258504 10.990294) 0.1 vec-k material-plastic)
      (square (vec -0.495 -0.8189 10.674556) 0.1 vec-k material-plastic)
      (square (vec -0.23 0.49479 11.008348) 0.1 vec-k material-plastic)
      (square (vec -0.37 -0.348382 11.081368) 0.1 vec-k material-plastic)
      (square (vec 0.255 -0.87806 10.717832) 0.1 vec-k material-plastic)
      (square (vec -1.005 -1.73092 10.1352504) 0.1 vec-k material-plastic)
      (square (vec -1.64 -1.41134 10.808552) 0.1 vec-k material-plastic)
      (square (vec -0.075 0.507164 11.025202) 0.1 vec-k material-plastic)
      (square (vec -1.565 -1.8767 9.320402) 0.1 vec-k material-plastic)
      (square (vec -1.05 -0.474982 10.859762) 0.1 vec-k material-plastic)
      (square (vec -0.62 0.293792 10.754872) 0.1 vec-k material-plastic)
      (square (vec -1.175 -0.85268 10.778782) 0.1 vec-k material-plastic)
      (square (vec -0.55 -1.0178 10.308814) 0.1 vec-k material-plastic)
      (square (vec -1.505 -0.8369 11.043264) 0.1 vec-k material-plastic)
      (square (vec -1.375 0.027424 10.744864) 0.1 vec-k material-plastic)
      (square (vec -0.255 -0.6865 10.84978) 0.1 vec-k material-plastic)
      (square (vec -1.0 0.35747 10.673698) 0.1 vec-k material-plastic)
      (square (vec -1.61 -0.87674 10.981048) 0.1 vec-k material-plastic)
      (square (vec 1.095 0.414508 10.513996) 0.1 vec-k material-plastic)
      (square (vec -0.575 -0.70902 10.748116) 0.1 vec-k material-plastic)
      (square (vec -0.95 -1.74576 10.1301494) 0.1 vec-k material-plastic)
      (square (vec 0.895 -0.175914 10.46757) 0.1 vec-k material-plastic)
      (square (vec 0.14 -0.7271 10.81677) 0.1 vec-k material-plastic)
      (square (vec -1.32 -0.96214 11.012208) 0.1 vec-k material-plastic)
      (square (vec -0.26 0.581476 10.986994) 0.1 vec-k material-plastic)
      (square (vec 0.825 0.67939 10.443246) 0.1 vec-k material-plastic)
      (square (vec -0.225 -1.78724 9.704142) 0.1 vec-k material-plastic)
      (square (vec -1.525 -0.111412 10.722044) 0.1 vec-k material-plastic)
      (square (vec -0.925 -0.65474 10.791272) 0.1 vec-k material-plastic)
      (square (vec -0.005 -0.82588 10.809246) 0.1 vec-k material-plastic)
      (square (vec -1.455 -0.259738 10.818844) 0.1 vec-k material-plastic)
      (square (vec -1.66 -1.46622 10.719582) 0.1 vec-k material-plastic)
      (square (vec -0.04 -0.213084 11.145944) 0.1 vec-k material-plastic)
      (square (vec 0.58 -0.08326 10.894946) 0.1 vec-k material-plastic)
      (square (vec -0.93 0.350048 10.75484) 0.1 vec-k material-plastic)
      (square (vec -0.07 -0.867 10.78134) 0.1 vec-k material-plastic)
      (square (vec 0.96 0.361644 10.628056) 0.1 vec-k material-plastic)
      (square (vec -0.485 0.669822 11.083348) 0.1 vec-k material-plastic)
      (square (vec -1.635 -0.7761 10.936276) 0.1 vec-k material-plastic)
      (square (vec 0.075 0.218178 11.13096) 0.1 vec-k material-plastic)
      (square (vec 0.01 0.374556 11.092324) 0.1 vec-k material-plastic)
      (square (vec -0.755 -0.84522 10.631398) 0.1 vec-k material-plastic)
      (square (vec -1.2 0.066182 10.78549) 0.1 vec-k material-plastic)
      (square (vec -1.72 -1.4921 10.607818) 0.1 vec-k material-plastic)
      (square (vec 0.265 -0.73928 10.781196) 0.1 vec-k material-plastic)
      (square (vec -1.09 0.21768 10.661014) 0.1 vec-k material-plastic)
      (square (vec -0.95 -1.1685 10.438158) 0.1 vec-k material-plastic)
      (square (vec -0.475 0.562692 11.03258) 0.1 vec-k material-plastic)
      (square (vec -1.265 -0.488104 10.84451) 0.1 vec-k material-plastic)
      (square (vec 0.41 -0.64162 10.809056) 0.1 vec-k material-plastic)
      (square (vec -1.6 -0.60192 10.623392) 0.1 vec-k material-plastic)
      (square (vec 0.8 0.163062 10.640698) 0.1 vec-k material-plastic)
      (square (vec -0.845 0.161024 10.826034) 0.1 vec-k material-plastic)
      (square (vec -1.505 -0.20005 10.775522) 0.1 vec-k material-plastic)
      (square (vec 0.335 -0.411278 10.95894) 0.1 vec-k material-plastic)
      (square (vec 0.085 -0.495242 10.951356) 0.1 vec-k material-plastic)
      (square (vec -1.46 -0.042288 10.737274) 0.1 vec-k material-plastic)
      (square (vec -0.47 -2.10566 9.776214) 0.1 vec-k material-plastic)
      (square (vec -0.685 -0.92724 10.552776) 0.1 vec-k material-plastic)
      (square (vec 0.58 0.22875 10.853872) 0.1 vec-k material-plastic)
      (square (vec 0.61 -0.3613 10.852028) 0.1 vec-k material-plastic)
      (square (vec 0.945 0.106384 10.47811) 0.1 vec-k material-plastic)
      (square (vec -1.88 -1.02478 10.471384) 0.1 vec-k material-plastic)
      (square (vec -1.015 -1.43938 10.257492) 0.1 vec-k material-plastic)
      (square (vec 0.675 0.486874 10.64838) 0.1 vec-k material-plastic)
      (square (vec -0.11 -0.268102 11.138832) 0.1 vec-k material-plastic)
      (square (vec 0.575 0.271072 10.851282) 0.1 vec-k material-plastic)
      (square (vec -0.905 0.447546 10.764918) 0.1 vec-k material-plastic)
      (square (vec -0.325 -0.490654 10.88267) 0.1 vec-k material-plastic)
      (square (vec 0.085 -0.76988 10.821896) 0.1 vec-k material-plastic)
      (square (vec -0.855 0.075266 10.843904) 0.1 vec-k material-plastic)
      (square (vec 0.39 -0.9978 10.51715) 0.1 vec-k material-plastic)
      (square (vec -0.595 -0.59974 10.819184) 0.1 vec-k material-plastic)
      (square (vec -0.835 -1.0084 10.428222) 0.1 vec-k material-plastic)
      (square (vec 0.515 -0.8447 10.621232) 0.1 vec-k material-plastic)
      (square (vec 0.365 -0.354774 10.956362) 0.1 vec-k material-plastic)
      (square (vec 0.61 0.374906 10.790316) 0.1 vec-k material-plastic)
      (square (vec -0.215 -0.76974 10.819356) 0.1 vec-k material-plastic)
      (square (vec -0.74 0.57185 10.800646) 0.1 vec-k material-plastic)
      (square (vec -0.365 -0.058712 11.121762) 0.1 vec-k material-plastic)
      (square (vec -0.525 0.334598 10.770064) 0.1 vec-k material-plastic)
      (square (vec 0.01 -0.81198 10.814316) 0.1 vec-k material-plastic)
      (square (vec -0.885 -0.76338 10.702434) 0.1 vec-k material-plastic)
      (square (vec -0.83 0.543454 10.80058) 0.1 vec-k material-plastic)
      (square (vec -0.32 -1.74286 9.8284482) 0.1 vec-k material-plastic)
      (square (vec -0.18 0.178198 11.111822) 0.1 vec-k material-plastic)
      (square (vec -1.57 -1.16164 11.042968) 0.1 vec-k material-plastic)
      (square (vec -1.73 -1.24204 10.877432) 0.1 vec-k material-plastic)
      (square (vec -1.06 -0.2803 10.902862) 0.1 vec-k material-plastic)
      (square (vec -1.01 -1.68676 10.170862) 0.1 vec-k material-plastic)
      (square (vec 0.64 -0.7973 10.534756) 0.1 vec-k material-plastic)
      (square (vec 0.31 0.156746 11.037482) 0.1 vec-k material-plastic)
      (square (vec -0.68 -0.192002 10.851856) 0.1 vec-k material-plastic)
      (square (vec -0.165 -0.116856 11.157798) 0.1 vec-k material-plastic)
      (square (vec -0.06 0.402838 11.087478) 0.1 vec-k material-plastic)
      (square (vec -0.325 0.646312 11.04004) 0.1 vec-k material-plastic)
      (square (vec -1.16 -0.474982 10.859762) 0.1 vec-k material-plastic)
      (square (vec -1.21 -0.364986 10.902926) 0.1 vec-k material-plastic)
      (square (vec -0.105 0.653932 10.968002) 0.1 vec-k material-plastic)
      (square (vec -0.5 -0.80504 10.68216) 0.1 vec-k material-plastic)
      (square (vec -0.145 0.448512 11.049048) 0.1 vec-k material-plastic)
      )
    ))

(define lights
  (list
    (point-light (vec   0.1  0  14) (color 0.1 0.1 0.1))
    (point-light (vec  -3  -3  6) (color 0.1 0.1 0.1))
    (point-light (vec  0   0 0)     (color 0.2 0.2 0.2))
    ))

(echo-bytes (bmp-create (shiny-image get-pixel)))
