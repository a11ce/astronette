#lang racket/base

(require 2htdp/image
         lang/posn
         racket/format)

(define green (color 115 255 180))
(define cyan  (color   0 255 255))
(define purple (color 200 50 255))
(define half-purple (color 200 50 255 200))
(define half-green (color 115 255 180 200))

(define HEX-WIDTH 100)
(define HORIZ-DIST (* 3/4 HEX-WIDTH))
(define VERT-DIST (* (sqrt 3) 1/4 HEX-WIDTH))


(struct hvec (x y) #:transparent)

(define N  (cons  0  2))
(define S  (cons  0 -2))
(define NW (cons -1  1))
(define NE (cons  1  1))
(define SW (cons -1 -1))
(define SE (cons  1 -1))

(define CW -60)

(define 2W (hvec -2  0))
(define 2E (hvec  2  0))

(define (v2+/2 v1 v2)
  (cons (+ (car v1)
           (car v2))
        (+ (cdr v1)
           (cdr v2))))

(define (v2+ . vs)
  (foldl v2+/2 (cons 0 0) vs))

(define x car)
(define y cdr)

(define hex
  (let ([1hex (regular-polygon 1 6 'outline green)])
    (define radius (/ HEX-WIDTH 2))
    (define width (* radius 2))
    (scale (/ width (image-width 1hex)) 1hex)))

(define (overlay/offset-hex i1 x y i2)
  (overlay/align/offset 'left 'bottom
                        i1
                        (* x HORIZ-DIST)
                        (* -1 y VERT-DIST) i2))


; TODO should be a struct w dims and 1-1-center info
(define (make-field width height)
  (define f hex)
  (for* ([idx width]
         [idy height])
    (when (= (modulo idx 2) (modulo idy 2))
      (set! f (overlay/offset-hex f idx idy hex))))
  f)

(define (scale-to/xy width height img)
  (scale/xy (/ width (image-width img))
            (/ height (image-height img))
            img))

(define (scale-to/height height img)
  (scale (/ height (image-height img))
         img))


(define (scale-to-and-pin-base/contact-range range img)
  (define simg (scale-to/height (* range VERT-DIST)
                                img))
  (put-pinhole (/ (image-width simg) 2)
               (image-height simg)
               simg))

(define ship-img (polygon (list (make-posn 0 20)
                                (make-posn 5 0)
                                (make-posn 10 20)
                                (make-posn 5 15))
                          'outline cyan))

(define ship (scale-to-and-pin-base/contact-range 2 ship-img))

(define (1-1-center field)
  (cons (/ HEX-WIDTH 2)
        (- (image-height field) VERT-DIST)))

(define (hex->xy v field)
  (define 11c (1-1-center field))
  (cons (+ (x 11c) (* (sub1 (x v)) HORIZ-DIST))
        (- (y 11c) (* (sub1 (y v)) VERT-DIST))))

(define (pin-field-at-coord v field)
  (define xy (hex->xy v field))
  (put-pinhole (x xy) (y xy) field))

(define (rotate-cw n img)
  (rotate (* n CW) img))

(define (overlay/pinhole/clear . args)
  (clear-pinhole (apply overlay/pinhole args)))

(define vel-mark (add-line (line 25 25 purple)
                           0 25 25 0
                           purple))

(define vel-pen
  (pen half-purple 0 'long-dash 'round 'round))

(define (add-line/hex v1 v2 pen field)
  (define xy1 (hex->xy v1 field))
  (define xy2 (hex->xy v2 field))
  (add-line field (x xy1) (y xy1) (x xy2) (y xy2) pen))

(define (draw-at/hex field v img)
  (overlay/pinhole/clear
   img
   (pin-field-at-coord v field)))

(define (draw-at*/hex field . cmds)
  (for/fold ([field field])
            ([cmd cmds])
    (apply draw-at/hex field cmd)))

; TODO unify labels

(define (nudge-pin-for-left-label field)
  (define new-pin (v2+ (cons (* -2/5 HEX-WIDTH) 0)
                       (cons (pinhole-x field) (pinhole-y field))))
  (put-pinhole (x new-pin) (y new-pin) field))

(define (nudge-pin-for-bottom-label field)
  (define new-pin (v2+ (cons 0 (* 1/5 HEX-WIDTH))
                       (cons (pinhole-x field) (pinhole-y field))))
  (put-pinhole (x new-pin) (y new-pin) field))


(define (draw-left-label str v field)
  (overlay/pinhole/clear
   (text str 15 half-green) ; TODO font atlas and stagger spacing
   (nudge-pin-for-left-label (pin-field-at-coord v field))))

(define (draw-bottom-label str v field)
  (overlay/pinhole/clear
   (text str 15 half-green) ; TODO font atlas
   (nudge-pin-for-bottom-label (pin-field-at-coord v field))))

(define (add-row-labels field)
  (for/fold ([field field])
            ([idx (in-range 1 (add1 5))])
    (draw-left-label (~a idx) (cons 1 idx) field)))

(define (add-col-labels field)
  (for/fold ([field field])
            ([idx (in-range 1 (add1 5))])
    (draw-bottom-label (~a (string-ref
                            "-abcdefghijklmnopqrstuvwxyz" idx))
                       (cons idx 1) field)))

(add-line/hex '(1 . 1) '(5 . 3) vel-pen
              (draw-at*/hex
               (add-col-labels (add-row-labels (make-field 5 5)))
               (list '(1 . 1) (rotate-cw 1 ship))
               (list '(5 . 3) vel-mark)))
