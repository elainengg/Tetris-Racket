;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |CS2500 HW7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; CS2500 Homework 7

; Problem1
(require 2htdp/image)
(require 2htdp/universe)

(define BOARD-HEIGHT 15)
(define BOARD-WIDTH  10)
(define PIXELS/BLOCK  20)
(define BG (empty-scene (* BOARD-WIDTH PIXELS/BLOCK) (* BOARD-HEIGHT PIXELS/BLOCK)))

;;; A Brick is a (make-brick Number Number Color)
(define-struct brick [x y color])
; Temlates for brick:
#|
(define (brick-temp b)
  (...(brick-x b)...
      (brick-y b)...
      (brick-color b)...))
|#

(define green-brick1 (make-brick 0 1 "green"))
(define green-brick2 (make-brick 1 0 "green"))
(define green-brick3 (make-brick 0 0 "green"))
(define green-brick4 (make-brick 1 1 "green"))
(define blue-brick1 (make-brick 0 0 "blue"))
(define blue-brick2 (make-brick 1 0 "blue"))
(define blue-brick3 (make-brick 2 0 "blue"))
(define blue-brick4 (make-brick 3 0 "blue"))
(define purple-brick1 (make-brick 0 1 "purple"))
(define purple-brick2 (make-brick 1 1 "purple"))
(define purple-brick3 (make-brick 2 1 "purple"))
(define purple-brick4 (make-brick 2 0 "purple"))
(define aqua-brick1 (make-brick 0 0 "aqua"))
(define aqua-brick2 (make-brick 0 1 "aqua"))
(define aqua-brick3 (make-brick 1 1 "aqua"))
(define aqua-brick4 (make-brick 2 1 "aqua"))
(define orange-brick1 (make-brick 1 0 "orange"))
(define orange-brick2 (make-brick 0 1 "orange"))
(define orange-brick3 (make-brick 1 1 "orange"))
(define orange-brick4 (make-brick 2 1 "orange"))
(define pink-brick1 (make-brick 0 0 "pink"))
(define pink-brick2 (make-brick 1 0 "pink"))
(define pink-brick3 (make-brick 1 1 "pink"))
(define pink-brick4 (make-brick 2 1 "pink"))
(define red-brick1 (make-brick 0 1 "red"))
(define red-brick2 (make-brick 1 1 "red"))
(define red-brick3 (make-brick 1 0 "red"))
(define red-brick4 (make-brick 2 0 "red"))

;;; A Bricks (Set of Bricks) is one of:
;;; - empty
;;; - (cons Brick Bricks)
; Templates for Bricks:
#|
(define (bricks-temp bs)
  (cond [(empty? bs) ...]
        [(cons? bs) ...(brick-temp (first bs)... (bricks-temp (rest bs)))]))
|#
(define bricks0 '())
(define green-bricks (cons green-brick1 (cons green-brick2
                                              (cons green-brick3 (cons green-brick4 '())))))
(define blue-bricks (cons blue-brick1 (cons blue-brick2
                                            (cons blue-brick3 (cons blue-brick4 '())))))
(define purple-bricks (cons purple-brick1 (cons purple-brick2
                                                (cons purple-brick3 (cons purple-brick4 '())))))
(define aqua-bricks (cons aqua-brick1 (cons aqua-brick2
                                            (cons aqua-brick3 (cons aqua-brick4 '())))))
(define orange-bricks (cons orange-brick1 (cons orange-brick2
                                                (cons orange-brick3 (cons orange-brick4 '())))))
(define pink-bricks (cons pink-brick1 (cons pink-brick2
                                            (cons pink-brick3 (cons pink-brick4 '())))))
(define red-bricks (cons red-brick1 (cons red-brick2
                                          (cons red-brick3 (cons red-brick4 '())))))

;;; A Pt (2D point) is a (make-posn Integer Integer)
;;; For every tetra in this game, Pt is:
;;; (make-posn (brick-x (first (rest bricks)) (brick-y (first (rest bricks))
;;; which is the x and y cordinates of the second brick in the set of bricks.



; Center point for bricks
(define CENTER-POINT (make-posn 5 7))


;;; A Tetra is a (make-tetra Pt Bricks)
;;; The center point is the point around which the tetra
;;; rotates when it spins.
(define-struct tetra [center bricks])

;Templates for tetra:
#|
(define (tetra-temp tt)
  (...(tetra-center tt)...
      (tetra-bricks tt)...))
|#
; point in the tetra
; one of the bricks should stay in place
; brick-x and brick-y
(define O (make-tetra CENTER-POINT green-bricks))
(define I (make-tetra CENTER-POINT blue-bricks))
(define L (make-tetra CENTER-POINT purple-bricks))
(define J (make-tetra CENTER-POINT aqua-bricks))
(define T (make-tetra CENTER-POINT orange-bricks))
(define Z (make-tetra CENTER-POINT pink-bricks))
(define S (make-tetra CENTER-POINT red-bricks))

; draw-brick: Brick -> Image
; Image of brick
(check-expect (draw-brick green-brick1 BG)
              (place-image/align (overlay
                                  (square PIXELS/BLOCK  "outline" "black")
                                  (square PIXELS/BLOCK "solid" (brick-color green-brick1)))
                                 (* PIXELS/BLOCK (brick-x green-brick1))
                                 (* PIXELS/BLOCK (brick-y green-brick1))
                                 "left" "top"
                                 BG))

(define (draw-brick b i)
  (place-image/align (overlay
                      (square PIXELS/BLOCK  "outline" "black")
                      (square PIXELS/BLOCK "solid" (brick-color b)))
                     (* PIXELS/BLOCK (brick-x b))
                     (* PIXELS/BLOCK (brick-y b))
                     "left" "top"
                     i))

; draw-bricks: Bricks -> Image
; Image of Bricks
(check-expect (draw-bricks bricks0 BG) BG) 
(check-expect (draw-bricks green-bricks BG)
              (draw-brick (first green-bricks) (draw-bricks (rest green-bricks) BG)))

(define (draw-bricks bs i)
  (cond [(empty? bs) i]
        [(cons? bs) (draw-brick (first bs) (draw-bricks (rest bs) i))]))

; draw-tetra: Tetra -> Image
; Image of Tetra
(check-expect (draw-tetra O BG) (draw-bricks (tetra-bricks O) BG))
(check-expect (draw-tetra I BG) (draw-bricks (tetra-bricks I) BG))

(define (draw-tetra t i)
  (draw-bricks (tetra-bricks t) i))


;;; A World is a (make-world Tetra Bricks)
;;; The set of bricks represents the pile of bricks
;;; at the bottom of the screen.
(define-struct world [tetra pile])
; Templates for World:
#;
(define (world-temp w)
  (...(world-tetra w)...
      (world-pile w)...))


(define world1 (make-world (make-tetra (make-posn PIXELS/BLOCK PIXELS/BLOCK) '()) '()))
(define world2 (make-world O '()))
(define world3 (make-world I (cons (make-brick 0 14 "green")
                                   (cons (make-brick 1 14 "green")
                                         (cons (make-brick 0 13 "green")
                                               (cons (make-brick 1
                                                                 13 "green") '()))))))




; Random-tetra: Number -> Tetra
; Randomly created tetra
(define (random-tetra n)
  (cond [(= n 0) O]
        [(= n 1) I]
        [(= n 2) L]
        [(= n 3) J]
        [(= n 4) T]
        [(= n 5) Z]
        [(= n 6) S]))


; left-collide? : Bricks -> Boolean
; Determines if all bricks in a set are exceeding the left wall.
(check-expect (left-collide? green-bricks) #t)
(check-expect (left-collide? bricks0) #t)
(define (left-collide? bricks)
  (cond [(empty? bricks) #t]
        [(< (brick-x (first bricks)) 0) #f]
        [else (left-collide? (rest bricks))]))
                               		 

; right-collide? : Bricks -> Boolean
; Determines if all bricks in a set are exceeding the right wall.
(check-expect (right-collide? green-bricks) #t)
(check-expect (right-collide? bricks0) #t)
(define (right-collide? bricks)
  (cond [(empty? bricks) #t]
        [(> (brick-x (first bricks)) 9) #f]
        [else (right-collide? (rest bricks))]))

; bottom? : Bricks -> Boolean
; Determines if all bricks in a set are on the bottom.
(check-expect (bottom? green-bricks) #t)
(check-expect (bottom? bricks0) #t)
(define (bottom? bricks)
  (cond [(empty? bricks) #t]
        [(> (brick-y (first bricks)) 14) #f]
        [else (bottom? (rest bricks))]))

; brick-overlap?: Brick Brick -> Boolean
; check if the brick overlap
(check-expect (brick-overlap? green-brick1 green-brick1) #t)
(check-expect (brick-overlap? green-brick1 green-brick2) #f)

(define (brick-overlap? b1 b2)
  (and (= (brick-x b1) (brick-x b2))
       (= (brick-y b1) (brick-y b2))))

; bricks-overlap: Brick Bricks -> Boolean
; check if the bricks overlap
(check-expect (bricks-overlap? green-brick1 green-bricks) #t)
(check-expect (bricks-overlap? blue-brick3 green-bricks) #f)
(define (bricks-overlap? b bs)
  (cond [(empty? bs)  #f]
        [(brick-overlap? b (first bs)) #t]
        [else (bricks-overlap? b (rest bs))]))
                                                 
; overlap-world:  World -> Boolean
; check if the bricks is going to overlap if keep moving
(check-expect (overlap-world? world3) #f)

(define (overlap-world? w)
  (cond [(empty? (tetra-bricks (world-tetra w))) #f]
        [(bricks-overlap? (first (tetra-bricks (world-tetra w)))
                          (world-pile w)) #t]
        [else (overlap-world? (make-world (make-tetra (tetra-center (world-tetra w))
                                                      (rest (tetra-bricks (world-tetra w))))
                                          (world-pile w)))]))

; correct-world? : World -> Boolean
; Determines if a world is correct created
; which means all bricks are within bounds and not on collide one another.
(check-expect (correct-world? world3) #t)

(define (correct-world? w)
  (and (left-collide?  (tetra-bricks (world-tetra w)))
       (right-collide? (tetra-bricks (world-tetra w)))
       (bottom? 	(tetra-bricks (world-tetra w)))
       (not (overlap-world? w))))



; brick-rotate-ccw : Brick Pt -> Brick
; Rotate the brick 90 counterclockwise around the posn.
(define (brick-rotate-ccw sqr c)
  (make-brick (+ (posn-x c)
                 (- (posn-y c)
                    (brick-y sqr)))
              (+ (posn-y c)
                 (- (brick-x sqr)
                    (posn-x c)))
              (brick-color sqr)))

; bricks-rotate-ccw: Bricks -> Bricks
; Rotate the bricks 90 counterclockwise around the posn.
(check-expect (bricks-rotate-ccw green-bricks O)
              (cons (brick-rotate-ccw green-brick1
                                      (tetra-center O))
                    (cons (brick-rotate-ccw green-brick2
                                            (tetra-center O))
                          (cons (brick-rotate-ccw green-brick3
                                                  (tetra-center O))
                                (cons (brick-rotate-ccw green-brick4 (tetra-center O))
                                      '())))))
(define (bricks-rotate-ccw bs t)
  (cond [(empty? bs) '()]
        [else (cons (brick-rotate-ccw (first bs)
                                      (tetra-center t))
                    (bricks-rotate-ccw (rest bs) t))]))


; rotate-tetra-ccw: Tetra -> Tetra 
; Rotate the Tetra 90 counterclockwise around the posn.

(define (rotate-tetra-ccw t)
  (make-tetra (tetra-center t) (bricks-rotate-ccw (tetra-bricks t) t)))
      



; brick-rotate-cw : Brick Pt -> Brick
; Rotate the brick 90 clockwise around the posn.
(define (brick-rotate-cw sqr c)
  (brick-rotate-ccw (brick-rotate-ccw (brick-rotate-ccw sqr c) c) c))

; bricks-rotate-cw: Bricks -> Bricks
; Rotate the bricks 90 clockwise around the posn.
(check-expect (bricks-rotate-cw green-bricks O)
              (cons (brick-rotate-cw green-brick1
                                     (tetra-center O))
                    (cons (brick-rotate-cw green-brick2
                                           (tetra-center O))
                          (cons (brick-rotate-cw green-brick3
                                                 (tetra-center O))
                                (cons (brick-rotate-cw green-brick4 (tetra-center O))
                                      '())))))
(define (bricks-rotate-cw bs t)
  (cond [(empty? bs) '()]
        [else (cons (brick-rotate-cw (first bs)
                                     (tetra-center t))
                    (bricks-rotate-cw (rest bs) t))]))

; rotate-tetra-cw: Tetra -> Tetra 
; Rotate the Tetra 90 clockwise around the posn.

(define (rotate-tetra-cw t)
  (make-tetra (tetra-center t) (bricks-rotate-cw (tetra-bricks t) t)))
      


; move-down-bricks: Bricks -> Bricks
; Move down bricks one block
(check-expect (move-down-bricks green-bricks)
              (cons (make-brick (brick-x green-brick1) (+ (brick-y green-brick1) 1) "green")
                    (cons (make-brick (brick-x green-brick2) (+ (brick-y green-brick2) 1) "green")
                          (cons
                           (make-brick (brick-x green-brick3) (+ (brick-y green-brick3) 1) "green")
                           (cons (make-brick (brick-x green-brick4)
                                             (+ (brick-y green-brick4) 1) "green") '())))))
(define (move-down-bricks bs)
  (cond [(empty? bs) '()]
        [(cons? bs) (cons (make-brick (brick-x (first bs))
                                      (+ (brick-y (first bs)) 1) (brick-color (first bs)))
                          (move-down-bricks (rest bs)))]))
                                               
; move-down-tetra: Tetra -> Tetra
; Move tetra down
(check-expect (move-down-tetra O) (make-tetra (make-posn (brick-x (first (rest green-bricks)))
                                                         (brick-y (first (rest green-bricks))))
                                              (cons (make-brick (brick-x green-brick1)
                                                                (+ (brick-y green-brick1) 1) "green")
                                                    (cons (make-brick (brick-x green-brick2)
                                                                      (+ (brick-y green-brick2)
                                                                         1) "green")
                                                          (cons
                                                           (make-brick (brick-x green-brick3)
                                                                       (+ (brick-y green-brick3)
                                                                          1) "green")
                                                           (cons (make-brick (brick-x green-brick4)
                                                                             (+ (brick-y green-brick4)
                                                                                1) "green") '()))))))
(define (move-down-tetra t)
  (make-tetra (make-posn (brick-x (first (rest (tetra-bricks t))))
                         (brick-y (first (rest (tetra-bricks t)))))
              (move-down-bricks (tetra-bricks t))))
             
                                    

; Dir = "left" | "right"
; Templates for Dir
#;
(define (dir-template d)
  (cond [(string=? d "right") ...]
        [(string=? d "left") ...]))

; move-bricks-dir: Bricks String -> Bricks
; Move bricks right/left

(check-expect (move-bricks-dir green-bricks "right")
              (cons (make-brick
                     (+ 1 (brick-x green-brick1))
                     (brick-y green-brick1) "green")
                    (cons (make-brick (+ 1 (brick-x green-brick2))
                                      (brick-y green-brick2) "green")
                          (cons (make-brick (+ 1 (brick-x green-brick3))
                                            (brick-y green-brick3) "green")
                                (cons (make-brick (+ 1 (brick-x green-brick4))
                                                  (brick-y green-brick4) "green") '())))))

(define (move-bricks-dir bs dir)
  (cond [(string=? dir "right")
         (cond [(empty? bs) '()]
               [(cons? bs) (cons (make-brick (+ 1 (brick-x (first bs)))
                                             (brick-y (first bs))
                                             (brick-color (first bs))) 
                                 (move-bricks-dir (rest bs) dir))])]
        [(string=? dir "left")
         (cond [(empty? bs) '()]
               [(cons? bs) (cons (make-brick (- (brick-x (first bs)) 1)
                                             (brick-y (first bs))
                                             (brick-color (first bs))) 
                                 (move-bricks-dir (rest bs) dir))])]))
                         

; move-tetra-dir: Tetra String -> Bricks
; Move tetra right/left

(check-expect (move-tetra-dir O "right")
              (make-tetra
               (make-posn (brick-x (first (rest (tetra-bricks O))))
                          (brick-y (first (rest (tetra-bricks O)))))
               (cons (make-brick
                      (+ 1 (brick-x green-brick1))
                      (brick-y green-brick1) "green")
                     (cons (make-brick (+ 1 (brick-x green-brick2))
                                       (brick-y green-brick2) "green")
                           (cons (make-brick (+ 1 (brick-x green-brick3))
                                             (brick-y green-brick3) "green")
                                 (cons (make-brick (+ 1 (brick-x green-brick4))
                                                   (brick-y green-brick4) "green") '()))))))
(define (move-tetra-dir t dir)
  (make-tetra (make-posn (brick-x (first (rest (tetra-bricks t))))
                         (brick-y (first (rest (tetra-bricks t)))))
              (move-bricks-dir (tetra-bricks t) dir)))
      


; add-to-pile : Bricks Bricks -> Bricks
; Adds bricks of moving tetra to bottom pile.

(define (add-to-pile tetra-bricks pile-bricks)
  (cond [(empty? tetra-bricks) pile-bricks]
        [else (append pile-bricks tetra-bricks)]))


; next-world : World -> World
; Moves current tetra down over time (no player input).
; Upon collision, adds current tetra to pile and chooses a new random tetra to control.
(define (next-world w)
  (cond [(not (correct-world? (make-world (move-down-tetra (world-tetra w))
                                          (world-pile w))))
         (make-world (random-tetra (random 7))
                     (add-to-pile (tetra-bricks (world-tetra w))
                                  (world-pile w)))]
        [else (make-world (move-down-tetra (world-tetra w))
                          (world-pile w))]))



; Rotation = "a" | "s"
; 'a is 90 degrees ccw, 's is 90 degrees cw

; Temp for rotation:
#;
(define (rotation-temp rotation)
  (cond [(string=? "a") ...]
        [(string=? "s") ...]))

; tetra-rotate: Tetra KE -> Tetra
; Rotate the tetra according to key event

(define (tetra-rotate t ke)
  (cond [(string=? "a" ke) (make-tetra (tetra-center t)
                                       (bricks-rotate-ccw (tetra-bricks t) t))]
        [(string=? "s" ke) (make-tetra (tetra-center t)
                                       (bricks-rotate-cw (tetra-bricks t) t))]))
                              


; count-bricks: Bs -> Number
; Counts bricks
(check-expect (count-bricks green-bricks) 4)
(check-expect (count-bricks (world-pile world3)) 4)

(define (count-bricks bs)
  (cond [(empty? bs) 0]
        [else (length bs)]))

; score: World -> Number
; Count all the bricks in the world save as score
(check-expect (score world3) 8)
(check-expect (score world2) 4)

(define (score w)
  (+ (count-bricks (world-pile w))
     4))

; last-picture: World -> Image
; Image of your score
(define (last-picture w)
  (overlay (text (string-append "Your Score is " (number->string (score w))) 20 "blue") BG))

; hit-top-bricks?: Bs -> Boolean
; If any brick from bricks hit the top
(check-expect (hit-top-bricks? green-bricks) #t)
(check-expect (hit-top-bricks? (world-pile world3)) #f)

(define (hit-top-bricks? bs)
  (cond [(empty? bs) #f]
        [else (cond [(= 0 (brick-y (first bs))) #t]
                    [else  (hit-top-bricks? (rest bs))])]))
                   
; last-world?: World -> Boolean
; Has pile hit top of the screen y of any of pile = 0 
(define (last-world? w)
  (if (hit-top-bricks? (world-pile w)) #t #f))


; world->scene: World -> Image
; Image of current world
(define (world->scene w)
  (draw-tetra (world-tetra w) (draw-bricks (world-pile w) BG)))

; key-event: String -> World
; Change the world according to KE

(define (key-event w ke)
  (cond [(or (string=? "right" ke) (string=? "left" ke))
         (if (correct-world? (make-world (move-tetra-dir (world-tetra w) ke) (world-pile w)))
             (make-world (move-tetra-dir (world-tetra w) ke) (world-pile w)) w)]
        [(or (string=? "s" ke) (string=? "a" ke))
         (if (correct-world? (make-world (tetra-rotate (world-tetra w) ke) (world-pile w)))
             (make-world (tetra-rotate (world-tetra w) ke) (world-pile w)) w)]
        [else w]))

(define INITIAL-WORLD (make-world (random-tetra (random 7)) empty))


(define (tetris w)
  (big-bang w
    [to-draw world->scene]
    [on-tick next-world 0.5]
    [on-key key-event]
    [stop-when last-world? last-picture]))
             
(tetris INITIAL-WORLD)             
