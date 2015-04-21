;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Programming Assignment --- Fixing The World    ;;
;; 25/3/15                                                   ;;
;; Brian Mc George - MCGBRI004 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;-------------------HELPER FUNCTIONS----------------------
;; ;print function for debugging purposes
(define (print . args)
  (cond ((not (null? args))
        (display (car args))
        (apply print (cdr args)))
  )
)

;; ;gets nth index of 0-indexed list. Can use list-ref instead
(define (index lst idx)
    (if (null? lst)
        lst
        (if (= idx 0)
            (car lst)
            (index (cdr lst) (- idx 1))
        )
    )
)
;; ;TESTS
;; ; (print (= 1 (index '(1 2 3 4 5) 0)) "\n")
;; ; (print (= 4 (index '(1 2 3 4 5) 3)) "\n")
;; ; (print (not (= 1 (index '(1 2 3 4 5) 2))) "\n")
;; ; (print (not (= 0 (index '(1 2 3 4 5) 0))) "\n")

;; ;checks if an item is in a list
;; You might want to do a more efficient version of this.
;;
(define (in item lst)
    (if (null? lst)
        #f
        (if (equal? item (car lst))
            #t
            (in item (cdr lst))
        )
    )
)
;; ;TESTS
;; ; (print (in 1 '(1 2 3)) "\n")
;; ; (print (in 2 '(1 2 3)) "\n")
;; ; (print (not (in 4 '(1 2 3))) "\n")
;; ; (print (in '(1 2) '((1 2) (3 4) 5)) "\n")

;; ;helper function for finding the length of a list
(define (lengthHelper n lst)
    (if (null? lst)
        n
        (lengthHelper (+ n 1) (cdr lst))
    )
)

;; ;finds length of a list
(define (length lst)
    (lengthHelper 0 lst)
)
;; ;TESTS
;; ; (print (= 4 (length '(1 2 3 4))) "\n")
;; ; (print (= 1 (length '(1))) "\n")
;; ; (print (= 2 (length '((1 2) (3 4)))) "\n")
;; ; (print (not (= 4 (length '(1 2 3 4 5)))) "\n")
;; ;-----------------------------------------------------------


;---------------------SOLVED STATES------------------------
;solved states of a 2x2x2 rubiks cube
(define solvedStates
    '(  ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
        ((3 1) (1 1) (4 1) (2 1) (7 3) (5 3) (8 3) (6 3))
        ((4 1) (3 1) (2 1) (1 1) (8 3) (7 3) (6 3) (5 3))
        ((2 1) (4 1) (1 1) (3 1) (6 3) (8 3) (5 3) (7 3))

        ((5 5) (1 6) (7 5) (3 6) (6 5) (2 6) (8 5) (4 6))
        ((7 5) (3 6) (8 5) (4 6) (5 5) (1 6) (6 5) (2 6))
        ((8 5) (4 6) (6 5) (2 6) (7 5) (3 6) (5 5) (1 6))
        ((6 5) (2 6) (5 5) (1 6) (8 5) (4 6) (7 5) (3 6))

        ((2 5) (6 6) (4 5) (8 6) (1 5) (5 6) (3 5) (7 6))
        ((4 5) (8 6) (3 5) (7 6) (2 5) (6 6) (1 5) (5 6))
        ((3 5) (7 6) (1 5) (5 6) (4 5) (8 6) (2 5) (6 6))
        ((1 5) (5 6) (2 5) (6 6) (3 5) (7 6) (4 5) (8 6))

        ((7 1) (8 1) (5 1) (6 1) (3 3) (4 3) (1 3) (2 3))
        ((5 1) (7 1) (6 1) (8 1) (1 3) (3 3) (2 3) (4 3))
        ((6 1) (5 1) (8 1) (7 1) (2 3) (1 3) (4 3) (3 3))
        ((8 1) (6 1) (7 1) (5 1) (4 3) (2 3) (3 3) (1 3))

        ((3 2) (4 2) (7 4) (8 4) (1 2) (2 2) (5 4) (6 4))
        ((1 2) (3 2) (5 4) (7 4) (2 2) (4 2) (6 4) (8 4))
        ((2 2) (1 2) (6 4) (5 4) (4 2) (3 2) (8 4) (7 4))
        ((4 2) (2 2) (8 4) (6 4) (3 2) (1 2) (7 4) (5 4))

        ((5 2) (6 2) (1 4) (2 4) (7 2) (8 2) (3 4) (4 4))
        ((7 2) (5 2) (3 4) (1 4) (8 2) (6 2) (4 4) (2 4))
        ((8 2) (7 2) (4 4) (3 4) (6 2) (5 2) (2 4) (1 4))
        ((6 2) (8 2) (2 4) (4 4) (5 2) (7 2) (1 4) (3 4))
    )
)
;; ;-----------------------------------------------------


;; ;---------------------QUESTION 1.1-----------------------
;; ;helper function for rotating the cube. Recalculates the various orientations
;; ;of the sub-cubes
(define (recalculateOrientation orientation axis)
    (cond
        [(= axis 0) ; x - rotation
            (if (> orientation 4)
                orientation
                (if(= orientation 4)
                    1
                    (+ orientation 1)
                )
            )
        ]
        [(= axis 1) ; y - rotation
            (if (or (= orientation 1) (= orientation 3))
                orientation
                (cond
                    [(= orientation 2) 6]
                    [(= orientation 4) 5]
                    [(= orientation 5) 2]
                    [(= orientation 6) 4]
                )
            )
        ]
        [(= axis 2) ; z - rotation
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 5]
                    [(= orientation 3) 6]
                    [(= orientation 5) 3]
                    [(= orientation 6) 1]
                )
            )
        ]
        [(= axis 3) ; X - rotation
            (if (> orientation 4)
                orientation
                (if(= orientation 1)
                    4 
                    (- orientation 1)
                )
            )
        ]
        [(= axis 4) ; Z - rotation
            (if (or (= orientation 2) (= orientation 4))
                orientation
                (cond
                    [(= orientation 1) 6]
                    [(= orientation 3) 5]
                    [(= orientation 5) 1]
                    [(= orientation 6) 3]
                )
            )
        ]
        [(= axis 5) ; Y - rotation
             (if (or (= orientation 1) (= orientation 3))
                 orientation
                 (cond
                     [(= orientation 2) 5]
                     [(= orientation 4) 6]
                     [(= orientation 5) 4]
                     [(= orientation 6) 2]
                 )
             )
        ]
 
    )
)
;; ; (print (= 2 (recalculateOrientation 1 0)) "\n")
;; ; (print (= 5 (recalculateOrientation 5 0)) "\n")
;; ; (print (= 1 (recalculateOrientation 1 1)) "\n")
;; ; (print (= 6 (recalculateOrientation 2 1)) "\n")
;; ; (print (= 5 (recalculateOrientation 1 2)) "\n")
;; ; (print (= 2 (recalculateOrientation 2 2)) "\n")

;rotations are performed using the left hand rule
; Rotates list in the positive x direction
(define (rotateXPos indexVal lst buildUpList) 
    (if(null? (index lst indexVal))
	    buildUpList
        (cond
            [(= indexVal 0) (rotateXPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 4) 0))))]
            [(= indexVal 2) (rotateXPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 0) 0))))]
            [(= indexVal 4) (rotateXPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 6) 0))))]
            [(= indexVal 6) (rotateXPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 2) 0))))]
			[else (rotateXPos (+ indexVal 1) lst (append buildUpList (list(index lst indexVal))))]	
		)
	)
)
;; ; (print (equal? (rotateXPos 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '()) '((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3))) "\n")
;; ; (print (equal? (rotateXPos 0 '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2)) '()) '((2 1) (1 6) (5 5) (4 1) (3 3) (8 3) (7 5) (6 2))) "\n")

; Rotates list in the negative x direction
(define (rotateXNeg indexVal lst buildUpList) 
    (if(null? (index lst indexVal))
	    buildUpList
        (cond
            [(= indexVal 0) (rotateXNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 2) 3))))]
            [(= indexVal 2) (rotateXNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 6) 3))))]
            [(= indexVal 4) (rotateXNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 0) 3))))]
            [(= indexVal 6) (rotateXNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 4) 3))))]
			[else (rotateXNeg (+ indexVal 1) lst (append buildUpList (list(index lst indexVal))))]	
		)
	)
)
;; ;(print (equal? (rotateXNeg 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '()) '((3 4) (2 1) (7 2) (4 1) (1 4) (6 3) (5 2) (8 3))) "\n")
;; ;(print (equal? (rotateXNeg 0 '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2)) '()) '((7 5) (1 6) (3 1) (4 1) (5 5) (8 3) (2 3) (6 2))) "\n")

; Rotates list in the positive y direction
(define (rotateYPos indexVal lst buildUpList) 
    (if(null? (index lst indexVal))
	    buildUpList
        (cond
            [(= indexVal 4) (rotateYPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 5) 1))))]
            [(= indexVal 5) (rotateYPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 7) 1))))]
            [(= indexVal 6) (rotateYPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 4) 1))))]
            [(= indexVal 7) (rotateYPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 6) 1))))]
			[else (rotateYPos (+ indexVal 1) lst (append buildUpList (list(index lst indexVal))))]	
		)
	)
)
;; ;(print (equal? (rotateYPos 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '()) '((1 1) (2 1) (3 1) (4 1) (6 3) (8 3) (5 3) (7 3))) "\n")
;; ;(print (equal? (rotateYPos 0 '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2)) '()) '((5 5) (1 6) (7 5) (4 1) (8 3) (6 6) (2 5) (3 6))) "\n")

; Rotates list in the negative y direction
(define (rotateYNeg indexVal lst buildUpList) 
    (if(null? (index lst indexVal))
	    buildUpList
        (cond
            [(= indexVal 4) (rotateYNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 6) 5))))]
            [(= indexVal 5) (rotateYNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 4) 5))))]
            [(= indexVal 6) (rotateYNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 7) 5))))]
            [(= indexVal 7) (rotateYNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 5) 5))))]
			[else (rotateYNeg (+ indexVal 1) lst (append buildUpList (list(index lst indexVal))))]	
		)
	)
)
;  (print (equal? (rotateYNeg 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '()) '((1 1) (2 1) (3 1) (4 1) (7 3) (5 3) (8 3) (6 3))) "\n")
;  (print (equal? (rotateYNeg 0 '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2)) '()) '((5 5) (1 6) (7 5) (4 1) (3 5) (2 6) (6 5) (8 3))) "\n")

; Rotates list in the positive z direction
(define (rotateZPos indexVal lst buildUpList) 
    (if(null? (index lst indexVal))
	    buildUpList
        (cond
            [(= indexVal 0) (rotateZPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 1) 2))))]
            [(= indexVal 1) (rotateZPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 5) 2))))]
            [(= indexVal 4) (rotateZPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 0) 2))))]
            [(= indexVal 5) (rotateZPos (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 4) 2))))]
			[else (rotateZPos (+ indexVal 1) lst (append buildUpList (list(index lst indexVal))))]	
		)
	)
)
;; ; (print (equal? (rotateZPos 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '()) '((2 5) (6 6) (3 1) (4 1) (1 5) (5 6) (7 3) (8 3))) "\n")
;; ; (print (equal? (rotateZPos 0 '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2)) '()) '((1 1) (8 6) (7 5) (4 1) (5 3) (2 4) (3 2) (6 2))) "\n")

; Rotates list in the negative z direction
(define (rotateZNeg indexVal lst buildUpList) 
    (if(null? (index lst indexVal))
	    buildUpList
        (cond
            [(= indexVal 0) (rotateZNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 4) 4))))]
            [(= indexVal 1) (rotateZNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 0) 4))))]
            [(= indexVal 4) (rotateZNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 5) 4))))]
            [(= indexVal 5) (rotateZNeg (+ indexVal 1) lst (append buildUpList (list (createPair (index lst 1) 4))))]
			[else (rotateZNeg (+ indexVal 1) lst (append buildUpList (list(index lst indexVal))))]	
		)
	)
)
;; ;(print (equal? (rotateYNeg 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '()) '((1 1) (2 1) (3 1) (4 1) (7 3) (5 3) (8 3) (6 3))) "\n")
;; ;(print (equal? (rotateYNeg 0 '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2)) '()) '((5 5) (1 6) (7 5) (4 1) (3 5) (2 6) (6 5) (8 3))) "\n")
 
; Skeleton code defined helper method
(define(createPair miniList axis)
	(list (car miniList) (recalculateOrientation (car (cdr miniList)) axis))
)

;rotates left 4 cubes along x axis
 (define (rotateX ispositive state)
     (if ispositive
         (list (rotateXPos 0 state (list)) (list "x"))
         (list (rotateXNeg 0 state (list)) (list "X"))
     )
 )
;; ;(print (equal? (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) '(((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3)) ("x"))) "\n")
;; ;(print (equal? (rotateX #t '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2))) '(((2 1) (1 6) (5 5) (4 1) (3 3) (8 3) (7 5) (6 2)) ("x"))) "\n")
;; ;(print (equal? (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) '(((3 4) (2 1) (7 2) (4 1) (1 4) (6 3) (5 2) (8 3)) ("X"))) "\n")
;; ;(print (equal? (rotateX #f '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2))) '(((7 5) (1 6) (3 1) (4 1) (5 5) (8 3) (2 3) (6 2)) ("X"))) "\n")

;rotates bottom 4 cubes along y axis
(define (rotateY ispositive state)
    (if ispositive
        (list (rotateYPos 0 state (list)) (list "y"))
        (list (rotateYNeg 0 state (list)) (list "Y"))
    )
)
;; ;(print (equal? (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) '(((1 1) (2 1) (3 1) (4 1) (6 3) (8 3) (5 3) (7 3)) ("y"))) "\n")
;; ;(print (equal? (rotateY #t '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2))) '(((5 5) (1 6) (7 5) (4 1) (8 3) (6 6) (2 5) (3 6)) ("y"))) "\n") 
;; ;(print (equal? (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) '(((1 1) (2 1) (3 1) (4 1) (7 3) (5 3) (8 3) (6 3)) ("Y"))) "\n")
;; ;(print (equal? (rotateY #f '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2))) '(((5 5) (1 6) (7 5) (4 1) (3 5) (2 6) (6 5) (8 3)) ("Y"))) "\n")

;rotates back 4 cubes along z axis
(define (rotateZ ispositive state)
    (if ispositive
        (list (rotateZPos 0 state (list)) (list "z"))
        (list (rotateZNeg 0 state (list)) (list "Z"))
    )
)
;; ; (print (equal? (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) '(((2 5) (6 6) (3 1) (4 1) (1 5) (5 6) (7 3) (8 3)) ("z"))) "\n")
;; ; (print (equal? (rotateZ #t '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2))) '(((1 1) (8 6) (7 5) (4 1) (5 3) (2 4) (3 2) (6 2)) ("z"))) "\n")
;; ; (print (equal? (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) '(((5 5) (1 6) (3 1) (4 1) (6 5) (2 6) (7 3) (8 3)) ("Z"))) "\n")
;; ; (print (equal? (rotateZ #f '((5 5) (1 6) (7 5) (4 1) (2 4) (8 3) (3 2) (6 2))) '(((2 4) (5 1) (7 5) (4 1) (8 5) (1 3) (3 2) (6 2)) ("Z"))) "\n")

;; ;helper for rotate function
(define (rotateHelper char state)
    (cond
        [(char=? char #\x) (car (rotateX #t state))]
        [(char=? char #\X) (car (rotateX #f state))]
        [(char=? char #\y) (car (rotateY #t state))]
        [(char=? char #\Y) (car (rotateY #f state))]
        [(char=? char #\z) (car (rotateZ #t state))]
        [(char=? char #\Z) (car (rotateZ #f state))]
    )
)

;; ;parses a string for rotations
(define (rotate rotations state)
    (if (= (string-length rotations) 0)
        state
        (rotate (substring rotations 1 (string-length rotations)) (rotateHelper (string-ref rotations 0) state))
    )
)
;; ; (print (equal? (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))) "\n")
;; ; (print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateY #t (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "xXx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t (car (rotateX #f (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "yXz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateX #f (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #f (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")

;-----------------------QUESTION 1.2-----------------------
; Generates the successor states of the current given rubiks cube state
; Non-Optimised version - generates 6 new states each time it is called
(define (generateSuccessorStates state prevMoves)
     (list
         (list
             (rotate "x" state)
             (rotate "X" state)
             (rotate "y" state)
             (rotate "Y" state)
             (rotate "z" state)
             (rotate "Z" state)
         )
         (list
             (append prevMoves '("x"))
             (append prevMoves '("X"))
             (append prevMoves '("y"))
             (append prevMoves '("Y"))
             (append prevMoves '("z"))
             (append prevMoves '("Z"))
         )
     )
 )
; generateSuccessorStates tests
;; ; (print (equal? (generateSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;; ;         (list
;; ;             (list
;; ;                 (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                 (car (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;             )
;; ;             '(("x") ("X") ("y") ("Y") ("z") ("Z"))
;; ;         )
;; ;     )
;; ; "\n")

; Generates successor state list that does not include rotations that undo last move
; Optimised version - generates 5 states if prevMoves not empty or 6 states if it is empty
(define (generateOptimisedSuccessorStates state prevMoves)
    (if (= (length prevMoves) 0)
        (generateSuccessorStates state prevMoves)  
        (let ([lastMove (index prevMoves (- (length prevMoves) 1))])
            (buildUpSuccessor (list) (list) state prevMoves #\x lastMove) 
        )
    )
)

; Builds up a state list and move list recursively
; Helper function for generateOptimisedSuccessorStates 
(define (buildUpSuccessor buildUpStateList buildUpMoveList state moveList char lastMove)
    (cond
        [(char=? char #\x)
            (cond 
                [(equal? lastMove "X")
                    (buildUpSuccessor buildUpStateList buildUpMoveList state moveList #\X lastMove)
                ]
                [else
                    (buildUpSuccessor (append buildUpStateList (list (rotate "x" state))) (append buildUpMoveList (list (append moveList '("x")))) state moveList #\X lastMove)                    
                ]
            )
        ]
        [(char=? char #\X)
            (cond
                 [(equal? lastMove "x")
                     (buildUpSuccessor buildUpStateList buildUpMoveList state moveList #\y lastMove)
                 ]
                 [else
                     (buildUpSuccessor (append buildUpStateList (list (rotate "X" state))) (append buildUpMoveList (list (append moveList '("X")))) state moveList #\y lastMove)
                 ]
             )
        ]
        [(char=? char #\y)
            (cond
                 [(equal? lastMove "Y")
                     (buildUpSuccessor buildUpStateList buildUpMoveList state moveList #\Y lastMove)
                 ]
                 [else
                     (buildUpSuccessor (append buildUpStateList (list (rotate "y" state))) (append buildUpMoveList (list (append moveList '("y")))) state moveList #\Y lastMove)
                 ]
             )
        ]
        [(char=? char #\Y)
            (cond
                  [(equal? lastMove "y")
                      (buildUpSuccessor buildUpStateList buildUpMoveList state moveList #\z lastMove)
                  ]
                  [else
                      (buildUpSuccessor (append buildUpStateList (list (rotate "Y" state))) (append buildUpMoveList (list (append moveList '("Y")))) state moveList #\z lastMove)
                  ]
             ) 
        ]
        [(char=? char #\z)
            (cond
                   [(equal? lastMove "Z")
                       (buildUpSuccessor buildUpStateList buildUpMoveList state moveList #\Z lastMove)
                   ]
                   [else
                       (buildUpSuccessor (append buildUpStateList (list (rotate "z" state))) (append buildUpMoveList (list (append moveList '("z")))) state moveList #\Z lastMove)
                   ]
            )
        ]
        [(char=? char #\Z)
            (cond
                    [(equal? lastMove "z")
                        (list buildUpStateList buildUpMoveList)
                    ]
                    [else
                        (list (append buildUpStateList (list (rotate "Z" state))) (append buildUpMoveList (list (append moveList '("Z")))))
                    ]
             )     
        ]
                                 
    )
)
; generateOptimisedSuccessorStates tests
;; ;It generates all states if previous moves is empty
;; ;(print (equal? (generateOptimisedSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;; ;        (list
;; ;            (list
;; ;                (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateZ #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;            )
;; ;            '(("x") ("X") ("y") ("Y") ("z") ("Z"))
;; ;        )
;; ;    )
;; ;"\n")
;; ;
;; ;It does not generate the state if its rotation will undo the last move
;; ;(print (equal? (generateOptimisedSuccessorStates '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '("x" "y" "z"))
;; ;        (list
;; ;            (list
;; ;                (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateX #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateY #f '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;                (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))
;; ;            )
;; ;            '(("x" "y" "z" "x") ("x" "y" "z" "X") ("x" "y" "z" "y") ("x" "y" "z" "Y") ("x" "y" "z" "z"))
;; ;        )
;; ;     )
;; ; "\n")

;-----------------------------QUESTION 2.1--------------------------
;----------------Non Tail Recursive Non-Optimised (medium speed)------------
; This method is NOT tail recursive by design!
; In trying to get the fastest solution, this way of doing it resulted in much much faster results (the theoretical questions has a comparision for n=7) at the expense of bit more memory usage
; Second solution obtained
(define (genStates n state moves)
     (cond
         [(<= n 0) (list (list state) (list moves))]
         [(= n 1) (generateSuccessorStates state moves)]
         [(> n 1)
             (let ((successorStatesSolns (generateSuccessorStates state moves)))
                 (let ((newStates (car successorStatesSolns)) (newMoves (car (cdr successorStatesSolns))))
                         (let
                             (
                                 (sol1 (genStates (- n 1) (list-ref newStates 0) (list-ref newMoves 0)))
                                 (sol2 (genStates (- n 1) (list-ref newStates 1) (list-ref newMoves 1)))
                                 (sol3 (genStates (- n 1) (list-ref newStates 2) (list-ref newMoves 2)))
                                 (sol4 (genStates (- n 1) (list-ref newStates 3) (list-ref newMoves 3)))
                                 (sol5 (genStates (- n 1) (list-ref newStates 4) (list-ref newMoves 4)))
                                 (sol6 (genStates (- n 1) (list-ref newStates 5) (list-ref newMoves 5)))
                             )
                             (list
                                 (append (car sol1) (car sol2) (car sol3) (car sol4) (car sol5) (car sol6))
                                 (append (car (cdr sol1)) (car (cdr sol2)) (car (cdr sol3)) (car (cdr sol4)) (car (cdr sol5)) (car (cdr sol6)))
                             )
                         )
                     )
               )

         ]
     )
 )
;; ;  Behaves correctly for depth 0
 ;; ;  (print (equal? (genStates 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
 ;; ;      '((((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (()))
 ;; ;      )
 ;; ;  "\n")
 ;; ;
;  Behaves correctly for depth 2
 ;; ;  (print (equal? (genStates 2 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
 ;; ;      '((((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
 ;; ;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
 ;; ;         ((5 4) (2 1) (1 2) (4 1) (6 3) (8 3) (7 5) (3 6))
 ;; ;         ((5 4) (2 1) (1 2) (4 1) (3 5) (7 6) (8 3) (6 3))
 ;; ;         ((2 5) (6 6) (1 2) (4 1) (5 4) (7 4) (3 2) (8 3))
 ;; ;         ((7 4) (5 4) (1 2) (4 1) (6 5) (2 6) (3 2) (8 3))
 ;; ;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
 ;; ;         ((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
 ;; ;         ((3 4) (2 1) (7 2) (4 1) (6 3) (8 3) (1 5) (5 6))
 ;; ;         ((3 4) (2 1) (7 2) (4 1) (5 5) (1 6) (8 3) (6 3))
 ;; ;         ((2 5) (6 6) (7 2) (4 1) (3 4) (1 4) (5 2) (8 3))
 ;; ;         ((1 4) (3 4) (7 2) (4 1) (6 5) (2 6) (5 2) (8 3))
 ;; ;         ((6 4) (2 1) (1 2) (4 1) (5 4) (8 3) (3 2) (7 3))
 ;; ;         ((3 4) (2 1) (5 2) (4 1) (1 4) (8 3) (6 2) (7 3))
 ;; ;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
 ;; ;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
 ;; ;         ((2 5) (8 6) (3 1) (4 1) (1 5) (6 6) (5 3) (7 3))
 ;; ;         ((6 5) (1 6) (3 1) (4 1) (8 5) (2 6) (5 3) (7 3))
 ;; ;         ((7 4) (2 1) (1 2) (4 1) (8 4) (5 3) (3 2) (6 3))
 ;; ;         ((3 4) (2 1) (8 2) (4 1) (1 4) (5 3) (7 2) (6 3))
 ;; ;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
 ;; ;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
 ;; ;         ((2 5) (5 6) (3 1) (4 1) (1 5) (7 6) (8 3) (6 3))
 ;; ;         ((7 5) (1 6) (3 1) (4 1) (5 5) (2 6) (8 3) (6 3))
 ;; ;         ((1 5) (6 6) (2 5) (4 1) (7 4) (5 6) (3 2) (8 3))
 ;; ;         ((3 4) (6 6) (7 2) (4 1) (2 5) (5 6) (1 5) (8 3))
 ;; ;         ((2 5) (6 6) (3 1) (4 1) (5 4) (8 3) (1 2) (7 3))
 ;; ;         ((2 5) (6 6) (3 1) (4 1) (7 3) (1 4) (8 3) (5 2))
 ;; ;         ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3))
 ;; ;        ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
 ;; ;        ((6 5) (1 6) (5 5) (4 1) (7 4) (2 6) (3 2) (8 3))
 ;; ;        ((3 4) (1 6) (7 2) (4 1) (5 5) (2 6) (6 5) (8 3))
 ;; ;        ((5 5) (1 6) (3 1) (4 1) (2 4) (8 3) (6 2) (7 3))
 ;; ;        ((5 5) (1 6) (3 1) (4 1) (7 3) (6 4) (8 3) (2 2))
 ;; ;        ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
 ;; ;        ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3)))
 ;; ;        (("x" "x")
 ;; ;         ("x" "X")
 ;; ;         ("x" "y")
 ;; ;         ("x" "Y")
 ;; ;         ("x" "z")
 ;; ;         ("x" "Z")
 ;; ;         ("X" "x")
 ;; ;         ("X" "X")
 ;; ;         ("X" "y")
 ;; ;         ("X" "Y")
 ;; ;         ("X" "z")
 ;; ;         ("X" "Z")
 ;; ;         ("y" "x")
 ;; ;         ("y" "X")
 ;; ;         ("y" "y")
 ;; ;         ("y" "Y")
 ;; ;         ("y" "z")
 ;; ;         ("y" "Z")
 ;; ;         ("Y" "x")
 ;; ;         ("Y" "X")
 ;; ;         ("Y" "y")
 ;; ;         ("Y" "Y")
 ;; ;         ("Y" "z")
 ;; ;         ("z" "x")
 ;; ;         ("z" "X")
 ;; ;         ("z" "y")
 ;; ;         ("z" "Y")
 ;; ;         ("z" "z")
 ;; ;         ("z" "Z")
 ;; ;         ("Z" "x")
 ;; ;         ("Z" "X")
 ;; ;         ("Z" "y")
 ;; ;         ("Z" "Y")
 ;; ;         ("Z" "z")
 ;; ;         ("Z" "Z")))
 ;; ;     )
 ;; ; "\n")

;-----------------------------Non-Optimised (slow)--------------------------
; finds all the states at a specific depth
; Non-Optimised, tail recursive genStates method
; This was the initial solution but had a high completion time
(define (genStatesTailRecursive n state moves)
    (if (= n 0)
        (list (list state) (list moves))
        (buildListController (list state) (list moves) 0 n (list) (list))
    )
)

; Creates a list of states and moves up to 'n' depth
; Helper function of genStates
(define (buildListController stateList moveList currentIndex totalIndex buildUpList buildUpMove)
    (if (= currentIndex totalIndex)
         (list stateList moveList)
         (cond
            [(null? stateList)
				(buildListController buildUpList buildUpMove (+ currentIndex 1) totalIndex (list) (list))
            ]
            [else (buildUp stateList moveList currentIndex totalIndex buildUpList buildUpMove)]
         )
    )
)
 ;  buildListController behaves correctly for n=1
 ;; ;(print (equal? (buildListController (list '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (list '()) 0 1 (list) (list))
 ;; ; '((((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3))
;; ;  ((3 4) (2 1) (7 2) (4 1) (1 4) (6 3) (5 2) (8 3))
 ;; ;  ((1 1) (2 1) (3 1) (4 1) (6 3) (8 3) (5 3) (7 3))
 ;; ;  ((1 1) (2 1) (3 1) (4 1) (7 3) (5 3) (8 3) (6 3))
 ;; ;  ((2 5) (6 6) (3 1) (4 1) (1 5) (5 6) (7 3) (8 3))
 ;; ;  ((5 5) (1 6) (3 1) (4 1) (6 5) (2 6) (7 3) (8 3)))
 ;; ; (("x") ("X") ("y") ("Y") ("z") ("Z")))
 ;; ;      )
 ;; ;  "\n")


; Populates a buildUpList and buildUpMove list at a single depth
; Helper function of genStatesTailRecursive
(define (buildUp stateList moveList currentIndex totalIndex buildUpList buildUpMove)
    (define successorStates (generateSuccessorStates (car stateList) (car moveList)))
        (buildListController (cdr stateList) (cdr moveList) currentIndex totalIndex (append buildUpList (car successorStates)) (append buildUpMove (car (cdr successorStates))))
)
; Tests not provided for buildUp as genStatesTailRecursive and buildListController tests will ensure that buildUp is performing correctly

;  genStatesTailRecursive Tests
;; ;  Behaves correctly for depth 0
;; ;  (print (equal? (genStatesTailRecursive 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;; ;      '((((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (()))
;; ;      )
;; ;  "\n")
;; ;
;  Behaves correctly for depth 2
;; ;  (print (equal? (genStatesTailRecursive 2 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;; ;      '((((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
;; ;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;; ;         ((5 4) (2 1) (1 2) (4 1) (6 3) (8 3) (7 5) (3 6))
;; ;         ((5 4) (2 1) (1 2) (4 1) (3 5) (7 6) (8 3) (6 3))
;; ;         ((2 5) (6 6) (1 2) (4 1) (5 4) (7 4) (3 2) (8 3))
;; ;         ((7 4) (5 4) (1 2) (4 1) (6 5) (2 6) (3 2) (8 3))
;; ;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;; ;         ((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
;; ;         ((3 4) (2 1) (7 2) (4 1) (6 3) (8 3) (1 5) (5 6))
;; ;         ((3 4) (2 1) (7 2) (4 1) (5 5) (1 6) (8 3) (6 3))
;; ;         ((2 5) (6 6) (7 2) (4 1) (3 4) (1 4) (5 2) (8 3))
;; ;         ((1 4) (3 4) (7 2) (4 1) (6 5) (2 6) (5 2) (8 3))
;; ;         ((6 4) (2 1) (1 2) (4 1) (5 4) (8 3) (3 2) (7 3))
;; ;         ((3 4) (2 1) (5 2) (4 1) (1 4) (8 3) (6 2) (7 3))
;; ;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
;; ;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;; ;         ((2 5) (8 6) (3 1) (4 1) (1 5) (6 6) (5 3) (7 3))
;; ;         ((6 5) (1 6) (3 1) (4 1) (8 5) (2 6) (5 3) (7 3))
;; ;         ((7 4) (2 1) (1 2) (4 1) (8 4) (5 3) (3 2) (6 3))
;; ;         ((3 4) (2 1) (8 2) (4 1) (1 4) (5 3) (7 2) (6 3))
;; ;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;; ;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
;; ;         ((2 5) (5 6) (3 1) (4 1) (1 5) (7 6) (8 3) (6 3))
;; ;         ((7 5) (1 6) (3 1) (4 1) (5 5) (2 6) (8 3) (6 3))
;; ;         ((1 5) (6 6) (2 5) (4 1) (7 4) (5 6) (3 2) (8 3))
;; ;         ((3 4) (6 6) (7 2) (4 1) (2 5) (5 6) (1 5) (8 3))
;; ;         ((2 5) (6 6) (3 1) (4 1) (5 4) (8 3) (1 2) (7 3))
;; ;         ((2 5) (6 6) (3 1) (4 1) (7 3) (1 4) (8 3) (5 2))
;; ;         ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3))
;; ;        ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;; ;        ((6 5) (1 6) (5 5) (4 1) (7 4) (2 6) (3 2) (8 3))
;; ;        ((3 4) (1 6) (7 2) (4 1) (5 5) (2 6) (6 5) (8 3))
;; ;        ((5 5) (1 6) (3 1) (4 1) (2 4) (8 3) (6 2) (7 3))
;; ;        ((5 5) (1 6) (3 1) (4 1) (7 3) (6 4) (8 3) (2 2))
;; ;        ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;; ;        ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3)))
;; ;        (("x" "x")
;; ;         ("x" "X")
;; ;         ("x" "y")
;; ;         ("x" "Y")
;; ;         ("x" "z")
;; ;         ("x" "Z")
;; ;         ("X" "x")
;; ;         ("X" "X")
;; ;         ("X" "y")
;; ;         ("X" "Y")
;; ;         ("X" "z")
;; ;         ("X" "Z")
;; ;         ("y" "x")
;; ;         ("y" "X")
;; ;         ("y" "y")
;; ;         ("y" "Y")
;; ;         ("y" "z")
;; ;         ("y" "Z")
;; ;         ("Y" "x")
;; ;         ("Y" "X")
;; ;         ("Y" "y")
;; ;         ("Y" "Y")
;; ;         ("Y" "z")
;; ;         ("Y" "Z")
;; ;         ("z" "x")
;; ;         ("z" "X")
;; ;         ("z" "y")
;; ;         ("z" "Y")
;; ;         ("z" "z")
;; ;         ("z" "Z")
;; ;         ("Z" "x")
;; ;         ("Z" "X")
;; ;         ("Z" "y")
;; ;         ("Z" "Y")
;; ;         ("Z" "z")
;; ;         ("Z" "Z")))    
;; ;     )
;; ; "\n")

;-----------------------------Optimised genStates (fastest)--------------------------
; This method is NOT tail recursive by design!
; In trying to get the fastest solution, this way of doing it resulted in much much faster results (the theoretical questions has a comparision for n=7) at the expense of bit more memory usage
(define (genStatesOptimised n state moves)
    (cond
        [(<= n 0) (list (list state) (list moves))]
        [(= n 1) (generateOptimisedSuccessorStates state moves)]
        [(> n 1)
            (let ((successorStatesSolns (generateOptimisedSuccessorStates state moves)))
                (let ((newStates (car successorStatesSolns)) (newMoves (car (cdr successorStatesSolns))))
                    (if (= (length newStates) 6)
                        (let 
                            (
                                 (sol1 (genStatesOptimised (- n 1) (list-ref newStates 0) (list-ref newMoves 0)))
                                 (sol2 (genStatesOptimised (- n 1) (list-ref newStates 1) (list-ref newMoves 1)))
                                 (sol3 (genStatesOptimised (- n 1) (list-ref newStates 2) (list-ref newMoves 2)))
                                 (sol4 (genStatesOptimised (- n 1) (list-ref newStates 3) (list-ref newMoves 3)))
                                 (sol5 (genStatesOptimised (- n 1) (list-ref newStates 4) (list-ref newMoves 4)))
                                 (sol6 (genStatesOptimised (- n 1) (list-ref newStates 5) (list-ref newMoves 5)))
                             )
                             (list
                                 (append (car sol1) (car sol2) (car sol3) (car sol4) (car sol5) (car sol6))
                                 (append (car (cdr sol1)) (car (cdr sol2)) (car (cdr sol3)) (car (cdr sol4)) (car (cdr sol5)) (car (cdr sol6)))
                             )
                        )
                        (let
                             (
                                 (sol1 (genStatesOptimised (- n 1) (list-ref newStates 0) (list-ref newMoves 0)))
                                 (sol2 (genStatesOptimised (- n 1) (list-ref newStates 1) (list-ref newMoves 1)))
                                 (sol3 (genStatesOptimised (- n 1) (list-ref newStates 2) (list-ref newMoves 2)))
                                 (sol4 (genStatesOptimised (- n 1) (list-ref newStates 3) (list-ref newMoves 3)))
                                 (sol5 (genStatesOptimised (- n 1) (list-ref newStates 4) (list-ref newMoves 4)))
                             )
                             (list
                                 (append (car sol1) (car sol2) (car sol3) (car sol4) (car sol5))
                                 (append (car (cdr sol1)) (car (cdr sol2)) (car (cdr sol3)) (car (cdr sol4)) (car (cdr sol5)))
                             )
                         )
                    )
                )
            )  
        
        ]
    )
)
 ; genStatesOptimised Tests
 ;  Behaves correctly for depth 0
 ;;  (print (equal? (genStatesOptimised 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
 ;;      '((((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (()))
 ;;      )
 ;;  "\n")
 ;;
 ;  Behaves correctly for depth 2
 ;;  (print (equal? (genStatesOptimised 2 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
 ;;      '((((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
 ;;         ((5 4) (2 1) (1 2) (4 1) (6 3) (8 3) (7 5) (3 6))
 ;;         ((5 4) (2 1) (1 2) (4 1) (3 5) (7 6) (8 3) (6 3))
 ;;         ((2 5) (6 6) (1 2) (4 1) (5 4) (7 4) (3 2) (8 3))
 ;;         ((7 4) (5 4) (1 2) (4 1) (6 5) (2 6) (3 2) (8 3))
 ;;         ((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
 ;;         ((3 4) (2 1) (7 2) (4 1) (6 3) (8 3) (1 5) (5 6))
 ;;         ((3 4) (2 1) (7 2) (4 1) (5 5) (1 6) (8 3) (6 3))
 ;;         ((2 5) (6 6) (7 2) (4 1) (3 4) (1 4) (5 2) (8 3))
 ;;         (1 4) (3 4) (7 2) (4 1) (6 5) (2 6) (5 2) (8 3))
 ;;         (6 4) (2 1) (1 2) (4 1) (5 4) (8 3) (3 2) (7 3))
 ;;         (3 4) (2 1) (5 2) (4 1) (1 4) (8 3) (6 2) (7 3))
 ;;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
 ;;         ((2 5) (8 6) (3 1) (4 1) (1 5) (6 6) (5 3) (7 3))
 ;;         ((6 5) (1 6) (3 1) (4 1) (8 5) (2 6) (5 3) (7 3))
 ;;         ((7 4) (2 1) (1 2) (4 1) (8 4) (5 3) (3 2) (6 3))
 ;;         ((3 4) (2 1) (8 2) (4 1) (1 4) (5 3) (7 2) (6 3))
 ;;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
 ;;         (2 5) (5 6) (3 1) (4 1) (1 5) (7 6) (8 3) (6 3))
 ;;         ((7 5) (1 6) (3 1) (4 1) (5 5) (2 6) (8 3) (6 3))
 ;;         ((1 5) (6 6) (2 5) (4 1) (7 4) (5 6) (3 2) (8 3))
 ;;         ((3 4) (6 6) (7 2) (4 1) (2 5) (5 6) (1 5) (8 3))
 ;;         ((2 5) (6 6) (3 1) (4 1) (5 4) (8 3) (1 2) (7 3))
 ;;         ((2 5) (6 6) (3 1) (4 1) (7 3) (1 4) (8 3) (5 2))
 ;;         ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3))
 ;;         ((6 5) (1 6) (5 5) (4 1) (7 4) (2 6) (3 2) (8 3))
 ;;         ((3 4) (1 6) (7 2) (4 1) (5 5) (2 6) (6 5) (8 3))
 ;;         ((5 5) (1 6) (3 1) (4 1) (2 4) (8 3) (6 2) (7 3))
 ;;         ((5 5) (1 6) (3 1) (4 1) (7 3) (6 4) (8 3) (2 2))
 ;;         ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3)))
 ;;         (("x" "x")
 ;;         ("x" "y")
 ;;         ("x" "Y")
 ;;         ("x" "z")
 ;;         ("x" "Z")
 ;;         ("X" "X")
 ;;         ("X" "y")
 ;;         ("X" "Y")
 ;;         ("X" "z")
 ;;         ("X" "Z")
 ;;         ("y" "x")
 ;;         ("y" "X")
 ;;         ("y" "y")
 ;;         ("y" "z")
 ;;         ("y" "Z")
 ;;         ("Y" "x")
 ;;         ("Y" "X")
 ;;         ("Y" "Y")
 ;;         ("Y" "z")
 ;;         ("Y" "Z")
 ;;         ("z" "x")
 ;;         ("z" "X")
 ;;         ("z" "y")
 ;;         ("z" "Y")
 ;;         ("z" "z")
 ;;         ("Z" "x")
 ;;         ("Z" "X")
 ;;         ("Z" "y")
 ;;         ("Z" "Y")
 ;;         ("Z" "Z")))
 ;;      )
 ;;  "\n")
 ;;

;-----------------------------Optimised Version of Non-Optimised genStatesTailRecursive (Not that fast)--------------------------
;finds all the states at a specific depth using generateOptimisedSuccessorStates
(define (genStatesOptimisedTailRecursive n state moves)
     (if (= n 0)
        (list (list state) (list moves))
        (optimisedBuildListController (list state) (list moves) 0 n (list) (list))
     )
)

 ; Creates a list of states and moves up to 'n' depth
 ; Helper function ofr genStatesOptimisedTailRecursive
(define (optimisedBuildListController stateList moveList currentIndex totalIndex buildUpList buildUpMove)
    (if (= currentIndex totalIndex)
         (list stateList moveList)
         (cond
            [(not (null? stateList))
                (buildUpOptimised stateList moveList currentIndex totalIndex buildUpList buildUpMove)
            ]
            [else (optimisedBuildListController buildUpList buildUpMove (+ currentIndex 1) totalIndex (list) (list))]
         )
    )
)
; optimisedBuildListController behaves correctly for n=1
;; ;(print (equal? (optimisedBuildListController (list '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (list '()) 0 1 (list) (list))
;; ; '((((5 4) (2 1) (1 2) (4 1) (7 4) (6 3) (3 2) (8 3))
;; ;  ((3 4) (2 1) (7 2) (4 1) (1 4) (6 3) (5 2) (8 3))
;; ;  ((1 1) (2 1) (3 1) (4 1) (6 3) (8 3) (5 3) (7 3))
;; ;  ((1 1) (2 1) (3 1) (4 1) (7 3) (5 3) (8 3) (6 3))
;; ;  ((2 5) (6 6) (3 1) (4 1) (1 5) (5 6) (7 3) (8 3))
;; ;  ((5 5) (1 6) (3 1) (4 1) (6 5) (2 6) (7 3) (8 3)))
;; ; (("x") ("X") ("y") ("Y") ("z") ("Z")))
;; ;      )
;; ;  "\n")



 ; Populates a buildUpList and buildUpMove list at a single depth
 ; Helper function for genStatesOptimisedTailRecursive
(define (buildUpOptimised stateList moveList currentIndex totalIndex buildUpList buildUpMove)
    (define successorStates (generateOptimisedSuccessorStates (car stateList) (car moveList)))
        (optimisedBuildListController (cdr stateList) (cdr moveList) currentIndex totalIndex (append buildUpList (car successorStates)) (append buildUpMove (car (cdr successorStates))))
)
; Tests not provided for buildUpOptimised as genStatesOptimisedTailRecursive and optimisedBuildListController tests will ensure that buildUpOptimised is performing correctly 

;   genStatesOptimisedTailRecursive Tests
;   Behaves correctly for depth 0
;;  (print (equal? (genStatesOptimisedTailRecursive 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;;      '((((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (()))
;;      )
;;  "\n")
;;
;   Behaves correctly for depth 2
;;  (print (equal? (genStatesOptimisedTailRecursive 2 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;;      '((((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
;;         ((5 4) (2 1) (1 2) (4 1) (6 3) (8 3) (7 5) (3 6))
;;         ((5 4) (2 1) (1 2) (4 1) (3 5) (7 6) (8 3) (6 3))
;;         ((2 5) (6 6) (1 2) (4 1) (5 4) (7 4) (3 2) (8 3))
;;         ((7 4) (5 4) (1 2) (4 1) (6 5) (2 6) (3 2) (8 3))
;;         ((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
;;         ((3 4) (2 1) (7 2) (4 1) (6 3) (8 3) (1 5) (5 6))
;;         ((3 4) (2 1) (7 2) (4 1) (5 5) (1 6) (8 3) (6 3))
;;         ((2 5) (6 6) (7 2) (4 1) (3 4) (1 4) (5 2) (8 3))
;;         (1 4) (3 4) (7 2) (4 1) (6 5) (2 6) (5 2) (8 3))
;;         (6 4) (2 1) (1 2) (4 1) (5 4) (8 3) (3 2) (7 3))
;;         (3 4) (2 1) (5 2) (4 1) (1 4) (8 3) (6 2) (7 3))
;;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
;;         ((2 5) (8 6) (3 1) (4 1) (1 5) (6 6) (5 3) (7 3))
;;         ((6 5) (1 6) (3 1) (4 1) (8 5) (2 6) (5 3) (7 3))
;;         ((7 4) (2 1) (1 2) (4 1) (8 4) (5 3) (3 2) (6 3))
;;         ((3 4) (2 1) (8 2) (4 1) (1 4) (5 3) (7 2) (6 3))
;;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
;;         (2 5) (5 6) (3 1) (4 1) (1 5) (7 6) (8 3) (6 3))
;;         ((7 5) (1 6) (3 1) (4 1) (5 5) (2 6) (8 3) (6 3))
;;         ((1 5) (6 6) (2 5) (4 1) (7 4) (5 6) (3 2) (8 3))
;;         ((3 4) (6 6) (7 2) (4 1) (2 5) (5 6) (1 5) (8 3))
;;         ((2 5) (6 6) (3 1) (4 1) (5 4) (8 3) (1 2) (7 3))
;;         ((2 5) (6 6) (3 1) (4 1) (7 3) (1 4) (8 3) (5 2))
;;         ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3))
;;         ((6 5) (1 6) (5 5) (4 1) (7 4) (2 6) (3 2) (8 3))
;;         ((3 4) (1 6) (7 2) (4 1) (5 5) (2 6) (6 5) (8 3))
;;         ((5 5) (1 6) (3 1) (4 1) (2 4) (8 3) (6 2) (7 3))
;;         ((5 5) (1 6) (3 1) (4 1) (7 3) (6 4) (8 3) (2 2))
;;         ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3)))
;;         (("x" "x")
;;         ("x" "y")
;;         ("x" "Y")
;;         ("x" "z")
;;         ("x" "Z")
;;         ("X" "X")
;;         ("X" "y")
;;         ("X" "Y")
;;         ("X" "z")
;;         ("X" "Z")
;;         ("y" "x")
;;         ("y" "X")
;;         ("y" "y")
;;         ("y" "z")
;;         ("y" "Z")
;;         ("Y" "x")
;;         ("Y" "X")
;;         ("Y" "Y")
;;         ("Y" "z")
;;         ("Y" "Z")
;;         ("z" "x")
;;         ("z" "X")
;;         ("z" "y")
;;         ("z" "Y")
;;         ("z" "z")
;;         ("Z" "x")
;;         ("Z" "X")
;;         ("Z" "y")
;;         ("Z" "Y")
;;         ("Z" "Z")))    
;;      )
;;  "\n")
;;

;---------------------------QUESTION 3.1-----------------------
;Solves a rubiks cube using breadth first search. Can solve up to roughly 7 moves.
;Currently hardcoded to stop after 9 moves
(define (solveCube solved initial n)
    (let ([statesList (genStatesOptimised n initial (list))])
        (let ([solution (listSearcher (car statesList) (car (cdr statesList))  solved)])
            (if (null? solution)
                (if (= n 9)
                    (list)
                    (solveCube solved initial (+ n 1))
                )
                solution
            )
        )
    )
)
;; ; (print (equal? '("Z" "Y" "X") (solveCube solvedStates (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;; ; (print (equal? '("X") (solveCube solvedStates (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")


; Searches provided stateList looking for a solvedState
; Helper function for solveCube & solveCubeSafe
(define (listSearcher stateList moveList solved)
    (if (or (null? stateList) (null? moveList))
        (list)
        (if (boolean? (member (car stateList) solved))
            (listSearcher (cdr stateList) (cdr moveList) solved)
            (car moveList)
        )
    )
)
;; ;(print (equal? (listSearcher '(((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) '(("x")) solvedStates) '("x")) "\n")
;; ;(print (equal? (listSearcher '(((1 3) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) '(("x","y") ("x")) solvedStates) '("x")) "\n")

; Solves cube using the genStatesOptimisedTailRecursive flavour of genStates
; Currently hardcoded to stop after 9 moves 
 (define (solveCubeSafe solved initial n)
     (let ([statesList (genStatesOptimisedTailRecursive n initial (list))])
         (let ([solution (listSearcher (car statesList) (car (cdr statesList))  solved)])
             (if (null? solution)
                 (if (= n 9)
                     (list)
                     (solveCubeSafe solved initial (+ n 1))
                 )
                 solution
             )
         )
     )
 )
;; ;(print (equal? '("Z" "Y" "X") (solveCubeSafe solvedStates (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;; ;(print (equal? '("X") (solveCubeSafe solvedStates (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")

(define (solveCubeSlow solved initial n)
    (let ([statesList (genStatesTailRecursive n initial (list))])
        (let ([solution (listSearcher (car statesList) (car (cdr statesList))  solved)])
            (if (null? solution)
                (if (= n 9)
                    (list)
                    (solveCubeSlow solved initial (+ n 1))
                )
                solution
            )
        )
    )
)
;; ;(print (equal? '("Z" "Y" "X") (solveCubeSlow solvedStates (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;; ;(print (equal? '("X") (solveCubeSlow solvedStates (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;---------------------------------------------------------------------
