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
;; ;TESTS
;; ; (print (= 2 (recalculateOrientation 1 0)) "\n")
;; ; (print (= 5 (recalculateOrientation 5 0)) "\n")
;; ; (print (= 1 (recalculateOrientation 1 1)) "\n")
;; ; (print (= 6 (recalculateOrientation 2 1)) "\n")
;; ; (print (= 5 (recalculateOrientation 1 2)) "\n")
;; ; (print (= 2 (recalculateOrientation 2 2)) "\n")

;rotations are performed using the left hand rule
;rotates left 4 cubes along x axis
(define (rotateX ispositive state)
	(if ispositive
        (list (rotateXPos 0 state (list)) (list "x"))
        (list (rotateXNeg 0 state (list)) (list "X"))
    )
)

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


(define(createPair miniList axis)
	(list (car miniList) (recalculateOrientation (car (cdr miniList)) axis))
)

;rotates bottom 4 cubes along y axis
(define (rotateY ispositive state)
    (if ispositive
        (list (rotateYPos 0 state (list)) (list "y"))
        (list (rotateYNeg 0 state (list)) (list "Y"))
    )
)

;rotates back 4 cubes along z axis
(define (rotateZ ispositive state)
    (if ispositive
        (list (rotateZPos 0 state (list)) (list "z"))
        (list (rotateZNeg 0 state (list)) (list "Z"))
    )
)


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
;; ;TESTS
;; ; (print (equal? (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))) "\n")
;; ; (print (equal? (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateY #t (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "xXx" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #t (car (rotateX #f (car (rotateX #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (equal? (rotate "yXz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateZ #t (car (rotateX #f (car (rotateY #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))))))))) "\n")
;; ; (print (not (equal? (rotate "xXy" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (car (rotateX #f (car (rotateZ #t '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)))))))) "\n")
;; ;------------------------------------------------------------

;-----------------------QUESTION 1.2-----------------------
;generates the successor states of the current given rubiks cube state
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

;; ;TESTS
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
(define (generateOptimisedSuccessorStates state prevMoves)
    (if (= (length prevMoves) 0)
        (generateSuccessorStates state prevMoves)  
        (let ([lastMove (index prevMoves (- (length prevMoves) 1))])
            (buildUpSuccessor (list) (list) state prevMoves #\x lastMove) 
        )
    )
)

; Builds up a state list and move list recursively
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
;; ;        )
;; ; "\n")

;-----------------------------QUESTION 2.1--------------------------

;finds all the states at a specific depth
(define (genStates n state moves)
    (if (= n 0)
        (list (list state) (list moves))
        (buildListController (list state) (list moves) 0 n (list) (list))
    )
)

; Creates a list of states and moves up to 'n' depth
(define (buildListController stateList moveList currentIndex totalIndex buildUpList buildUpMove)
    (if (= currentIndex totalIndex)
         (list stateList moveList)
         (cond
            [(not (null? stateList))
                (buildUp stateList moveList currentIndex totalIndex buildUpList buildUpMove)
            ]
            [else (buildListController buildUpList buildUpMove (+ currentIndex 1) totalIndex (list) (list))]
         )
    )
)

; Populates a buildUpList and buildUpMove list at a single depth
(define (buildUp stateList moveList currentIndex totalIndex buildUpList buildUpMove)
    (let ([successorStates (generateSuccessorStates (car stateList) (car moveList))])
        (buildListController (cdr stateList) (cdr moveList) currentIndex totalIndex (append buildUpList (car successorStates)) (append buildUpMove (car (cdr successorStates))))
    )
)

;;  Behaves correctly for depth 0
;;  (print (equal? (genStates 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;;      '((((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (()))
;;      )
;;  "\n")
;;
;;  Behaves correctly for depth 2
;;  (print (equal? (genStates 2 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;;      '((((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
;;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;;         ((5 4) (2 1) (1 2) (4 1) (6 3) (8 3) (7 5) (3 6))
;;         ((5 4) (2 1) (1 2) (4 1) (3 5) (7 6) (8 3) (6 3))
;;         ((2 5) (6 6) (1 2) (4 1) (5 4) (7 4) (3 2) (8 3))
;;         ((7 4) (5 4) (1 2) (4 1) (6 5) (2 6) (3 2) (8 3))
;;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;;         ((7 1) (2 1) (5 1) (4 1) (3 3) (6 3) (1 3) (8 3))
;;         ((3 4) (2 1) (7 2) (4 1) (6 3) (8 3) (1 5) (5 6))
;;         ((3 4) (2 1) (7 2) (4 1) (5 5) (1 6) (8 3) (6 3))
;;         ((2 5) (6 6) (7 2) (4 1) (3 4) (1 4) (5 2) (8 3))
;;         ((1 4) (3 4) (7 2) (4 1) (6 5) (2 6) (5 2) (8 3))
;;         ((6 4) (2 1) (1 2) (4 1) (5 4) (8 3) (3 2) (7 3))
;;         ((3 4) (2 1) (5 2) (4 1) (1 4) (8 3) (6 2) (7 3))
;;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
;;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;;         ((2 5) (8 6) (3 1) (4 1) (1 5) (6 6) (5 3) (7 3))
;;         ((6 5) (1 6) (3 1) (4 1) (8 5) (2 6) (5 3) (7 3))
;;         ((7 4) (2 1) (1 2) (4 1) (8 4) (5 3) (3 2) (6 3))
;;         ((3 4) (2 1) (8 2) (4 1) (1 4) (5 3) (7 2) (6 3))
;;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;;         ((1 1) (2 1) (3 1) (4 1) (8 3) (7 3) (6 3) (5 3))
;;         ((2 5) (5 6) (3 1) (4 1) (1 5) (7 6) (8 3) (6 3))
;;         ((7 5) (1 6) (3 1) (4 1) (5 5) (2 6) (8 3) (6 3))
;;         ((1 5) (6 6) (2 5) (4 1) (7 4) (5 6) (3 2) (8 3))
;;         ((3 4) (6 6) (7 2) (4 1) (2 5) (5 6) (1 5) (8 3))
;;         ((2 5) (6 6) (3 1) (4 1) (5 4) (8 3) (1 2) (7 3))
;;         ((2 5) (6 6) (3 1) (4 1) (7 3) (1 4) (8 3) (5 2))
;;         ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3))
;;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;;         ((6 5) (1 6) (5 5) (4 1) (7 4) (2 6) (3 2) (8 3))
;;         ((3 4) (1 6) (7 2) (4 1) (5 5) (2 6) (6 5) (8 3))
;;         ((5 5) (1 6) (3 1) (4 1) (2 4) (8 3) (6 2) (7 3))
;;         ((5 5) (1 6) (3 1) (4 1) (7 3) (6 4) (8 3) (2 2))
;;         ((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))
;;         ((6 1) (5 1) (3 1) (4 1) (2 3) (1 3) (7 3) (8 3)))
;;         (("x" "x")
;;          ("x" "X")
;;          ("x" "y")
;;          ("x" "Y")
;;          ("x" "z")
;;          ("x" "Z")
;;          ("X" "x")
;;          ("X" "X")
;;          ("X" "y")
;;          ("X" "Y")
;;          ("X" "z")
;;          ("X" "Z")
;;          ("y" "x")
;;          ("y" "X")
;;          ("y" "y")
;;          ("y" "Y")
;;          ("y" "z")
;;          ("y" "Z")
;;          ("Y" "x")
;;          ("Y" "X")
;;          ("Y" "y")
;;          ("Y" "Y")
;;          ("Y" "z")
;;          ("Y" "Z")
;;          ("z" "x")
;;          ("z" "X")
;;          ("z" "y")
;;          ("z" "Y")
;;          ("z" "z")
;;          ("z" "Z")
;;          ("Z" "x")
;;          ("Z" "X")
;;          ("Z" "y")
;;          ("Z" "Y")
;;          ("Z" "z")
;;          ("Z" "Z")))    
;;      )
;;  "\n")
;;  


;finds all the states at a specific depth using generateOptimisedSuccessorStates
(define (genStatesOptimised n state moves)
     (if (= n 0)
        (list (list state) (list moves))
        (OptimisedBuildListController (list state) (list moves) 0 n (list) (list))
     )
)

 ; Creates a list of states and moves up to 'n' depth
(define (OptimisedBuildListController stateList moveList currentIndex totalIndex buildUpList buildUpMove)
    (if (= currentIndex totalIndex)
         (list stateList moveList)
         (cond
            [(not (null? stateList))
                (buildUpOptimised stateList moveList currentIndex totalIndex buildUpList buildUpMove)
            ]
            [else (OptimisedBuildListController buildUpList buildUpMove (+ currentIndex 1) totalIndex (list) (list))]
         )
    )
)

; Populates a buildUpList and buildUpMove list at a single depth
(define (buildUpOptimised stateList moveList currentIndex totalIndex buildUpList buildUpMove)
    (define successorStates (generateOptimisedSuccessorStates (car stateList) (car moveList)))
        (OptimisedBuildListController (cdr stateList) (cdr moveList) currentIndex totalIndex (append buildUpList (car successorStates)) (append buildUpMove (car (cdr successorStates))))
)

;;  Behaves correctly for depth 0
;;  (print (equal? (genStatesOptimised 0 '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3)) '())
;;      '((((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) (()))
;;      )
;;  "\n")
;;
;;  Behaves correctly for depth 2
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


;---------------------------QUESTION 3.1-----------------------
;Solves a rubiks cube using breadth first search. Can solve up to roughly 7 moves.
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

; Searches provided stateList looking for a solvedState
(define (listSearcher stateList moveList solved)
    (if (or (null? stateList) (null? moveList))
        (list)
        (if (boolean? (member (car stateList) solved))
            (listSearcher (cdr stateList) (cdr moveList) solved)
            (car moveList)
        )
    )
)
;---------------------------------------------------------------------
;TESTS
; (print (equal? '("Z" "Y" "X") (solveCube solvedStates (rotate "xyz" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
; (print (equal? '("X") (solveCube solvedStates (rotate "x" '((1 1) (2 1) (3 1) (4 1) (5 3) (6 3) (7 3) (8 3))) 0)) "\n")
;---------------------------------------------------------------------
