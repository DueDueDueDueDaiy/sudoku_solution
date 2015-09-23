;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "lib.rkt")

;; A SudokuDigit is one of:
;; * '?
;; * 1 <= Nat <= 9

;; A Puzzle is a (listof (listof SudokuDigit))
;; requires: the list and all sublists have a length of 9

;; A Solution is a Puzzle
;; requires: none of the SudokuDigits are '?
;;           the puzzle satisfies the number placement 
;;             rules of sudoku

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are some sample sudoku puzzles

;; From the basic test shown in the assignment:
(define veryeasy
  '((? 4 5 8 9 3 7 1 6)
    (8 1 3 5 7 6 9 2 4)
    (7 6 9 2 1 4 5 3 8)
    (5 3 6 9 8 7 1 4 2)
    (4 9 2 1 6 5 8 7 3)
    (1 7 8 4 3 2 6 5 9)
    (6 8 4 7 2 1 3 9 5)
    (3 2 1 6 5 9 4 8 7)
    (9 5 7 3 4 8 2 6 1)))

;; the above puzzle with more blanks:
(define easy
  '((? 4 5 8 ? 3 7 1 ?)
    (8 1 ? ? ? ? ? 2 4)
    (7 ? 9 ? ? ? 5 ? 8)
    (? ? ? 9 ? 7 ? ? ?)
    (? ? ? ? 6 ? ? ? ?)
    (? ? ? 4 ? 2 ? ? ?)
    (6 ? 4 ? ? ? 3 ? 5)
    (3 2 ? ? ? ? ? 8 7)
    (? 5 7 3 ? 8 2 6 ?)))

;; the puzzle listed on wikipedia
(define wikipedia '((5 3 ? ? 7 ? ? ? ?)
                    (6 ? ? 1 9 5 ? ? ?)
                    (? 9 8 ? ? ? ? 6 ?)
                    (8 ? ? ? 6 ? ? ? 3)
                    (4 ? ? 8 ? 3 ? ? 1)
                    (7 ? ? ? 2 ? ? ? 6)
                    (? 6 ? ? ? ? 2 8 ?)
                    (? ? ? 4 1 9 ? ? 5)
                    (? ? ? ? 8 ? ? 7 9)))

;; A blank puzzle template for you to use:
(define blank '((? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thank THE CS 135 TEAM for your support during this semester!
;; Merry Christmas to all of you!
;; With love, Yue Dai.



;;purpose: function sudoku that consumes a Puzzle and 
;;         produces a Solution or false if a solution does not exist.
;;contract: Puzzle -> (Anyof Puzzle False)
;;examples:
(check-expect (sudoku blank) 
              '((1 2 3 4 5 6 7 8 9)
                (4 5 6 7 8 9 1 2 3)
                (7 8 9 1 2 3 4 5 6)
                (2 1 4 3 6 5 8 9 7)
                (3 6 5 8 9 7 2 1 4)
                (8 9 7 2 1 4 3 6 5)
                (5 3 1 6 4 2 9 7 8)
                (6 4 2 9 7 8 5 3 1)
                (9 7 8 5 3 1 6 4 2)))
(check-expect (sudoku '((? 2 3 4 5 6 ? 8 9)
                        (4 5 6 7 8 9 1 2 3)
                        (7 8 9 ? 2 3 4 5 6)
                        (2 1 4 3 6 5 8 9 7)
                        (3 6 5 8 9 7 2 ? 4)
                        (8 9 7 2 ? 4 3 6 5)
                        (5 ? 1 6 4 2 9 7 8)
                        (6 4 2 9 7 8 5 ? 1)
                        (? 7 8 5 3 1 6 4 2))) 
              '((1 2 3 4 5 6 7 8 9)
                (4 5 6 7 8 9 1 2 3)
                (7 8 9 1 2 3 4 5 6)
                (2 1 4 3 6 5 8 9 7)
                (3 6 5 8 9 7 2 1 4)
                (8 9 7 2 1 4 3 6 5)
                (5 3 1 6 4 2 9 7 8)
                (6 4 2 9 7 8 5 3 1)
                (9 7 8 5 3 1 6 4 2)))

(define (sudoku puzzle)
  (local [;;purpose: function ninth consumes a list of nine numbers
          ;;         and produces the ninth number of the list
          ;;contract: (Listof Nat) -> Nat
          (define (ninth lst)
            (cond [(empty? (rest lst)) (first lst)]
                  [else (ninth (rest lst))]))
          ;;purpose: function rowno. consumes a list of row
          ;;         and produces a list of row with row no. in front of each row
          ;;contract: (Listof (Listof Nat)) -> (Listof Nat (Listof Nat))
          (define (rowno. puzzle)
            (list (list 1 (first puzzle))
                  (list 2 (second puzzle))
                  (list 3 (third puzzle))
                  (list 4 (fourth puzzle))
                  (list 5 (fifth puzzle))
                  (list 6 (sixth puzzle))
                  (list 7 (seventh puzzle))
                  (list 8 (eighth puzzle))
                  (list 9 (ninth puzzle))))
          ;;purpose: function find-blank-row consumes a row and a column number
          ;;         and produces a list of pair of positions of '? in the row
          ;;contract: (Listof Nat) Nat -> (Listof Pair)
          (define (find-blank-row row col)
            (cond [(empty? (second row)) empty]
                  [(eq? (first (second row)) '?) 
                   (cons (list (first row) col) 
                         (find-blank-row (list (first row) (rest (second row))) 
                                         (add1 col)))]
                  [else (find-blank-row (list (first row) (rest (second row))) 
                                        (add1 col))]))
          ;;purpose: function find-blank consumes a puzzle
          ;;         and produces a list of pairs of 
          ;;         all the positions of '? in the puzzle
          ;;contract: Puzzle -> (Listof Pair)
          (define (find-blank puzzle)
            (foldr append empty (map (lambda (x) (find-blank-row x 1)) (rowno. puzzle))))
          ;;purpose: function current-row consumes a pair and a puzzle
          ;;         and produnces the row of the pair
          ;;contract: Pair Puzzle -> (Listof Nat)
          (define (current-row pair puzzle)
            (cond [(= 1 (first pair)) (first puzzle)]
                  [(= 2 (first pair)) (second puzzle)]
                  [(= 3 (first pair)) (third puzzle)]
                  [(= 4 (first pair)) (fourth puzzle)]
                  [(= 5 (first pair)) (fifth puzzle)]
                  [(= 6 (first pair)) (sixth puzzle)]
                  [(= 7 (first pair)) (seventh puzzle)]
                  [(= 8 (first pair)) (eighth puzzle)]
                  [(= 9 (first pair)) (ninth puzzle)]))
          ;;purpose: function current-col consumes a pair and a puzzle
          ;;         and produnces the column of the pair
          ;;contract: Pair Puzzle -> (Listof Nat)
          (define (current-col pair puzzle)
            (cond [(= 1 (second pair)) (map first puzzle)]
                  [(= 2 (second pair)) (map second puzzle)]
                  [(= 3 (second pair)) (map third puzzle)]
                  [(= 4 (second pair)) (map fourth puzzle)]
                  [(= 5 (second pair)) (map fifth puzzle)]
                  [(= 6 (second pair)) (map sixth puzzle)]
                  [(= 7 (second pair)) (map seventh puzzle)]
                  [(= 8 (second pair)) (map eighth puzzle)]
                  [(= 9 (second pair)) (map ninth puzzle)]))
          ;;purpose: function current-3*3 consumes a pair and a puzzle
          ;;         and produnces the 3*3 grids of the pair
          ;;contract: Pair Puzzle -> (Listof Nat)
          (define (current-3*3 pair puzzle)
            (cond [(and (>= (first pair) 1) (<= (first pair) 3) 
                        (>= (second pair) 1) (<= (second pair) 3)) ;row 1-row 3 col 1-col 3
                   (list (first (first puzzle)) (second (first puzzle)) (third (first puzzle))
                         (first (second puzzle)) (second (second puzzle)) (third (second puzzle))
                         (first (third puzzle)) (second (third puzzle)) (third (third puzzle)))]
                  [(and (>= (first pair) 4) (<= (first pair) 6) 
                        (>= (second pair) 1) (<= (second pair) 3)) ;row 4-row 6 col 1-col 3
                   (list (first (fourth puzzle)) (second (fourth puzzle)) (third (fourth puzzle))
                         (first (fifth puzzle)) (second (fifth puzzle)) (third (fifth puzzle))
                         (first (sixth puzzle)) (second (sixth puzzle)) (third (sixth puzzle)))]
                  [(and (>= (first pair) 7) (<= (first pair) 9) 
                        (>= (second pair) 1) (<= (second pair) 3)) ;row 7-row 9 col 1-col 3
                   (list (first (seventh puzzle)) (second (seventh puzzle)) (third (seventh puzzle))
                         (first (eighth puzzle)) (second (eighth puzzle)) (third (eighth puzzle))
                         (first (ninth puzzle)) (second (ninth puzzle)) (third (ninth puzzle)))]
                  [(and (>= (first pair) 1) (<= (first pair) 3) 
                        (>= (second pair) 4) (<= (second pair) 6)) ;row 1-row 3 col 4-col 6
                   (list (fourth (first puzzle)) (fifth (first puzzle)) (sixth (first puzzle))
                         (fourth (second puzzle)) (fifth (second puzzle)) (sixth (second puzzle))
                         (fourth (third puzzle)) (fifth (third puzzle)) (sixth (third puzzle)))]
                  [(and (>= (first pair) 4) (<= (first pair) 6) 
                        (>= (second pair) 4) (<= (second pair) 6)) ;row 4-row 6 col 4-col 6
                   (list (fourth (fourth puzzle)) (fifth (fourth puzzle)) (sixth (fourth puzzle))
                         (fourth (fifth puzzle)) (fifth (fifth puzzle)) (sixth (fifth puzzle))
                         (fourth (sixth puzzle)) (fifth (sixth puzzle)) (sixth (sixth puzzle)))]
                  [(and (>= (first pair) 7) (<= (first pair) 9) 
                        (>= (second pair) 4) (<= (second pair) 6)) ;row 7-row 9 col 4-col 6
                   (list (fourth (seventh puzzle)) (fifth (seventh puzzle)) (sixth (seventh puzzle))
                         (fourth (eighth puzzle)) (fifth (eighth puzzle)) (sixth (eighth puzzle))
                         (fourth (ninth puzzle)) (fifth (ninth puzzle)) (sixth (ninth puzzle)))]
                  [(and (>= (first pair) 1) (<= (first pair) 3) 
                        (>= (second pair) 7) (<= (second pair) 9)) ;row 1-row 3 col 7-col 9
                   (list (seventh (first puzzle)) (eighth (first puzzle)) (ninth (first puzzle))
                         (seventh (second puzzle)) (eighth (second puzzle)) (ninth (second puzzle))
                         (seventh (third puzzle)) (eighth (third puzzle)) (ninth (third puzzle)))]
                  [(and (>= (first pair) 4) (<= (first pair) 6) 
                        (>= (second pair) 7) (<= (second pair) 9)) ;row 4-row 6 col 7-col 9
                   (list (seventh (fourth puzzle)) (eighth (fourth puzzle)) (ninth (fourth puzzle))
                         (seventh (fifth puzzle)) (eighth (fifth puzzle)) (ninth (fifth puzzle))
                         (seventh (sixth puzzle)) (eighth (sixth puzzle)) (ninth (sixth puzzle)))]
                  [(and (>= (first pair) 7) (<= (first pair) 9) 
                        (>= (second pair) 7) (<= (second pair) 9)) ;row 7-row 9 col 7-col 9
                   (list (seventh (seventh puzzle)) (eighth (seventh puzzle)) (ninth (seventh puzzle))
                         (seventh (eighth puzzle)) (eighth (eighth puzzle)) (ninth (eighth puzzle))
                         (seventh (ninth puzzle)) (eighth (ninth puzzle)) (ninth (ninth puzzle)))]))
          ;;purpose: function possible consumes list1 and list2
          ;;         and produces a new list2 
          ;;         by removing all the elements in list1 from the original list2
          ;;contract: (Listof Nat) (Listof Nat) -> (Listof Nat)
          (define (possible lst1 lst2)
            (cond [(empty? lst1) lst2]
                  [else (possible (rest lst1) (remove (first lst1) lst2))]))
          ;;purpose: function possible-values consumes a Pair and a Puzzle
          ;;         and produces a new list of values possible 
          ;;         for the position of '? in the puzzle
          ;;contract: Pair Puzzle -> (Listof Nat)
          (define (possible-values pair puzzle)
            (possible (filter number? (append (current-row pair puzzle)
                                              (current-col pair puzzle)
                                              (current-3*3 pair puzzle)))
                      '(1 2 3 4 5 6 7 8 9)))
          ;;purpose: function change-digit-row consumes a list, a pair and a value
          ;;         and produces a new list by replacing the Nat on that position
          ;;         by the new value
          ;;contract: (Listof Nat) Pair Nat -> (Listof Nat)
          (define (change-digit-row row pair value)
            (cond [(= (second pair) 1) 
                   (list value (second row) (third row) (fourth row) (fifth row)
                         (sixth row) (seventh row) (eighth row) (ninth row))]
                  [(= (second pair) 2) 
                   (list (first row) value (third row) (fourth row) (fifth row)
                         (sixth row) (seventh row) (eighth row) (ninth row))]
                  [(= (second pair) 3) 
                   (list (first row) (second row) value (fourth row) (fifth row)
                         (sixth row) (seventh row) (eighth row) (ninth row))]
                  [(= (second pair) 4) 
                   (list (first row) (second row) (third row) value (fifth row)
                         (sixth row) (seventh row) (eighth row) (ninth row))]
                  [(= (second pair) 5) 
                   (list (first row) (second row) (third row) (fourth row) value
                         (sixth row) (seventh row) (eighth row) (ninth row))]
                  [(= (second pair) 6) 
                   (list (first row) (second row) (third row) (fourth row) (fifth row)
                         value (seventh row) (eighth row) (ninth row))]
                  [(= (second pair) 7) 
                   (list (first row) (second row) (third row) (fourth row) (fifth row)
                         (sixth row) value (eighth row) (ninth row))]
                  [(= (second pair) 8) 
                   (list (first row) (second row) (third row) (fourth row) (fifth row)
                         (sixth row) (seventh row) value (ninth row))]
                  [(= (second pair) 9) 
                   (list (first row) (second row) (third row) (fourth row) (fifth row)
                         (sixth row) (seventh row) (eighth row) value)]))
          ;;purpose: function change-digit consumes a puzzle, a pair and a value
          ;;         and produces a new puzzle by replacing the Nat on that position
          ;;         by the new value
          ;;contract: Puzzle Pair Nat -> Puzzle
          (define (replace-digit puzzle pair value)
            (cond [(= (first pair) 1) 
                   (list (change-digit-row (first puzzle) pair value)
                         (second puzzle) (third puzzle) 
                         (fourth puzzle) (fifth puzzle) (sixth puzzle)
                         (seventh puzzle) (eighth puzzle) (ninth puzzle))]
                  [(= (first pair) 2) 
                   (list (first puzzle)
                         (change-digit-row (second puzzle) pair value) 
                         (third puzzle) 
                         (fourth puzzle) (fifth puzzle) (sixth puzzle) 
                         (seventh puzzle) (eighth puzzle) (ninth puzzle))]
                  [(= (first pair) 3) 
                   (list (first puzzle) (second puzzle) 
                         (change-digit-row (third puzzle) pair value) 
                         (fourth puzzle) (fifth puzzle) (sixth puzzle) 
                         (seventh puzzle) (eighth puzzle) (ninth puzzle))]
                  [(= (first pair) 4) 
                   (list (first puzzle) (second puzzle) (third puzzle) 
                         (change-digit-row (fourth puzzle) pair value) 
                         (fifth puzzle) (sixth puzzle) 
                         (seventh puzzle) (eighth puzzle) (ninth puzzle))]
                  [(= (first pair) 5) 
                   (list (first puzzle) (second puzzle) (third puzzle) 
                         (fourth puzzle) 
                         (change-digit-row (fifth puzzle) pair value) 
                         (sixth puzzle) 
                         (seventh puzzle) (eighth puzzle) (ninth puzzle))]
                  [(= (first pair) 6) 
                   (list (first puzzle) (second puzzle) (third puzzle) 
                         (fourth puzzle) (fifth puzzle) 
                         (change-digit-row (sixth puzzle) pair value) 
                         (seventh puzzle) (eighth puzzle) (ninth puzzle))]
                  [(= (first pair) 7) 
                   (list (first puzzle) (second puzzle) (third puzzle) 
                         (fourth puzzle) (fifth puzzle) (sixth puzzle) 
                         (change-digit-row (seventh puzzle) pair value) 
                         (eighth puzzle) (ninth puzzle))]
                  [(= (first pair) 8) 
                   (list (first puzzle) (second puzzle) (third puzzle) 
                         (fourth puzzle) (fifth puzzle) (sixth puzzle) 
                         (seventh puzzle) 
                         (change-digit-row (eighth puzzle) pair value) 
                         (ninth puzzle))]
                  [(= (first pair) 9) 
                   (list (first puzzle) (second puzzle) (third puzzle) 
                         (fourth puzzle) (fifth puzzle) (sixth puzzle) 
                         (seventh puzzle) (eighth puzzle) 
                         (change-digit-row (ninth puzzle) pair value))]))
          ;;purpose: function my-solved? consumes a Puzzle
          ;;         and produces true if the Puzzle is a complete solution
          ;;         or false if it is not
          ;;contract: Puzzle -> Bool
          (define (my-solved? soln) 
            (cond [(= (length (filter number? (foldr append empty soln))) 81) true]
                  [else false]))
          ;;purpose: function neighbours consumes a puzzle 
          ;;         and produces a list of new puzzle generating by
          ;;         trying all possible values in the relating '?
          ;;contract: Puzzle -> Puzzle
          (define (neighbours puzzle)
            (map (lambda (x) (replace-digit puzzle (first (find-blank puzzle)) x)) 
                 (possible-values (first (find-blank puzzle)) puzzle)))]
    (find-final puzzle neighbours my-solved?)))

;;tests:
(check-expect (sudoku easy) 
              '((2 4 5 8 9 3 7 1 6)
                (8 1 3 5 7 6 9 2 4)
                (7 6 9 2 1 4 5 3 8)
                (5 3 6 9 8 7 1 4 2)
                (4 9 2 1 6 5 8 7 3)
                (1 7 8 4 3 2 6 5 9)
                (6 8 4 7 2 1 3 9 5)
                (3 2 1 6 5 9 4 8 7)
                (9 5 7 3 4 8 2 6 1)))
(check-expect (sudoku '((? 4 5 8 9 3 7 1 6)
                        (8 1 3 5 7 6 9 2 4)
                        (7 6 9 2 1 4 5 3 8)
                        (5 3 6 9 8 7 1 4 2)
                        (4 9 2 1 6 5 8 7 3)
                        (1 7 8 4 3 2 6 5 9)
                        (6 8 4 7 2 1 3 9 5)
                        (3 2 1 6 5 9 4 8 7)
                        (9 5 7 3 4 8 2 6 1)))
              '((2 4 5 8 9 3 7 1 6)
                (8 1 3 5 7 6 9 2 4)
                (7 6 9 2 1 4 5 3 8)
                (5 3 6 9 8 7 1 4 2)
                (4 9 2 1 6 5 8 7 3)
                (1 7 8 4 3 2 6 5 9)
                (6 8 4 7 2 1 3 9 5)
                (3 2 1 6 5 9 4 8 7)
                (9 5 7 3 4 8 2 6 1)))
(check-expect (sudoku '((5 3 ? ? 7 ? ? ? ?)
                        (6 ? ? 1 9 5 ? ? ?)
                        (? 9 8 ? ? ? ? 6 ?)
                        (8 ? ? ? 6 ? ? ? 3)
                        (4 ? ? 8 ? 3 ? ? 1)
                        (7 ? ? ? 2 ? ? ? 6)
                        (? 6 ? ? ? ? 2 8 ?)
                        (? ? ? 4 1 9 ? ? 5)
                        (? ? ? ? 8 ? ? 7 9)))
              '((5 3 4 6 7 8 9 1 2)
                (6 7 2 1 9 5 3 4 8)
                (1 9 8 3 4 2 5 6 7)
                (8 5 9 7 6 1 4 2 3)
                (4 2 6 8 5 3 7 9 1)
                (7 1 3 9 2 4 8 5 6)
                (9 6 1 5 3 7 2 8 4)
                (2 8 7 4 1 9 6 3 5)
                (3 4 5 2 8 6 1 7 9)))