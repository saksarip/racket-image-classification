;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname SakethSaripalliHomework10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)


(define-struct node [l r])
; A LeafyTree(LT) is one of:
; - "leaf"
; - (make-node LeafyTree LeafyTree)
; Interpretation: a binary tree that ends in a leaf

(define LT1 "leaf")
(define LT2 (make-node "leaf" LT1))
(define LT3 (make-node LT2 "leaf"))
(define LT4 (make-node "leaf" LT3))
(define LT5 (make-node "leaf" "leaf"))
(define LT6 (make-node LT4 LT5))

;; lt-temp : LT -> ?
#;(define (lt-temp lt)
    (cond[(string? lt) ...]
         [(node? lt) ...(lt-temp (node-l lt))
                     ...(lt-temp (node-r lt))]))


; An LR is one of:
; - "left"
; - "right"
; Interpretation: directions one can take in a LeafyTree
(define LR1 "left")
(define LR2 "right")

;; lr-temp : LR -> ?
#;(define (lr-temp lr)
    (cond[(string=? "left" lr) ...]
         [(string=? "right" lr) ...]))
 
; A Path is a [List-of LR]

(define PATH1 (list LR1 LR2 LR1 LR2))
(define PATH2 (list LR2 LR1 LR1 LR2 LR1))
(define PATH3 (list LR1 LR1 LR2 LR1 LR2 LR1))

;; path-temp : Path -> ?
#;(define (path-temp path)
    (cond[(empty? path)...]
         [(cons? path) ...(lr-temp (first path))
                       ...(path-temp (rest path))]))

#|
Design the function paths that given a LeafyTree outputs
the list of all Paths that represent the ways to go
from the root of the tree to its leaves.
Do not forget to first finish designing the supplied data definitions.
|#


;; paths : LT -> [List-of Paths]
;; outputs all the paths to get to every leaf in a leafy tree

(check-expect (paths LT1) (list empty))
(check-expect (paths LT2) (list
                           (list "left")
                           (list "right")))
(check-expect (paths LT3) (list
                           (list "left" "left")
                           (list "left" "right")
                           (list "right")))
(check-expect (paths LT4) (list
                           (list "left")
                           (list "right" "left" "left")
                           (list "right" "left" "right")
                           (list "right" "right")))
(check-expect (paths LT5) (list
                           (list "left")
                           (list "right")))
(check-expect (paths LT6) (list
                           (list "left" "left")
                           (list "left" "right" "left" "left")
                           (list "left" "right" "left" "right")
                           (list "left" "right" "right")
                           (list "right" "left")
                           (list "right" "right")))


(define (paths lt)
  (cond[(string? lt) (list empty)]
       [(node? lt) (append (map (λ(path) (cons "left" path)) (paths (node-l lt)))
                           (map (λ(path) (cons "right" path)) (paths (node-r lt))))]))


;; Exercise 2

; An Operation(OP) is one of:
; - "+"
; - "*"
; Interpretation: a commutative operation
(define PLUS "+")
(define TIMES "*")


;; op-temp : OP -> ?
#;(define (op-temp op)
    (cond[(string=? "+" op) ...]
         [(string=? "*" op) ...]))

 
; An AExp (Arithmetic Expression) is one of:
; - Number
; - (cons Operation [List-of AExp])

(define AEXP0 4)
(define AEXP1 7)
(define AEXP2 (cons PLUS (list AEXP0)))
(define AEXP3 (cons TIMES (list AEXP0 AEXP1)))
(define AEXP4 (cons PLUS (list AEXP0 (cons TIMES (list AEXP0 AEXP1)))))
(define AEXP5 (cons TIMES (list AEXP0 (cons TIMES (list AEXP0 AEXP2)))))


;; aexp-temp : AExp -> ?
#;(define (aexp-temp aexp)
    (cond[(number? aexp) ...]
         [(list? aexp) ...(op-temp (first aexp))
                       ...(list-aexp-temp (rest aexp))]))
;; list-aexp-temp : [List-of AExp] -> ?
#; (define (list-aexp-temp loaexp)
     (cond[(empty? laoexp) ...]
          [(cons? laoexp) (aexp-temp (first loaexp))
                          (list-aexp-temp (rest loaexp))]))




#|
Design the function eval that consumes an AExp and evaluates it to a single number.
Do not forget to first finish designing the supplied data definitions.|#

;; eval : AExp -> Number
;; Evaluates an Aexp to a single number


(check-expect (eval AEXP0) 4)
(check-expect (eval AEXP2) 4)
(check-expect (eval AEXP3) 28)
(check-expect (eval AEXP4) 32)
(check-expect (eval AEXP5) 64)



(define (eval aexp)
  (cond
    [(number? aexp) aexp]
    [(cons? aexp)
     (foldr (usable-expression aexp)
            (base-finder aexp)
            (map eval (rest aexp)))]))
      



;; usable-expression : AExp -> [Number Number -> Number]
;; returns the usable function from the first element of a non-empty aexp
(check-expect ((usable-expression AEXP2) 4 4) 8)
(check-expect ((usable-expression AEXP3) 4 4) 16)
(define (usable-expression aexp)
  (cond[(string=? "+" (first aexp)) +]
       [(string=? "*" (first aexp)) *]))

;; base-finder : AExp -> Number
;; returns the base case of a function from an operation
(check-expect (base-finder AEXP2) 0)
(check-expect (base-finder AEXP3) 1)
(define (base-finder aexp)
  (cond[(string=? "+" (first aexp)) 0]
       [(string=? "*" (first aexp)) 1]))






;; Exercise 3

;; A Feature is an integer in [0, 255]
;; and represents a grayscale pixel

(define FEATURE-0 0)
(define FEATURE-10 10)
(define FEATURE-255 255)

(define (feature-template f) ...)


(define FEATURE1 254)
(define FEATURE2 255)
(define FEATURE3 255)
(define FEATURE4 255)
(define FEATURE5 0)
(define FEATURE6 253)
(define FEATURE7 252)
(define FEATURE8 255)
(define FEATURE9 255)

(define LOF1-0 empty)
(define LOF1-1 (cons FEATURE1 LOF1-0))
(define LOF1-2 (cons FEATURE2 LOF1-1))
(define LOF1-3 (cons FEATURE3 LOF1-2))

(define LOF2-0 empty)
(define LOF2-1 (cons FEATURE4 LOF2-0))
(define LOF2-2 (cons FEATURE5 LOF2-1))
(define LOF2-3 (cons FEATURE6 LOF2-2))

(define LOF3-0 empty)
(define LOF3-1 (cons FEATURE7 LOF3-0))
(define LOF3-2 (cons FEATURE8 LOF3-1))
(define LOF3-3 (cons FEATURE9 LOF3-2))

(define LOLOF0 empty)
(define LOLOF1 (cons LOF1-3 LOLOF0))
(define LOLOF2 (cons LOF2-3 LOLOF1))
(define LOLOF3 (cons LOF3-3 LOLOF2))

(define BITMAP0 LOLOF0)
(define BITMAP1 LOLOF1)
(define BITMAP2 LOLOF2)
(define BITMAP3 LOLOF3)

(define INSTANCE0 empty)
(define INSTANCE1 (append LOF1-3 empty))
(define INSTANCE2 (append LOF2-3 LOF1-3 empty))
(define INSTANCE3 (append LOF3-3 LOF2-3 LOF1-3 empty))




;; A ListOfFeatures is one of:
;; - empty
;; - (cons Feature ListOfFeatures)
;; and represents an arbitrary number of grayscale pixels

(define LOF-EMPTY empty)
(define LOF-1-2 (cons 1 (cons 2 empty)))

;; lof-template : ListOfFeatures -> ???
(define (lof-template lof)
  (cond
    [(empty? lof) ...]
    [(cons? lof)
     (... (feature-temp (first lof)) ...
          (lof-template (rest lof)) ...)]))

;; A Bitmap is one of:
;; - empty
;; - (cons ListOfFeatures Bitmap)
;; and represents a grid of grayscale pixels

(define BITMAP-EMPTY empty)
(define BITMAP-ZERO
  (cons
   (cons 254 (cons 255 (cons 255 empty)))
   (cons
    (cons 255 (cons 0 (cons 253 empty)))
    (cons
     (cons 252 (cons 255 (cons 255 empty)))
     empty))))

;; bitmap-template : Bitmap -> ???
(define (bitmap-template b)
  (cond
    [(empty? b) ...]
    [(cons? b)
     (...  (lof-template (first b)) ...
           (bitmap-template (rest b)) ...)]))

;; Part c
;; An Instance is a ListOfFeatures
;; and represents a list of grayscale pixels

(define INSTANCE-EMPTY empty)
(define INSTANCE-ZERO
  (cons 254
        (cons 255
              (cons 255
                    (cons 255
                          (cons 0
                                (cons 253
                                      (cons 252
                                            (cons 255
                                                  (cons 255 empty))))))))))

;; instance-template : Instance -> ???
(define (instance-template inst)
  (cond [(empty? inst) ...]
        [(cons? inst)
         (... (first inst) ...
              (instance-template (rest inst)) ...)]))

;; Part d
(define-struct training [fname inst img digit])
;; A Training is a (make-training String Instance Image Integer)
;; - where fname is the file name or "" if the data did not come from a file
;; - inst is the flattened list of pixels
;; - img is the visualization of the bitmap of pixels
;; - and digit is the digit that this is an image of

(define PIXEL-SIZE 10)
(define IMAGE-ZERO
  (above
   (beside
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "black"))
   (beside
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "white")
    (square PIXEL-SIZE "solid" "black"))
   (beside
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "black")
    (square PIXEL-SIZE "solid" "black"))))
(define TRAINING-ZERO
  (make-training "" INSTANCE-ZERO IMAGE-ZERO 0))

;; training-template : Training -> ???
(define (training-template t)
  (... (training-fname t) ...
       (instance-template (training-inst t)) ...
       (training-img t) ...
       (training-digit t) ...))

(define-struct testing [fname inst img])
;; A Testing is a (make-testing String Instance Image)
;; - where fname is the name of the file the data came from or "" if it is not from a file
;; - inst is the flattened list of pixels
;; - and img is the visualization of the bitmap of pixels

(define TESTING-ZERO (make-testing "" INSTANCE-ZERO IMAGE-ZERO))

;; testing-template : Testing -> ???
(define (testing-template t)
  (... (testing-fname t) ...
       (instance-template (testing-inst t)) ...
       (testing-img t) ...))

;; Part e
(define-struct neighbor [training dist])
;; A Neighbor is a (make-neighbor Training NonNegNumber)
;; - where training is an instance of training data
;; - and dist is the "distance" from a certain testing data to that training data

(define NEIGHBOR-ZERO (make-neighbor TRAINING-ZERO 3))

;; neighbor-template : Neighbor -> ???
(define (neighbor-template n)
  (... (training-template (neighbor-training n)) ...
       (neighbor-dist n) ...))




;; return-former : Any Any -> Any
;; Returns the first argument
(check-expect (return-former 1 "hello") 1)
(check-expect (return-former true false) true)
(define (return-former a b) a)


;; next-index : (X) [List-of X] Nat -> Nat
;; Produces the next index in the list
(check-expect (next-index (list 1 2 3) 0) 1)
(check-expect (next-index (list "hello" "world") 1) 0)
(define (next-index lox index)
  ;; Okay to use if/cond instead of modulo
  (modulo (add1 index) (length lox)))

;; prev-index : (X) [List-of X] Nat -> Nat
;; Produces the previous index in the list
(check-expect (prev-index (list 1 2 3) 0) 2)
(check-expect (prev-index (list "hello" "world") 1) 0)
(define (prev-index lox index)
  ;; Okay to use if/cond instead of modulo
  (modulo (sub1 index) (length lox)))


;; flatten : (X) [List-of [List-of X]] -> [List-of X]
;; Flatten the nested list into a single-level list
(check-expect (flatten empty) empty)
(check-expect
 (flatten (list (list 1 2 3) (list "hello" "world")))
 (list 1 2 3 "hello" "world"))
(define (flatten lox)
  (foldr append empty lox))


;; training-fnames : Nat -> [List-of String]
;; Produce the names of all training files given the number of examples per digit
(check-expect (training-fnames 0) empty)
(check-expect
 (training-fnames 3)
 (list "train/d_1_0.txt" "train/d_2_0.txt" "train/d_3_0.txt"
       "train/d_1_1.txt" "train/d_2_1.txt" "train/d_3_1.txt"
       "train/d_1_2.txt" "train/d_2_2.txt" "train/d_3_2.txt"
       "train/d_1_3.txt" "train/d_2_3.txt" "train/d_3_3.txt"
       "train/d_1_4.txt" "train/d_2_4.txt" "train/d_3_4.txt"
       "train/d_1_5.txt" "train/d_2_5.txt" "train/d_3_5.txt"
       "train/d_1_6.txt" "train/d_2_6.txt" "train/d_3_6.txt"
       "train/d_1_7.txt" "train/d_2_7.txt" "train/d_3_7.txt"
       "train/d_1_8.txt" "train/d_2_8.txt" "train/d_3_8.txt"
       "train/d_1_9.txt" "train/d_2_9.txt" "train/d_3_9.txt"))
(define (training-fnames examples-per-digit)
  (foldr (λ (digit sofar)
           (append (map (λ (ex) (generate-training-file-name ex digit))
                        (build-list examples-per-digit add1))
                   sofar))
         empty
         (build-list 10 identity)))

;; generate-training-file-name : Nat Nat -> String
;; Generate a training file name for the given digit and example id
(check-expect (generate-training-file-name 1 0) "train/d_1_0.txt")
(check-expect (generate-training-file-name 7 3) "train/d_7_3.txt")
(define (generate-training-file-name exid digit)
  (string-append "train/d_" (number->string exid) "_" (number->string digit) ".txt"))


;; fname->label : String -> Nat
;; Find the number in the given file name (assume it exists)
(check-expect (fname->label "hello2.jpg") 2)
(check-expect (fname->label "train/d_7_5.txt") 5)
(define (fname->label fname)
  (string->number (substring fname (- (string-length fname) 5) (- (string-length fname) 4))))


;; map-lol : (X) [X -> Y] [List-of [List-of X]] -> [List-of [List-of Y]]
;; Map each element of each list using the given function
(check-expect (map-lol add1 empty) empty)
(check-expect
 (map-lol string->number
          (list (list "0" "0" "0")
                (list "1" "2" "3")
                (list "3" "2" "1")))
 (list (list 0 0 0)
       (list 1 2 3)
       (list 3 2 1)))
(define (map-lol transformer lolox)
  (map (λ (lox) (map transformer lox)) lolox))


;; f-left-to-right : (X) [X X -> X] [NEList-of X] -> X
;; Apply the given function from left to right
(check-expect (f-left-to-right * (list 7)) 7)
(check-expect (f-left-to-right - (list 1 2 3)) -4)
(define (f-left-to-right f lox)
  (foldl (lambda (elem sofar) (f sofar elem)) (first lox) (rest lox)))


;; read-lolon : String -> [List-of [List-of Number]]
;; Read the numbers from the given file
(check-expect (read-lolon "numbers.txt")
              (list
               (list 0 0 0 0)
               (list 1 2 3 4)
               (list 2 1 0 -0.5)))
(define (read-lolon fpath)
  (map (λ (los) (map string->number los)) (read-words/line fpath)))



(define LIST-MISMATCH "Lists must be the same size")

;; map-2list : (X Y Z) [X Y -> Z] [List-of X] [List-of Y] -> [List-of Z]
;; Apply the given function to each pair of elements
(check-expect (map-2list string=? empty empty) empty)
(check-error (map-2list * (list 10) (list 7 2)))
(check-error (map-2list - (list 7 2) (list 10)))
(check-expect (map-2list + (list 1 2) (list 3 4)) (list 4 6))
(define (map-2list transformer l1 l2)
  (cond [(and (empty? l1) (empty? l2)) empty]
        [(and (empty? l1) (cons? l2)) (error LIST-MISMATCH)]
        [(and (cons? l1) (empty? l2)) (error LIST-MISMATCH)]
        [(and (cons? l1) (cons? l2))
         (cons (transformer (first l1) (first l2))
               (map-2list transformer (rest l1) (rest l2)))]))

;; smallest-of-list-by-f : (X) [X -> Number] [NEList-of X] -> X
;; Find the element of the list that minimizes the given function
(check-expect (smallest-of-list-by-f string-length (list "hello")) "hello")
(check-expect
 (smallest-of-list-by-f
  length
  (list (list 1 2 3) (list 100) (list -1000 -99 -1 0) (list 2)))
 (list 100))
(define (smallest-of-list-by-f transformer nelox)
  (foldr (λ (element sofar)
           (if (<= (transformer element) (transformer sofar)) element sofar))
         (first nelox)
         (rest nelox)))



(define PIXEL-BLACK (square PIXEL-SIZE "solid" "black"))
(define PIXEL-WHITE (square PIXEL-SIZE "solid" "white"))


;; bitmap->image : Bitmap -> Image
;; Returns the visualization of the given bitmap
(check-expect (bitmap->image BITMAP-EMPTY) empty-image)
#;(check-expect
   (bitmap->image BITMAP-ZERO)
   (above (beside PIXEL-BLACK PIXEL-BLACK PIXEL-BLACK)
          (beside PIXEL-BLACK PIXEL-WHITE PIXEL-BLACK)
          (beside PIXEL-BLACK PIXEL-BLACK PIXEL-BLACK)))
(define (bitmap->image bm)
  (foldr (λ (row sofar) (above (bitmap-row->image row) sofar))
         empty-image
         bm))

;; bitmap-row->image : [List-of Feature] -> Image
;; Produce an image of each feature in a line
(check-expect (bitmap-row->image LOF-EMPTY) empty-image)
(check-expect (bitmap-row->image (list 0 255)) (beside PIXEL-WHITE PIXEL-BLACK))
(define (bitmap-row->image row)
  (foldr (λ (f sofar) (beside (feature->image f) sofar))
         empty-image
         row))

;; feature->image : Feature -> Image
;; Produce an image of a pixel with the given color
(check-expect (feature->image 0) PIXEL-WHITE)
(check-expect (feature->image 255) PIXEL-BLACK)
(define (feature->image f)
  (local [(define flipped (- 255 f))]
    ;; Okay to use a helper to find the correct color
    (square PIXEL-SIZE "solid" (make-color flipped flipped flipped))))


;; Exercise 3: Code Beginning 

(define WIDTH 800)
(define HEIGHT 1200)
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "white"))

;; mnist : Number String -> Number
;; Visualizes a prediction for a given number, given its image
;; and returns the prediction
(define (mnist trainnumber path)
  (local [(define LISTOFNEIGHBORS (neighbor-list trainnumber path))
          (define CLOSESTNEIGHBOR (find-closest LISTOFNEIGHBORS))
          (define TEST (test-maker path))]
    (return-former (training-digit (neighbor-training CLOSESTNEIGHBOR))
                   (big-bang 0
                     [to-draw (λ (n) (draw-digits n LISTOFNEIGHBORS CLOSESTNEIGHBOR TEST))]
                     [on-key (λ(n k) (change-neighbor n k LISTOFNEIGHBORS))]))))



;; draw-digits : Number [List-of Neighbors] Neighbor Test -> Image
;; Draws the the best prediction, the test image, and the training image of a neighbor
(check-expect (draw-digits 3 (neighbor-list 5 "test/d_10_0.txt")
                           (find-closest (neighbor-list 5 "test/d_10_0.txt"))
                           (test-maker "test/d_10_0.txt"))
              (beside (above/align "left" (draw-test (test-maker "test/d_10_0.txt"))
                                   (draw-closest
                                    (find-closest (neighbor-list 5 "test/d_10_0.txt"))))
                      (draw-train (list-ref (neighbor-list 5 "test/d_10_0.txt") 3))))

(check-expect (draw-digits 7 (neighbor-list 4 "test/d_6100_1.txt")
                           (find-closest (neighbor-list 4 "test/d_6100_1.txt"))
                           (test-maker "test/d_6100_1.txt"))
              (beside (above/align "left" (draw-test (test-maker "test/d_6100_1.txt"))
                                   (draw-closest
                                    (find-closest (neighbor-list 4 "test/d_6100_1.txt"))))
                      (draw-train (list-ref (neighbor-list 4 "test/d_6100_1.txt") 7))))


(define (draw-digits index lon closest test)
  (beside (above/align "left" (draw-test test)
                       (draw-closest closest))
          (draw-train (list-ref lon index))))
                



;; draw-test : Test -> Image
;; draws the testing image from a test file

(check-expect (draw-test (test-maker "test/d_10_0.txt"))
              (place-image (text "Test Image" 15 "red")
                           (/ WIDTH 4) 25
                           (place-image (testing-img (test-maker "test/d_10_0.txt"))
                                        (/ WIDTH 4) 125
                                        (rectangle (/ WIDTH 2) 400 "solid" "white"))))

(check-expect (draw-test (test-maker "test/d_6100_1.txt"))
              (place-image (text "Test Image" 15 "red")
                           (/ WIDTH 4) 25
                           (place-image (testing-img (test-maker "test/d_6100_1.txt"))
                                        (/ WIDTH 4) 125
                                        (rectangle (/ WIDTH 2) 400 "solid" "white"))))

(define (draw-test test)
  (place-image (text "Test Image" 15 "red")
               (/ WIDTH 4) 25
               (place-image (testing-img test)
                            (/ WIDTH 4) 125
                            (rectangle (/ WIDTH 2) 400 "solid" "white"))))



;; draw-closest : Neighbor -> Image
;; draws the closest training image from a training file
(check-expect (draw-closest (file->neighbor "train/d_1_9.txt" "test/d_10_0.txt"))
              (place-image (text (closest-string
                                  (file->neighbor "train/d_1_9.txt" "test/d_10_0.txt")) 15 "purple")
                           (/ WIDTH 4) 25
                           (place-image
                            (training-img
                             (neighbor-training
                              (file->neighbor "train/d_1_9.txt" "test/d_10_0.txt")))
                            (/ WIDTH 4) 125
                            (rectangle (/ WIDTH 2) 500 "solid" "white"))))

(check-expect (draw-closest (file->neighbor "train/d_2_0.txt" "test/d_6100_1.txt"))
              (place-image (text (closest-string
                                  (file->neighbor "train/d_2_0.txt" "test/d_6100_1.txt")) 15 "purple")
                           (/ WIDTH 4) 25
                           (place-image
                            (training-img
                             (neighbor-training
                              (file->neighbor "train/d_2_0.txt" "test/d_6100_1.txt")))
                            (/ WIDTH 4) 125
                            (rectangle (/ WIDTH 2) 500 "solid" "white"))))


(define (draw-closest closest)
  (place-image (text (closest-string closest) 15 "purple")
               (/ WIDTH 4) 25
               (place-image (training-img (neighbor-training closest))
                            (/ WIDTH 4) 125
                            (rectangle (/ WIDTH 2) 500 "solid" "white"))))



;; closest-string : Neighbor -> String
;; extractst the string that will be displayed for the closest neighbor

(check-expect (closest-string N1) "Best Match: 1\n3")
(check-expect (closest-string N2) "Best Match: 2\n4")

(define (closest-string closest)
  (string-append "Best Match: "
                 (number->string (training-digit (neighbor-training closest)))
                 "\n"
                 (number->string (neighbor-dist closest))))

;; draw-train : Neighbor -> Image
;; draws the current neighbor the user wants to dispplay
(check-expect (draw-train (file->neighbor "train/d_1_9.txt" "test/d_10_0.txt"))
              (place-image (text
                            (neighbor-string
                             (file->neighbor "train/d_1_9.txt" "test/d_10_0.txt")) 15 "blue")
                           (/ WIDTH 4) 250
                           (place-image
                            (training-img
                             (neighbor-training (file->neighbor "train/d_1_9.txt" "test/d_10_0.txt")))
                            (/ WIDTH 4) 400
                            (rectangle (/ WIDTH 2) HEIGHT "solid" "white"))))


(check-expect (draw-train (file->neighbor "train/d_2_0.txt" "test/d_6100_1.txt"))
              (place-image (text
                            (neighbor-string
                             (file->neighbor "train/d_2_0.txt" "test/d_6100_1.txt")) 15 "blue")
                           (/ WIDTH 4) 250
                           (place-image
                            (training-img
                             (neighbor-training
                              (file->neighbor "train/d_2_0.txt" "test/d_6100_1.txt")))
                            (/ WIDTH 4) 400
                            (rectangle (/ WIDTH 2) HEIGHT "solid" "white"))))

(define (draw-train neighbor)
  (place-image (text (neighbor-string neighbor) 15 "blue")
               (/ WIDTH 4) 250
               (place-image (training-img (neighbor-training neighbor))
                            (/ WIDTH 4) 400
                            (rectangle (/ WIDTH 2) HEIGHT "solid" "white"))))



;; neighbor-string : Neighbor -> String
;; draws the string that is supposed to be displayed for a given neighbor


(check-expect (neighbor-string N1) "Training\n1: 3")
(check-expect (neighbor-string N2) "Training\n2: 4")

(define (neighbor-string neighbor)
  (string-append "Training"
                 "\n"
                 (number->string (training-digit (neighbor-training neighbor)))
                 ": " (number->string (neighbor-dist neighbor))))



;; change-neighbor : (X) Number KeyEvent [List-of X]-> Number
;; changes our current index of the list of neighbors
(check-expect (change-neighbor 1 "left" (list 1 2 3)) 0)
(check-expect (change-neighbor 1 "right" (list 1 2 3)) 2)
(check-expect (change-neighbor 1 "n" (list 1 2 3)) 1)

(define (change-neighbor index key lon)
  (cond[(key=? "left" key) (prev-index lon index)]
       [(key=? "right" key) (next-index lon index)]
       [else index]))


;; test-maker : String -> Test
;; creates a test from a given filepath to a test

(check-expect (test-maker "train/d_1_9.txt")
              (make-testing "train/d_1_9.txt"
                            (instance-maker "train/d_1_9.txt")
                            (bitmap->image (file->bitmap "train/d_1_9.txt"))))

(check-expect (test-maker "train/d_2_0.txt")
              (make-testing "train/d_2_0.txt"
                            (instance-maker "train/d_2_0.txt")
                            (bitmap->image (file->bitmap "train/d_2_0.txt"))))


(define (test-maker f-test)
  (make-testing f-test
                (instance-maker f-test)
                (bitmap->image (file->bitmap f-test))))

;; neighbor-list : Number String -> [List-of Neighbors]
;; Creates the list of neighbors for a particular testing file

(check-within (neighbor-list 5 "test/d_10_0.txt")
              (map (λ(file) (file->neighbor file "test/d_10_0.txt")) (training-fnames 5)) 0.1)

(check-within (neighbor-list 10 "test/d_6100_1.txt")
              (map (λ(file) (file->neighbor file "test/d_6100_1.txt")) (training-fnames 10)) 0.1)

(define (neighbor-list trainnumber path)
  (map (λ(file) (file->neighbor file path)) (training-fnames trainnumber)))


;; file->neighbor String String -> Neighbor
;; Given a file path towards a training image, and a filepath towards a testing image,
(check-within (file->neighbor "train/d_1_9.txt" "test/d_10_0.txt")
              (make-neighbor (train-maker "train/d_1_9.txt")
                             (exact->inexact (distance-calc
                                              (instance-maker "train/d_1_9.txt")
                                              (instance-maker "test/d_10_0.txt")))) 0.1)

(check-within (file->neighbor "train/d_2_0.txt" "test/d_6100_1.txt")
              (make-neighbor (train-maker "train/d_2_0.txt")
                             (exact->inexact (distance-calc
                                              (instance-maker "train/d_2_0.txt")
                                              (instance-maker "test/d_6100_1.txt")))) 0.1)


(define (file->neighbor f-train f-test)
  (make-neighbor (train-maker f-train)
                 (exact->inexact (distance-calc
                                  (instance-maker f-train)
                                  (instance-maker f-test)))))

;; train-maker : String -> Train
;; turns a filepath into a training file
(check-expect (train-maker "train/d_1_9.txt")
              (make-training "train/d_1_9.txt"
                             (instance-maker "train/d_1_9.txt")
                             (bitmap->image (file->bitmap "train/d_1_9.txt"))
                             (fname->label "train/d_1_9.txt")))

(check-expect (train-maker "train/d_2_0.txt")
              (make-training "train/d_2_0.txt"
                             (instance-maker "train/d_2_0.txt")
                             (bitmap->image (file->bitmap "train/d_2_0.txt"))
                             (fname->label "train/d_2_0.txt")))



(define (train-maker f-train)
  (make-training f-train
                 (instance-maker f-train)
                 (bitmap->image (file->bitmap f-train))
                 (fname->label f-train)))


;; instance-maker : String -> Instance
;; creates an instance out of a filename
(check-expect (instance-maker "numbers.txt")
              (list 0 0 0 0 1 2 3 4 2 1 0 -0.5))

(check-expect (instance-maker "train/d_1_9.txt")
              (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 55 148 210 253 253 113 87 148 55 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 87 232 252 253 189 210 252 252 253 168 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 4 57 242 252 190 65 5 12 182 252 253 116 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 96 252 252 183 14 0 0 92 252 252 225 21 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 132 253 252 146 14 0 0 0 215 252 252 79 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 126 253 247 176 9 0 0 8 78 245 253 129 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 16 232 252 176 0 0 0 36 201 252 252 169 11 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 22 252 252 30 22 119 197 241 253 252 251 77 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 16 231 252 253 252 252 252 226 227 252 231 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 55 235 253 217 138 42 24 192 252 143 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 62 255 253 109 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 71 253 252 21 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 253 252 21 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 71 253 252 21 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 106 253 252 21 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 45 255 253 21 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 218 252 56 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 96 252 189 42 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 14 184 252 170 11 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 14 147 252 42 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(check-expect (instance-maker "test/d_10_0.txt")
              (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 28 195 254 254 254 254 254 255 61 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 6 191 253 253 253 253 253 253 253 60 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 26 190 253 253 253 253 240 191 242 253 60 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 15 187 253 253 253 253 253 200 0 211 253 60 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 22 66 253 253 253 253 241 209 44 23 218 253 60 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 124 253 253 253 253 253 182 0 0 131 253 253 60 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 38 217 253 253 244 111 37 0 0 131 253 253 60 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 124 253 253 253 165 0 0 0 22 182 253 253 60 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 124 253 253 240 45 0 0 0 53 253 253 249 58 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 16 168 253 216 45 0 0 0 0 53 253 253 138 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 159 253 253 147 0 0 0 0 0 53 253 253 138 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 136 252 253 227 5 0 0 0 0 0 53 253 243 101 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 140 253 253 124 0 0 0 0 0 0 156 253 218 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 13 164 253 142 5 0 0 0 0 0 32 233 253 218 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 62 253 253 130 0 0 0 0 0 37 203 253 253 127 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 62 253 253 147 36 36 36 36 151 222 253 245 127 8 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 34 202 253 253 253 253 253 253 253 253 253 200 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 140 253 253 253 253 253 253 253 248 235 65 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 87 173 253 253 253 253 253 253 182 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 14 78 96 253 253 253 137 56 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define (instance-maker fname)
  (flatten (file->bitmap fname)))


;; file->bitmap : String -> Bitmap
;; Retrieves a bitmap from a filename
(check-expect (file->bitmap "numbers.txt") (list
                                            (list 0 0 0 0)
                                            (list 1 2 3 4)
                                            (list 2 1 0 -0.5)))
(check-expect (file->bitmap "train/d_1_9.txt") (read-lolon "train/d_1_9.txt"))

(define (file->bitmap fname)
  (read-lolon fname))

;; distance-calc : Instance Instance -> Number
;; calculates the euclidean distance between two instances

(check-expect (distance-calc (list 1 2 3 4) (list 2 3 4 5)) 2)
(check-expect (distance-calc (list 1 2 3 4) (list 3 4 5 6)) 4)

(define (distance-calc train-values test-values)
  (sqrt (foldr + 0 (map-2list euclidean-distance train-values test-values))))


;; euclidean-distance : Number Number -> Number
;; Calculates the euclidean distance between two numbers
(check-expect (euclidean-distance 5 3) 4)
(check-expect (euclidean-distance 0 2) 4)
(check-expect (euclidean-distance 3 3) 0)
(define (euclidean-distance value1 value2)
  (sqr (- value1 value2)))

;; find-closest : [List-of Neighbors] -> Neighbor
;; extracts the one that has the smallest distance


(define T1 (make-training ".txt" (list 1) (square 1 "solid" "black") 1))
(define T2 (make-training ".txt" (list 2) (square 2 "solid" "black") 2))
(define T3 (make-training ".txt" (list 3) (square 3 "solid" "black") 3))

(define N1 (make-neighbor T1 3))
(define N2 (make-neighbor T2 4))
(define N3 (make-neighbor T3 7))

(define LON1 (list N1 N2))
(define LON2 (list N2 N3))

(check-expect (find-closest LON1) N1)
(check-expect (find-closest LON2) N2)

(define (find-closest lon)
  (smallest-of-list-by-f neighbor-dist lon))

(mnist 10 "test/d_55555_9.txt")













  














