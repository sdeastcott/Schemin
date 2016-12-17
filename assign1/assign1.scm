(define (author) (println "AUTHOR: Steven Eastcott sdeastcott@crimson.ua.edu"))

(define (exprTest # $expr target)
    (define result (catch (eval $expr #)))
    (if (error? result)
        (println $expr " is EXCEPTION: " (result'value)
	    " (it should be " target ")")
        (println $expr " is " result
	    " (it should be " target ")")
    )
)

;---------------------------- TASK 1 --------------------------------
(define (run1)

(println 
"
'and' and 'my-and' may behave differently because 'and' is a 
special form and 'my-and' is a user-defined function. More 
specfically:

'and' will evaluate in normal order and has the capability of 
short circuiting, i.e. stop evaluating if the first predicate 
is #f. 

'my-and' will evaluate in applicative order, meaning it will 
evaluate both predicates, regardless of what the first predicate 
is.

(define f (f))
(inspect (and #f (f)))
(inspect (my-and #f (f)))

Looking at the example above, 'my-and' will get caught in an 
infinite loop from calling function 'f' rather than short 
circuiting.
"))


;---------------------------- TASK 2 --------------------------------
(define (run2)
    (exprTest (min5 6 5 3 8 9) 3)
    (exprTest (min5 0 0 0 0 0) 0)
    (exprTest (min5 1 2 3 4 5) 1)
    (exprTest (min5 5 9 8 2 -5) -5)
    (exprTest (min5 -2 -9 8 2 -5) -9)
)

(define (min5 a b c d e)
    (define (f x y) (if (< x y) x y))
    (f (f (f a b) (f c d)) e)
)


;---------------------------- TASK 3 --------------------------------
(define (run3)
    (exprTest (cym 0) "#FFFFFF")
    (exprTest (cym 1) "#FEF6FE")
    (exprTest (cym 17) "#F57DD8")
    (exprTest (cym 25) "#EB4AB0")
    (exprTest (cym 30) "#E33093")
    (exprTest (cym 75) "#614A09")
    (exprTest (cym 91) "#23B74B")
    (exprTest (cym 99) "#04F679")
    (exprTest (cym 100) "#00FF7F")
)

(define (cym x)
    (define x (real x))
    (define pi 3.14159265358979323846264338327950288419716939937510)

    (define (cyan)
        (fmt "%02X" (int (* 255 (sin (* pi .5 (+ (* x .01) 1))))))
    )

    (define (yellow)
        (fmt "%02X" (int (* 255 (- 1 (sin (* pi .01 x))))))
    )

    (define (magenta)
        (fmt "%02X" (int (* 127.5 (- 1 (sin (* pi .5 (- (* 3 x .01) 1)))))))
    )
    
    (string+ "#" (cyan) (yellow) (magenta))
)


;---------------------------- TASK 4 --------------------------------
(define (run4)
    (exprTest (root5 0) 0)
    (exprTest (root5 1) 1)
    (exprTest (root5 2) 1.148698355)
    (exprTest (root5 5) 1.3797296615)
    (exprTest (root5 32) 2)
    (exprTest (root5 -1234) -4.1520543709)
    (exprTest (root5 100) 2.5118864315)
    (exprTest (root5 123456789) 41.5243645783)
)

(define (root5 x)
    (define (average x y) (/ (+ x y) 5))

    (define (improve guess)
        (average (* 4 guess) (/ x (^ guess 4)))
    )

    (define (good-enough? guess)
        (< (abs (/ (- (^ guess 5) x) x)) 0.00000000001)
    )

    (define (root5-iter guess)
        (if (good-enough? guess)
            guess
            (root5-iter (improve guess))
        )
    )

    (if (= x 0) 
        0
        (root5-iter 1.0)
    )
)


;---------------------------- TASK 5 --------------------------------
(define (run5)
    (exprTest (bico 0 0) 1)
    (exprTest (bico 1 0) 1)
    (exprTest (bico 1 1) 1)
    (exprTest (bico 5 1) 5)
    (exprTest (bico 5 2) 10)
    (exprTest (bico 5 3) 10)
    (exprTest (bico 5 5) 1)
    (exprTest (bico 7 2) 21)
    (exprTest (bico 8 4) 70)
    (exprTest (bico 8 6) 28)
    (exprTest (bico 8 8) 1)
)

(define (bico x y)
    (if (or (= y 0) (= x y)) 
        1
        (+ (bico (- x 1) y) (bico (- x 1) (- y 1)))
    )
)


;---------------------------- TASK 6 --------------------------------
(define (run6)
    (define (f a b c d) (+ a b c d))
    (exprTest ((((curry f 1) 2) 3) 4) 10)
    (exprTest ((((curry f 0) 0) 0) 0) 0)
    (exprTest ((((curry f 1) -1) 1) -1) 0)
    (exprTest ((((curry f 1) 10) 11) 100) 122)
    (exprTest ((((curry f -1) -1) -1) -1) -4)
)

(define (curry f a)
    (lambda (b)
        (lambda (c)
            (lambda (d)
                (f a b c d)
            )
        )
    )
)


;---------------------------- TASK 7 --------------------------------
(define (run7)
    (define (f n) (+ (^ n 3) (^ n 2) n))
    (exprTest (zorp -5 f) -105)
    (exprTest (zorp -1 f) -1)
    (exprTest (zorp 0 f) 0)
    (exprTest (zorp 1 f) 3)
    (exprTest (zorp 4 f) 85)
    (exprTest (zorp 11 f) 22377)
    (exprTest (zorp 16 f) -25851)
    (exprTest (zorp 20 (lambda (x) (+ (^ x 3) (^ x 2) x))) -25898)
)

(define (zorp i f)
    (define (zorp-iter a b c i)
        (if (< i 3)
            c
            (zorp-iter b c (+ c (/ (^ (- c b) 2) (+ a (* (- 2) b) c))) (- i 1))
        )
    )

    (if (< i 3)
        (f i)
        (zorp-iter (f 0) (f 1) (f 2) i)
    )
)


;---------------------------- TASK 8 --------------------------------
(define (run8)
    (exprTest (egypt/ 0 10) 0)
    (exprTest (egypt/ 1 1) 1)
    (exprTest (egypt/ 1960 56) 35)
    (exprTest (egypt/ 123456789 1234) 100046)
    (exprTest (egypt/ 100 10) 10)
    (exprTest (egypt/ 100 11) 9)
    (exprTest (egypt/ 1 100) 0)
    (exprTest (egypt/ 99 100) 0)
)

(define (double x) (+ x x))

(define (halve x) 
    (define (iter x y count total) 
        (cond ((= y 0) total)
              ((> (double x) y) (iter 1 (- y x) 0 (+ total count)))
              ((= count 0) (iter (double x) y (+ 1 count) total))
              (else (iter (double x) y (double count) total))
        )
    )
    (iter 1 x 0 0)
)

(define (egypt/ dividend divisor)
    (define (iter1 a b c)
        (if (< b c) 
            (iter1 (double a) (double b) c)
            (iter2 a b c 0)
        )
    )

    (define (iter2 a b c d)
        (cond ((= a 0) d)
              ((<= b c) (iter2 (halve a) (halve b) (- c b) (+ d a)))
              (else (iter2 (halve a) (halve b) c d))
        )
    )

    (iter1 1 divisor dividend)
)


;---------------------------- TASK 9 --------------------------------
(define (run9)
    ;(define f1 (lambda (n) 1))
    ;(define f2 (lambda (n) (if (= 0 (% (- n 1) 3)) (+ 1 (* 4 (/ (- n 1) 3))) 1)))
    (println "The continued fraction converges to 1.6487212707 when carried out to infinity")
)

(define (mystery depth augend f1 f2)
    (define (iter store depth)
        (if (= depth 0)
            (+ augend store)
            (iter (/ (f1 depth) (+ (f2 depth) store)) (- depth 1))
        )
    )
    
    (iter 0.0 depth)
)


;---------------------------- TASK 10 --------------------------------
(define (run10) 
    (println "The ramanujan expression converges to 4 when carried out to infinity")
)

(define (ramanujan depth) 
    (define (recurse x depth)
        (if (= x depth)
            (* (+ x 1) (sqrt (+ x 6)))
            (* (+ x 1) (sqrt (+ (+ x 6) (recurse (+ x 1) depth))))
        )
    )

    (recurse 0 depth)
)    

(define (iramanujan depth) 
    (define (iter store count)
        (if (= count 0)
            (* (+ count 1) (sqrt (+ (+ count 6) store)))
            (iter (* (+ count 1) (sqrt (+ (+ count 6) store))) (- count 1))
        )
    )

    (iter 0 depth)
)


;-------------------------------------------------------------------------
(println "assignment 1 loaded!")
