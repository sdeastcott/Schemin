(include "assign3.scm")

(println "Assignment 3 Test Script: VERSION 7\n")

(println "author...\n")
(author)
(println)

(define (silent # $x)
    (println $x)
    (eval $x #)
    )

(print "PROBLEM 1:")
(if (defined? 'run1 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run1)
        (println "\n-------my tests---------------------\n")
        (println "(define (mysquare x) (* x x))")
        (define (mysquare x) (* x x))
        (inspect (nonlocals mysquare))
        (println "    [it should return (* begin)]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 2:")
(if (defined? 'run2 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run2)
        (println "\n-------my tests---------------------\n")
        (silent (define (mysquare x) (* x x)))
        (inspect (replace mysquare '* +))
        (inspect (mysquare 2))
        (println "    [it should be 4]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)
    
(print "PROBLEM 3:")
(if (defined? 'run3 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run3)
        (println "\n-------my tests---------------------\n")
        (silent (define t (avl)))
        (inspect ((t 'size)))
        (println "    [it should be 0]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 4:")
(if (defined? 'run4 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run4)
        (println "\n-------my tests---------------------\n")
        (silent (define f (make-connector)))
        (silent (define m1 (make-connector)))
        (silent (define m2 (make-connector)))
        (silent (define r (make-connector)))
        (silent (gravity f m1 m2 r))
        (inspect (set-value! m1 1 this))
        (inspect (forget-value! m1 this))
        (inspect (set-value! m1 1 this))
        (inspect (set-value! m2 1 this))
        (inspect (set-value! r 1 this))
        (inspect (get-value f))
        (println "    [it should be 0.0066730000]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 5:")
(if (defined? 'run5 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run5)
        (println "\n-------my tests---------------------\n")
        (silent (define b (barrier)))
        (silent ((b 'set) 1))
        (println "approaching the barrier")
        (silent ((b 'install)))
        (silent ((b 'remove)))
        (println "past the barrier")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 6:")
(if (defined? 'run6 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run6)
        (println "\n-------my tests---------------------\n")
        (inspect (stream-car (big-gulp)))
        (println "    [it should be 7]")
        (inspect (stream-car (stream-cdr (big-gulp))))
        (println "    [it should be 11]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 7:")
(if (defined? 'run7 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run7)
        (println "\n-------my tests---------------------\n")
        (inspect (define (f x) (* x x)))
        (inspect (stream-car (signal f 1 0.01)))
        (println "    [it should be 1]")
        (inspect (stream-car poly))
        (println "    [it should be -4]")
        (inspect (stream-car intPoly))
        (inspect (stream-car (integral poly 0.01)))
        (println "    [these should be the same]")
        (inspect (stream-car poly))
        (inspect (stream-car divIntPoly))
        (inspect (stream-car (differential (stream-car poly) intPoly 0.01)))
        (println "    [these should be the same]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 8:")
(if (defined? 'run8 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run8)
        (println "\n-------my tests---------------------\n")
        (inspect (stream-car (mystery 1.0)))
        (println "    [it should be 1.000000]")
        (inspect (stream-car (ps-mystery 1.0)))
        (println "    [it should be 1.000000]")
        (inspect (stream-car (acc-mystery 1.0)))
        (println "    [it should be 0.5384615385]")
        (inspect (stream-car (super-mystery 1.0)))
        (println "    [it should be 1.000000]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(print "PROBLEM 9:")
(if (defined? 'run9 this)
    (begin
        (println "\n\n------your tests--------------------\n")
        (run9)
        (println "\n-------my tests---------------------\n")
        (inspect (stream-car (ramanujan)))
        (println "    [it should be 1729]")
        )
    (println " NOT IMPLEMENTED")
    )
(println)

(author)
(println)
(println "this test script ran to completion\n");
