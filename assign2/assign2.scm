(define (author) (println "AUTHOR: Steven Eastcott sdeastcott@crimson.ua.edu"))

(define (exprTest # $expr target)
    (define result (catch (eval $expr #)))
    (if (error? result)
        (println $expr " is EXCEPTION: " (result'value)
            " (it should be " target ")")
        (println $expr " is " result " (it should be " target ")")
    )
)

(include "table.scm")

;----------------------------- TASK 1 ------------------------------
(define (run1)
    (println (list 1 2 3))
    (iterate i (list 1 2 3) (inspect (+ i i)) (inspect (* i i)))
)

(define (iterate # $x items $)
    (define (construct remaining result)
        (cond 
            ((= (length remaining) 1)
                (append (list 'lambda (list $x))
                        (append result (list (car remaining)))))

            ((pair? remaining)
                (construct (cdr remaining) 
                           (append result (list (car remaining)))))
        )
    )

    (define myLambda (eval (construct $ nil) #))

    (define (loop l) 
        (cond 
            ((> (length l) 0)
	       (myLambda (car l))
	       (loop (cdr l))
            )
        )
    )

    (loop items)
)


;----------------------------- TASK 2 ------------------------------
(define (run2)
    (define . 'MISSING)
    (define (f a b c d) (+ a b c d))
    (exprTest ((peval f . . . .) 1 2 3 4) 10)
    (exprTest ((peval f 1 2 3 4)) 10)
    (exprTest ((peval f 1 . . 4) 2 3) 10)
    (exprTest ((peval f 1 2 . 4) 3) 10)
    (exprTest ((peval f 1 . 3 4) 2) 10)
    (exprTest ((peval f . 2 3 .) 1 4) 10)
)

(define (peval @)
    (define args @)

    (define (funct @)
        (define (peval-helper list1 list2 result)
            (cond
                ((not (pair? list1))
                     (if (equal? list1 'MISSING)
                         (append result list2)
                         (append result list1))

                     (apply (car args) result))

                ((equal? (car list1) 'MISSING)
                     (if (not (pair? list2))
                         (peval-helper (cdr list1) list2 (append result list2))
                         (peval-helper (cdr list1) (cdr list2) (append result (cons (car list2) nil)))))

                (else
                     (peval-helper (cdr list1) list2 (append result (cons (car list1) nil))))
            )
        )
        (peval-helper (cdr args) @ nil)
    )
    funct
)




;----------------------------- TASK 3 ------------------------------
(define (run3)
    (exprTest (qpeek (enqueue (dequeue (enqueue (enqueue (enqueue (enqueue (Queue) 2) 1) 4) 5)) 8)) 1)
    (exprTest (qpeek (dequeue (enqueue (enqueue (dequeue (enqueue (Queue) 4)) 2) 1))) 1)
    (exprTest (qpeek (enqueue (enqueue (enqueue (enqueue (enqueue (Queue) 4) 3) 2) 1) 0)) 4)
    (exprTest (qsize (enqueue (enqueue (enqueue (enqueue (enqueue (Queue) 4) 3) 2) 1) 0)) 5)
    (exprTest (qsize (dequeue (enqueue (enqueue (dequeue (enqueue (Queue) 4)) 2) 1))) 1)
)

;---------------------- Stack Implementation ----------------------
(define (Stack) (cons 0 nil))
(define (push stack x) (cons (+ 1 (car stack)) (cons x (cdr stack))))
(define (pop stack) (cons (- (car stack) 1) (cddr stack)))
(define (speek stack) (cadr stack))
(define (ssize stack) (car stack))

;---------------------- Queue Implementation ----------------------
(define (Queue) (cons (Stack) (Stack)))

(define (enqueue queue x)
    (cons (push (car queue) x) (cdr queue))
)

(define (popS1pushS2 queue)
    (if (= 0 (caar queue)) 
        queue
        (popS1pushS2 (cons (pop (car queue)) (push (cdr queue) (cadr (car queue)))))
    )
)

(define (dequeue queue)
    (cond
        ((= (cadr queue) 0) 
            (define holder (popS1pushS2 queue))
            (cons (car holder) (pop (cdr holder))))
        
        (else (cons (car queue) (pop (cdr queue))))
    )
)

(define (qsize queue) (+ (ssize (car queue)) (ssize (cdr queue))))

(define (qpeek queue)
    (if (= (cadr queue) 0)
        (caddr (popS1pushS2 queue))
        (caddr queue)
    )
)


;----------------------------- TASK 4 ------------------------------
(define (run4)
    (exprTest (no-locals (quote (define (f) (define x 1) 10)))
        '(define (f) ((lambda (x) 10) 1))
    ) 

    (exprTest 
        (no-locals (quote (define (nsq a) (define x (+ a 1)) (define y (- a 1)) (* x y)))) 
        '(define (nsq a) ((lambda (x y) (* x y)) (+ a 1) (- a 1)))
    )
)

(define (no-locals l)
    (define (helper body params args)
        (cond 
            ((= (length body) 1)
                (append 
                  (list (car l) (cadr l))
                  (list (append (list (list 'lambda params (car body))) args))))

            ((and (pair? body) (eq? 'define (caar body)))
                 (helper 
                     (cdr body)
                     (append params (list (cadr (car body))))
                     (append args (list (caddr (car body))))))
        )
    )
    (helper (cddr l) nil nil)
)


;----------------------------- TASK 5 ------------------------------
(define (run5)
    (define (f a) (cons 1 a))
    (define zero (lambda (f) (lambda (x) x)))
    (define one (lambda (f) (lambda (x) (f x))))
    (define two (lambda (f) (lambda (x) (f (f x)))))
    (define three (lambda (f) (lambda (x) (f (f (f x))))))
    (define four (lambda (f) (lambda (x) (f (f (f (f x)))))))
    (exprTest (equal? (((pred four) f) nil) ((three f) nil)) #t)
    (exprTest (equal? (((pred three) f) nil) ((two f) nil)) #t)
    (exprTest (equal? (((pred one) f) nil) ((zero f) nil)) #t)
)

(define pred
    (lambda (n)
        (lambda (f)
            (lambda (x)
                (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u))
            )
        )
    )
)


;----------------------------- TASK 6 ------------------------------
(define (run6)
    (define tree1 (treeNode 1 (treeNode 2 (treeNode 4 nil nil) (treeNode 5 nil nil)) (treeNode 3 (treeNode 6 nil nil) nil)))
    (define tree2 (treeNode 5 (treeNode 3 (treeNode 1 nil nil) (treeNode 2 nil nil)) (treeNode 4 nil nil)))
    (exprTest (treedepth tree1) 2)
    (exprTest (treedepth tree2) 1.666666)
    (exprTest (treeflatten tree1) (list (list 2 4) (list 2 5) (list 2 6)))
    (exprTest (treeflatten tree2) (list (list 2 1) (list 2 2) (list 1 4)))
    
)

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (map proc (cdr items)))
    )
)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (treeflatten tree)
    (define (helper node depth result)
        (cond 
            ((and (equal? (cadr node) nil) (equal? (caddr node) nil)) 
                 (append result (list (list depth (car node)))))

            ((equal? (cadr node) nil) 
                 (append result (helper (caddr node) (+ 1 depth) result)))

            ((equal? (caddr node) nil) 
                 (append result (helper (cadr node) (+ 1 depth) result)))

            (else 
                (append result (append (helper (cadr node) (+ 1 depth) result) (helper (caddr node) (+ 1 depth) result))))
        )
    )
    (helper tree 0 nil)
)

(define (treeNode value left right) (list value left right))

(define (treedepth tree)
    (define depthList (map car (treeflatten tree)))
    (/ (accumulate + 0 depthList) (real (length depthList)))
)
    

; --- TASK 7 ------------
; --- NOT IMPLEMENTED ---


;----------------------------- TASK 8 ------------------------------
(define (run8)
    (exprTest ((cxr 'add) '(1 2 3 4 5 6)) 3)
    (exprTest ((cxr 'dddd) '(1 2 3 4 5 6)) 5)
    (exprTest ((cxr 'add) (list (list 2 3 4 5) 6 7 (list 8 9))) 7)
    (exprTest ((cxr 'aa) (list (list 2 3 4 5) 6 7 (list 8 9))) 2)
    (exprTest ((cxr 'aaddd) (list (list 2 3 4 5) 6 7 (list 8 9))) 8)
    (exprTest ((cxr 'daddd) (list (list 2 3 4 5) 6 7 (list 8 9))) (list 9))
)


(define (cxr symbol)
    (define (iter remaining x)
        (if (= (length remaining) 1)
            (cond
                ((equal? remaining "a") (car x))
                ((equal? remaining "d") (cdr x)))

            (cond
                ((equal? (car remaining) "a") (car (iter (cdr remaining) x)))
                ((equal? (car remaining) "d") (cdr (iter (cdr remaining) x))))
        )
    )
    (lambda (x) (iter (string symbol) x))
)


;----------------------------- TASK 9 ------------------------------
(define (run9)
    (install-generic)
    (exprTest (+ 1 2) 3)
    (exprTest (+ "x" "y") "xy")
    (exprTest (+ "123" 4) "1234")
    (exprTest (+ 123 "4") 127)
    (exprTest (- 123 "4") 119)
    (exprTest (- "abc" 1) "bc")
    (exprTest (* "abc" 3) "abcabcabc")
    (exprTest (* 3 "33") 99)
    (exprTest (/ 8 "2") 4)
    (uninstall-generic)
)

(define old+ +)
(define old- -)
(define old* *)
(define old/ /)

(define (apply-generic op arg1 arg2)
  (let ((type-tags (list (type arg1) (type arg2))))
    (let ((proc (getTable op type-tags)))
      (if (null? proc)
          (cond 
              ((eq? '+ op) (old+ arg1 arg2))
              ((eq? '- op) (old- arg1 arg2))
              ((eq? '* op) (old* arg1 arg2))
              ((eq? '/ op) (old/ arg1 arg2)))
          (proc arg1 arg2))))
)


(define (install-generic)
    (clearTable)

    (set! + (lambda (a b) (apply-generic '+ a b)))
    (set! - (lambda (a b) (apply-generic '- a b)))
    (set! * (lambda (a b) (apply-generic '* a b)))
    (set! / (lambda (a b) (apply-generic '/ a b)))

    (define (addStrings str1 str2) (string+ str1 str2))
    (define (addStringInteger str integer) (addStrings str (string integer)))
    (define (addIntegerString integer str) (old+ integer (int str)))
    (define (subIntegerString integer str) (old- integer (int str)))
    (define (mulIntegerString integer str) (old* integer (int str)))
    (define (divIntegerString integer str) (old/ integer (int str)))

    (define (subStringInteger str integer) 
        (if (= integer 0) 
            str
            (subStringInteger (cdr str) (old- integer 1))))

    (define (mulStringInteger str integer) 
        (define (mul-helper result count)
            (cond ((= count 1) str)
                  ((= integer 0) "")
                  (else (string+ result (mul-helper str (old- count 1))))))

        (mul-helper str integer))

    (putTable '+ '(STRING STRING) addStrings)
    (putTable '+ '(STRING INTEGER) addStringInteger)
    (putTable '+ '(INTEGER STRING) addIntegerString)
    (putTable '- '(STRING INTEGER) subStringInteger)
    (putTable '- '(INTEGER STRING) subIntegerString)
    (putTable '* '(STRING INTEGER) mulStringInteger)
    (putTable '* '(INTEGER STRING) mulIntegerString)
    (putTable '/ '(INTEGER STRING) divIntegerString)
    'generic-system-installed
)

(define (uninstall-generic)
    (set! + old+)
    (set! - old-)
    (set! * old*)
    (set! / old/)
    'generic-system-uninstalled
) 


;------------------------ TASK 10 ------------------------------
(define (run10) 
    (install-coercion)
    (exprTest (coerce 123 'REAL) 123.00000)
    (exprTest (coerce 456 'STRING) "456")
    (exprTest (coerce 789.1 'INTEGER) 789)
    (exprTest (coerce 123.1 'STRING) "123.1")
    (exprTest (coerce "123" 'INTEGER) 123)
    (exprTest (coerce "456" 'REAL) 456.00000)
    (exprTest (coerce (list 0 0 0) 'STRING) "(0 0 0)")
)

(define (coerce arg convertType)
    ((getTable 'coerce (list (type arg) convertType)) arg)
)

(define (install-coercion)
    (clearTable)
    (define (coerceIntegerReal i) (real i))
    (define (coerceIntegerString i) (string i))
    (define (coerceRealInteger r) (int r))
    (define (coerceRealString r) (string r))
    (define (coerceStringInteger s) (int s))
    (define (coerceStringReal s) (real s))
    (define (coerceListString l) (string l))
    (putTable 'coerce '(INTEGER REAL) real)
    (putTable 'coerce '(INTEGER STRING) string)
    (putTable 'coerce '(REAL INTEGER) int)
    (putTable 'coerce '(REAL STRING) string)
    (putTable 'coerce '(STRING INTEGER) int)
    (putTable 'coerce '(STRING REAL) real)
    (putTable 'coerce '(CONS STRING) string)
    'coercion-system-installed
)
