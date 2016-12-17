(define (author) (println "AUTHOR: Steven Eastcott sdeastcott@crimson.ua.edu"))

(define (exprTest # $expr target)
    (define result (catch (eval $expr #)))
    (if (error? result)
        (println $expr " is EXCEPTION: " (result'value)
            " (it should be " target ")")
        (println $expr " is " result " (it should be " target ")")
    )
)

;---------------------- STREAM FUNCTIONS ----------------------
(define scons cons-stream)
(define scar stream-car)
(define scdr stream-cdr)
(define (gcd a b) (if (= b 0) a (gcd b (% a b))))
(define (rprime? x y) (= (gcd x y) 1))
(define (integers-from-n n) (scons n (integers-from-n (+ 1 n))))
(define integers (integers-from-n 1))

(define (add-streams s t)
    (scons (+ (scar s) (scar t)) (add-streams (scdr s) (scdr t))))

(define (stream-ref s n)
    (if (= n 0)
        (scar s)
        (stream-ref (scdr s) (- n 1))))

(define (sdisplay str num)
    (print "[")
    (define (internal index)
        (if (> index num) 
            'printed
            (begin 
                (print (stream-ref str index) ", ") 
                (internal (+ 1 index)))))

    (internal 0)
    (println "...]"))

(define (smap proc @)
    (define argstreams @)
    (if (stream-null? argstreams) nil
        (scons (apply proc (map scar argstreams))
               (apply smap (cons proc (map scdr argstreams))))))

(define (stream-filter pred stream)
    (cond ((stream-null? stream) nil)
          ((pred (scar stream)) 
               (scons (scar stream) (stream-filter pred (scdr stream))))
          (else (stream-filter pred (scdr stream)))))

(define (scale-stream stream factor)  
    (smap (lambda (x) (* x factor)) stream))

;---------------------------- TASK 2 ----------------------------
(define (run2)
    ;(inspect (fib 20))
    ;(replace fib '+ +)
    ;(replace fib '- -)
    ;(inspect (fib 20))
    (println "(fib 20) is about 12% faster after calling replace on both + and -")
)
  
(define (replace f sym val)
    (define (define-case code)
        (cond ((eq? 'CONS (type (cddr code)))
              (set-cdr! (cdr code) (helper (cddr code)))
              code)))

    (define (let-case code)
        (map (lambda (pair) (inspect pair)
             (define expr (cdr pair))
             (cond ((eq? 'CONS (type expr))
                   (set-cdr! pair (helper expr)) pair)
             )) (cadr code)))

    (define (helper x)
        (cond
            ((eq? 'define (car x)) (handle-define x))
            ((eq? 'let (car x)) (handle-let x))
            (else (map 
                (lambda (x) 
                    (cond
                        ((eq? 'CONS (type x)) (helper x))
                        (else (if (eq? x sym) val x)))) x))))

    (define result (helper (get 'code f)))
    (set 'code result f))

(define (fib n)
    (if (< n 2) n
    (+ (fib (- n 1)) (fib (- n 2)))))


;---------------------------- TASK 4 -----------------------------
(define (run4)
    (define f (make-connector))
    (define m1 (make-connector))
    (define m2 (make-connector))
    (define r (make-connector))
    (set-value! m1 100 this)
    (set-value! m2 2 this)
    (set-value! r 1 this)
    (gravity f m1 m2 r)
    (exprTest (get-value f) 1.33460000))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define (set-value! connector new-value informant) ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (inform-about-value constraint) (constraint 'I-have-a-value))
(define (inform-about-no-value constraint) (constraint 'I-lost-my-value))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))

(define (divider d1 d2 q)
  (define (process-new-value)
    (cond ((and (has-value? d1) (= (get-value d1) 0))
           (set-value! q 0 me))
          ((and (has-value? d1) (has-value? d2))
           (set-value! q (/ (get-value d1) (get-value d2)) me))
          ((and (has-value? q) (has-value? d2))
           (set-value! d1 (* (get-value q) (get-value d2)) me))
          ((and (has-value? q) (has-value? d1))
           (set-value! d2 (/ (get-value d1) (get-value q)) me))))

  (define (process-forget-value)
    (forget-value! q me)
    (forget-value! d1 me)
    (forget-value! d2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (println "Unknown request -- DIVIDER" request))))

  (connect d1 me)
  (connect d2 me)
  (connect q me)
  me
)


(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2 (/ (get-value product) (get-value m1)) me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1 (/ (get-value product) (get-value m2)) me))))

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (println "Unknown request -- MULTIPLIER" request))))

  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me
)


(define (constant value connector)
  (define (me request)
    (println "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me
)

(define (make-connector)
    (define value #f)
    (define informant nil)
    (define constraints (list))

    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter inform-about-value constraints))

            ((not (= value newval))
             (println "Contradiction" (list value newval)))

            (else 'ignored)))

    (define (has-value? connector)
        (if (nil? informant) #f #t))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant nil)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints)) 'ignored))

    (define (connect new-constraint)
      (if (not (member? new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))

      (if (has-value? this)
          (inform-about-value new-constraint))

      'done)

    (define (me request)
      (cond ((eq? request 'has-value?)
            (if (nil? informant) #f #t))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (println "Unknown operation -- CONNECTOR" request))))
    me
)


(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (gravity f m1 m2 r)
    (define a (make-connector))
    (define b (make-connector))
    (define c (make-connector)) 
    (define d (make-connector))
    (constant 0.00667300 d)
    (multiplier m1 m2 a)
    (multiplier r r b)
    (divider a b c)
    (multiplier c d f)
)

;---------------------------- TASK 5 -----------------------------
(define (run5)
    (define b (barrier))
    ((b 'set) 3)

    (define (barrierTest) 
        (displayAtomic "Start... " (gettid) "\n") 
        (sleep 1)
        (displayAtomic "Stuck... " (gettid) "\n") 
        ((b 'install))
        (displayAtomic "Passed... " (gettid) "\n"))

    (define t1 (thread (barrierTest)))
    (define t2 (thread (barrierTest)))
    (define t3 (thread (barrierTest)))
    (tjoin t1)
    (tjoin t2)
    (tjoin t3))

(define (barrier)
    (define tidList nil)
    (define count nil)

    (define (set n) 
        (lock)
        (set! count n)
        (unlock))

    (define (install)
        (lock)
        (if (member? (gettid) tidList) nil
            (set! tidList (cons (gettid) tidList)))
        (if (= (length tidList) count) 
            (unlock)
            (begin (unlock) (install))))

    (define (remove) 
        (set! tidList nil)
        (set! count nil))

    this
)

;---------------------------- TASK 6 -----------------------------
(define (run6)
    (exprTest (stream-ref bgs 4) 121)
    (exprTest (stream-ref bgs 6) 539))

(define (f start)
    (scons start 
        (scons (* 7 start) 
            (scons (* 11 start) (f (* start start))))))

(define seven (scons 7 (scale-stream seven 7)))
(define eleven (scons 11 (scale-stream eleven 11)))
(define seven-eleven (f 77))

(define (big-gulp)
    (define (helper a b c)
        (if (< (scar a) (scar b))
            (if (< (scar a) (scar c)) 
                (scons (scar a) (helper (scdr a) b c))
                (scons (scar c) (helper a b (scdr c))))

            (if (< (scar b) (scar c))
                (scons (scar b) (helper a (scdr b) c))
                (scons (scar c) (helper a b (scdr c))))))

    (helper seven eleven seven-eleven))
    
(define bgs (big-gulp))


;---------------------------- TASK 7 -----------------------------
(define (run7)
    (print "poly: ") 
    (sdisplay poly 5)
    (print "intPoly: ") 
    (sdisplay intPoly 5)
    (print "divIntPoly: ") 
    (sdisplay divIntPoly 5)
    (print "difference: ") 
    (sdisplay difference 5)
)

(define (signal f x dx) (scons (f x) (signal f (+ x dx) dx)))

(define (integral s dx)
    (define d (scons dx d))
    (define result (smap + (scons 0 (smap * d s)) (scons 0 result))))

(define (differential start s dx)
    (define d (scons dx d))
    (smap / (smap - (scdr s) s) d))

(define poly (signal (lambda (x) (- (+ (^ x 2) (* 3 x)) 4)) 0 .01))
(define intPoly (integral poly .01))
(define divIntPoly (differential (scar poly) intPoly .01))
(define (subStreams s1 s2) (smap - s1 s2))
(define difference (subStreams poly divIntPoly))


;---------------------------- TASK 8 -----------------------------
(define (run8)
    (print "mystery: ")
    (sdisplay (mystery 1) 5)

    (print "ps-mystery: ")
    (sdisplay (ps-mystery 1) 5)

    (print "acc-mystery: ")
    (sdisplay (acc-mystery 1) 5)

    (print "super-mystery: ")
    (sdisplay (super-mystery 1) 5))

(define (mystery x)
    (define (numer start) (scons start (scale-stream (numer start) (* x x)))) 
    (define denom (scons 2.0 (smap * odds evens denom)))
    (define ones (scons -1.0 (smap (lambda(x) (* x -1.0)) ones)))
    (define odds (scons 3.0 (smap (lambda (x) (+ x 2.0)) odds)))
    (define evens (scons 4.0 (smap (lambda (x) (+ x 2.0)) evens)))
    (define new-numer (smap * ones (numer (* x x))))
    (scons 1.0 (smap / new-numer denom)))

(define (ps-mystery x)
    (define partial (smap + (mystery x) (scons 0 partial)))
    partial
)

(define (acc-mystery x)
    (define (euler-transform s)
        (let ((s0 (stream-ref s 0))           
              (s1 (stream-ref s 1))           
              (s2 (stream-ref s 2)))       
              (scons (- s2 (/ (* (- s2 s1) (- s2 s1)) (+ s0 (* -2 s1) s2)))
              (euler-transform (scdr s)))))

    (euler-transform (ps-mystery x)))

(define (super-mystery x)
    (define (make-tableau transform s)
        (scons s (make-tableau transform (transform s))))

    (define (accelerated-sequence transform s)
        (smap scar (make-tableau transform s)))

    (define (euler-transform s)
        (let ((s0 (stream-ref s 0))
              (s1 (stream-ref s 1))
              (s2 (stream-ref s 2)))
              (scons (- s2 (/ (* (- s2 s1) (- s2 s1)) (+ s0 (* -2 s1) s2)))
              (euler-transform (scdr s)))))

    (accelerated-sequence euler-transform (ps-mystery x)))


;---------------------------- TASK 9 -----------------------------
(define (run9)
   (exprTest (scar (scdr (ramanujan))) 4104)
)

(define (ramanujan)
    (define (merge-weighted s1 s2 weight)
        (cond
            ((stream-null? s1) s2)
            ((stream-null? s2) s1)
            (else
                (if (<= (weight (scar s1)) (weight (scar s2)))
                    (scons (scar s1) (merge-weighted (scdr s1) s2 weight))
                    (scons (scar s2) (merge-weighted s1 (scdr s2) weight))))))


    (define (weighted-pairs s1 s2 weight)
        (scons
            (list (scar s1) (scar s2))
            (merge-weighted (smap (lambda (x) (list (scar s1) x)) (scdr s2))
            (weighted-pairs (scdr s1) (scdr s2) weight) weight)))

    (define (cube-weight x) (+ (expt (car x) 3) (expt (cadr x) 3)))
    (define ones (cons-stream 1 ones))
    (define ints (scons 0 (add-streams ones ints)))
    (define p (weighted-pairs ints ints cube-weight))

    (define (search pairs)
        (if (= (cube-weight (scar pairs)) (cube-weight (scar (scdr pairs))))
            (scons (cube-weight (scar pairs)) (search (scdr pairs)))
            (search (scdr pairs))))

    (search p))

