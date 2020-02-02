(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (if (null? rests) 
    nil
    (cons (append (list first) (car rests)) (cons-all first (cdr rests))))
  )
  ;(map (append first) rests))


(define (zip pairs)
  (cons (map car pairs) (list (map cadr pairs))
    ))


;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (helper a number)
    (if (null? a) 
      nil
      (cons (list number (car a)) (helper (cdr a) (+ number 1)))
      )

  )
     (helper s 0))
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS


(define (list-change total denom)
  ; BEGIN PROBLEM 18
  (cond 
    ((= total 0) (cons nil nil))
    ((< total 0) nil)
    ((null? denom) nil)
    (else (append (cons-all (car denom) (list-change (- total (car denom)) denom)) (list-change total (cdr denom))))
    )) 



  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr ;look up in environment if it is a symbol?
         ; END PROBLEM 19
         )
        ((quoted? expr) ;'(+ 1 2) 'string  ; no evaluation
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr) ;but how come there be lambda/define in a let? 
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append (list form params)(let-to-lambda body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr)) ; what is it refered to 
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (cons (cons (quote lambda) (cons (map let-to-lambda (car (zip values))) (let-to-lambda body))) (map let-to-lambda (cadr (zip values))))
           ; how to make a new lambda expression? 
           ; END PROBLEM 19
           ))
        (else ; recursion case???  
         ; BEGIN PROBLEM 19
          ;(cons((let-to-lambda expr.first) (let-to-lambda expr.second)))
           (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
