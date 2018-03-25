#lang racket

(provide 
         entry entry-key entry-value entry?
         bnot bnot-arg bnot?
         band band-arg1 band-arg2 band?
         bor bor-arg1 bor-arg2 bor?
         tt tt-vars tt-rows tt?
	 hours
         lookup unique-keys?
         boolean-exp? type-of
	 all-vars
	 eval-in-env
	 all-combs
	 truth-table	 	 	 
	 satisfiable? equivalent?
	 find-exp
         substitute-in
         match)

; Please do not modify lines above this one.

; ****************************************************************
; CS 201b HW #4  DUE 11:59 pm Sunday, October 22, 2017
; using the submit command on the Zoo.
; ****************************************************************
; Name: Olivia Roth
; Email address: olivia.roth@yale.edu
; ****************************************************************
; ** problem 0 ** (1 easy point)
; Please modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 12)

; ****************************************************************
; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Racket constructs, except mutators (set! and its relatives.)
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; The topics of this assignment are as follows.
; Racket: deep recursion on a recursively defined data structure.
; Computer Science: Boolean functions, expressions, environments,
; truth tables, satisfiability, equivalence.

; You might also find uses for the special form: case.

; ****************************************************************
; We define a table as a list of entries,
; where each entry is given by the following structure.

(struct entry (key value) #:transparent)

; Recall that a struct defines a constructor, selector(s), and a type predicate.
; In this case, the constructor is entry, the selectors are
; entry-key and entry-value, and the type predicate is entry?.

; Here are two examples of tables.

(define test1
  (list
   (entry "second" 2)
   (entry "first" 1)
   (entry "fifth" 5)))

(define test2
  (list
   (entry 'x 0)
   (entry 'z 1)
   (entry 'y 1)
   (entry 'z 0)))

(define test3
  (list))

(define test4
  (list
   (entry 'x 0)))

; ****************************************************************
; ** problem 1 ** (9 points)
; Write two procedures to deal with tables as follows.

; (lookup key table)
; (unique-keys? table)

; (lookup key table)
; returns #f if no entry in the table has a key equal? to key
; otherwise, returns the value of the first entry whose key is equal? to key.

; (unique-keys? table)
; returns #t if all the entries of table have keys that are pairwise
; not equal? and #f otherwise.

; Examples
;> (lookup "first" test1)
;1
;> (lookup "third" test1)
;#f
;> (lookup 'z test2)
;1
;> (unique-keys? test1)
;#t
;> (unique-keys? test2)
;#f
;>
; ****************************************************************

(define (lookup key table)
  (cond ((empty? table) #f)
        ((equal? (entry-key (car table)) key) (entry-value (car table)))
        (else (lookup key (cdr table)))))

(define (unique-keys? table)
  (cond ((empty? table) #t)
        ((lookup (entry-key (car table)) (cdr table)) #f)
        (else (unique-keys? (cdr table)))))

; ****************************************************************
; Our representation of Boolean expressions will use the following
; struct definitions.  Note that the transparent attribute allows
; the contents of a structure to be printed out, and allows two
; structures to be compared for content using equal?

(struct bnot (arg) #:transparent)
(struct band (arg1 arg2) #:transparent)
(struct bor (arg1 arg2) #:transparent)

; We recursively define a representation of Boolean expressions as follows.

; 1) 0 and 1 represent the constants 0 and 1
; 2) Racket symbols represent variables (for example, 'x, 'y, 'z2)
; 3) If exp1 and exp2 represent Boolean expressions, then
;    (bnot exp1) represents the NOT of exp1
;    (band exp1 exp2) represents the AND of exp1 and exp2
;    (bor exp1 exp2) represents the OR of exp1 and exp2

; Some examples of Boolean expressions follow.

(define exp0 (bnot 0))
(define exp1 (bor 'x 'y))
(define exp2 (band 'y 'z))
(define exp3 (band 'w (bor (bnot 'x) 0)))
(define exp4 (bor 'x (bnot 'x)))
(define exp5 (band (bor 'x (bnot 'y)) (bnot (band 0 'z))))

; ****************************************************************
; ** problem 2 ** (10 points)
; Write two procedures

; (boolean-exp? exp)
; (type-of exp)

; (boolean-exp? exp) takes an arbitrary Racket value exp
; and tests to see whether it is a Boolean expression according
; to the definition above, returning #t if so and #f if not.

; (type-of exp)
; that takes a Boolean expression as defined above
; and returns its type as one of the symbols:
;   constant, variable, not, or, and
; Note that the type is determined by the top-level
; operation in case the expression is not a constant or variable.

; Recall that (number? exp) tests whether exp is a number,
; and (symbol? exp) tests whether exp is a symbol, and the
; structs bnot, band, and bor have type predicates bnot?, band?, and bor?

; Examples
;> (boolean-exp? 0)
;#t
;> (boolean-exp? 2)
;#f
;> (boolean-exp? exp0)
;#t
;> (boolean-exp? (band "hi" 'c))
;#f
;> (boolean-exp? (band 'x (bor 0 (bnot 1))))
;#t
;> (boolean-exp? (band 'x (bor 0 (bnot #t))))
;#f
;> (type-of 0)
;'constant
;> (type-of 'hi)
;'variable
;> (type-of (bnot (band 'x 0)))
;'not
;> (type-of (bor (band 'x 'y) (band 'x 'z)))
;'or
;> (type-of (band (bor 0 1) (bnot 'x)))
;'and
; ****************************************************************

(define (boolean-exp? exp)
  (cond ((equal? exp 0) #t)
        ((equal? exp 1)  #t)
        ((symbol? exp)  #t)
        ((bnot? exp)(boolean-exp? (bnot-arg exp)))
        ((band? exp)(and (boolean-exp? (band-arg1 exp)) (boolean-exp? (band-arg2 exp))))
        ((bor? exp)(and (boolean-exp? (bor-arg1 exp)) (boolean-exp? (bor-arg2 exp))))
        (else #f)))
            
(define (type-of exp)
  (cond ((number? exp) 'constant)
        ((symbol? exp) 'variable)
        ((bor? exp) 'or)
        ((band? exp) 'and)
        ((bnot? exp) 'not)
        (else 'not_valid)))

; ****************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (all-vars exp)

; that takes a Boolean expression exp 
; and makes a list containing all the variables
; that occur in exp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in exp (scanning left to right.)

; Hint: selectors, type-of and deep recursion on the structure 
; of Boolean expressions.  Note that there is a Racket procedure
; remove-duplicates.

; Examples
;> (all-vars 0) 
;'()
;> (all-vars (bnot (band 'x (band 'y (bor 'x 'z)))))
;'(x y z)
;> (all-vars (band 1 (bor 0 (bnot 'u))))
;'(u)
;> (all-vars (band (band 'x 'y) (band 'y 'x)))
;'(x y)
;> (all-vars (bor (bor (bor 'c 'b) (bor 'a 'b)) 'c))
;'(c b a)
;> 
; ****************************************************************

(define (all-vars exp)
  (cond ((number? exp) '())
        ((symbol? exp) (list exp))
        ((bnot? exp)(remove-duplicates (all-vars (bnot-arg exp))))
        ((band? exp)(remove-duplicates(append (all-vars (band-arg1 exp)) (all-vars (band-arg2 exp)))))
        ((bor? exp)(remove-duplicates(append (all-vars (bor-arg1 exp)) (all-vars (bor-arg2 exp)))))
        (else '())))

; ****************************************************************
; We represent an environment as a table each entry of which
; has a key that is a Racket symbol and a value that is 0 or 1,
; which specifies the truth value of that variable in the environment.
; For example:

(define environ1
  (list
   (entry 'x 0) (entry 'y 1) (entry 'z 0)))
  
(define environ2
  (list
   (entry 'u 0) (entry 'x 1) (entry 'w 1) (entry 'y 0) (entry 'z 1)))

; ****************************************************************
; ** problem 4 ** (10 points)
; Write a procedure 

; (eval-in-env exp env)

; that takes a Boolean expression exp and an environment env
; (represented as described above) and returns 0 or 1 giving 
; the value of the expression in the environment.

; If the Boolean expression contains variables that do not
; occur in the environment, (eval-in-env exp env) should
; return the symbol: 'unspecified-variable.
; (You may want to check for this condition first.)

; Hint: deep recursion on the structure of Boolean expressions
; along with argument selectors and type-of.

; Examples
;> (eval-in-env 1 environ1)
;1
;> (eval-in-env (bor 0 0) '())
;0
;> (eval-in-env 'x environ1)
;0
;> (eval-in-env 'x environ2)
;1
;> (eval-in-env (bnot 'z) environ1)
;1
;> (eval-in-env (bor 'y (bnot 'x)) environ2)
;0
;> (eval-in-env (band (band (bor 'u 'x) (bor 'w 0)) (bnot (band 'y 'z))) environ2)
;1
;> (eval-in-env exp5 environ1)
;0
;> (eval-in-env (band 'y (bor 'x 'u)) (list (entry 'x 0) (entry 'y 1)))
;'unspecified-variable
;>

;(define environ1
;  (list
;   (entry 'x 0) (entry 'y 1) (entry 'z 0)))
;  
;(define environ2
;  (list
;   (entry 'u 0) (entry 'x 1) (entry 'w 1) (entry 'y 0) (entry 'z 1)))

; ****************************************************************

(define (eval-in-env exp env)
  (if (leftoverHELP (all-vars exp) env)
      (cond ((equal? exp 0)  0)
        ((equal? exp 1)  1)      
        ((symbol? exp) (eval-in-env (lookup exp env) env))
        ((bnot? exp) (if (equal? (eval-in-env (bnot-arg exp) env) 0)
                         1
                         0))
        ((band? exp) (let ([x1(eval-in-env (band-arg1 exp) env)][x2(eval-in-env (band-arg2 exp) env)])
                       (if (and (= x1 1) (= x2 1))
                           1
                           0)))
        ((bor? exp) (let ([x1 (eval-in-env (bor-arg1 exp) env)][x2(eval-in-env (bor-arg2 exp) env)])
                       (if (or (= x1 1) (= x2 1))
                           1
                           0)))
        (else 'unspecified-variable))
      'unspecified-variable))


;helper method checks if the variables in the expression are present in the environment.
;it goes through the list of variables one-by-one and calls the method lookup to see if they are present in env.
;returns false if a variable is unspecified and true otherwise
(define (leftoverHELP exp env)
  (cond ((empty? exp) #t)
        ((lookup (car exp) env) (leftoverHELP (cdr exp) env))
        (else #f)))

; ****************************************************************
; We define a truth table as represented by the following struct

(struct tt (vars rows) #:transparent)

; whose fields contain the following
; (1) a (possibly empty) list of n distinct variables, and
; (2) a table containing an entry for each combination of n 0's and 1's,
; whose key is a list of n 0's and 1's and whose value is the value (0 or 1)
; of the function represented by the truth table.

; Note that the entries in a truth table should be in increasing order of
; their keys, considered as binary numbers.

; Examples of truth tables for the expressions
; (bnot 'x), (band 'x 'y), (bor (bnot 'a) 'b) 
; and the function that is the XOR of 'u and 'v.

(define tt-not (tt '(x)
                   (list
                    (entry '(0) 1)
                    (entry '(1) 0))))

(define tt-and (tt '(x y)
                   (list 
                    (entry '(0 0) 0)
                    (entry '(0 1) 0)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
                    
 
(define tt-imp (tt '(a b)
                   (list
                    (entry '(0 0) 1)
                    (entry '(0 1) 1)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
  
(define tt-xor (tt '(u v)
                   (list
                    (entry '(0 0) 0)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))

; Here is a truth table for a function of three arguments a, b, c.

(define tt-f1 (tt '(a b c)
                  (list
                   (entry '(0 0 0) 0)
                   (entry '(0 0 1) 0)
                   (entry '(0 1 0) 1)
                   (entry '(0 1 1) 1)
                   (entry '(1 0 0) 0)
                   (entry '(1 0 1) 1)
                   (entry '(1 1 0) 0)
                   (entry '(1 1 1) 1))))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (all-combs n)

; that takes a non-negative integer n and creates the list of all 
; lists of n 0's or 1's in the *specific order* required for
; a truth table.  In other words, the lists, interpreted as binary numbers, 
; should be in increasing order.

; Hint: if a recursive call gives the correct answer
; for (all-combs 2), what needs to happen to it
; to give the correct answer for (all-combs 3)?
; (This may remind you of power-set from assignment #1.)

; Use let or let* to avoid recomputing the recursive call!

; Examples
;> (all-combs 0)
;'(())
;> (all-combs 1)
;'((0) (1))
;> (all-combs 2)
;'((0 0) (0 1) (1 0) (1 1))
;> (all-combs 3)
;'((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
;> 
; ****************************************************************

(define (all-combs n)
 ( recurse n '(()) ))

;method runs until n is zero, appending the results of addz and addo each time it is called.
(define (recurse n finalList)
  (if (equal? n 0)
      finalList
      (recurse ( - n 1) ( append (addz finalList '()) (addo finalList '()) ))))

;takes every item in final and
;cons a zero onto the front of the first item of the list, THEN cons onto the adjusted list
;takes the cdr of the final list each time it runs
;stops running when the final list is empty
(define (addz final adjusted)
  (if (empty? final)
      (reverse adjusted)
      (addz (cdr final) (cons (cons 0 (car final)) adjusted))))

;takes every item in final and
;cons a one onto the front of the first item of the list, THEN cons onto the adjusted list
;takes the cdr of the final list each time it runs
;stops running when the final list is empty
(define (addo final adjusted)
  (if (empty? final)
      (reverse adjusted)
      (addo (cdr final) (cons (cons 1 (car final)) adjusted))))

; ****************************************************************
; ** problem 6 ** (10 points)
; Write a procedure

; (truth-table exp)

; that takes a Boolean expression exp and returns the truth table for exp
; where the variables for the table are extracted from exp using all-vars, 
; and the function value for each row is obtained by evaluating exp 
; in the corresponding environment.  Notice that all-vars specifies
; the order of variables for the truth table.

; Examples:
;> (truth-table exp0)
;(tt '() (list (entry '() 1)))
;> (truth-table exp1)
;(tt
; '(x y)
; (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1)))
;> (truth-table exp5)
;(tt
; '(x y z)
; (list
;  (entry '(0 0 0) 1)
;  (entry '(0 0 1) 1)
;  (entry '(0 1 0) 0)
;  (entry '(0 1 1) 0)
;  (entry '(1 0 0) 1)
;  (entry '(1 0 1) 1)
;  (entry '(1 1 0) 1)
;  (entry '(1 1 1) 1)))
;> 
; **************************************************************

(define (truth-table exp)
  (let ([allVARIABLES (all-vars exp)] [allCOMBOS (all-combs (length (all-vars exp)))])
    (tt allVARIABLES (ttHELP exp (reverse allCOMBOS) allVARIABLES '()))))

;creates an entry with the first item in allCOMBOS as the key, and the value as the eval-in-env of attSymbols
;keep cycling through until allCOMBOS is empty
(define (ttHELP exp allCOMBOS allVARIABLES newLst)
  (cond ((empty? allCOMBOS) newLst)
        (else (ttHELP exp (cdr allCOMBOS) allVARIABLES (cons (entry (car allCOMBOS) (eval-in-env exp (attSymbols allVARIABLES (car allCOMBOS) '()))) newLst)))))

;create an entry with the first item in symbL as the key, and the first of powersetList as the value
;keep cycling through until symbL is empty
(define (attSymbols symbL powersetList attnew)
  (cond ((empty? symbL) attnew)
        (else (attSymbols (cdr symbL) (cdr powersetList) (cons (entry (car symbL) (car powersetList)) attnew)))))
         
; ****************************************************************
; ** problem 7 ** (10 points)
; Write two procedures

; (satisfiable? exp)
; (equivalent? exp1 exp2)

; (satisfiable? exp)
; takes one Boolean expression exp and
; returns #t if exp is satisfiable and #f otherwise.

; (equivalent? exp1 exp2)
; takes two Boolean expressions exp1 and exp2
; and returns #t if they are equivalent and #f if they
; are not equivalent.

; A Boolean expression is satisfiable if there exists an environment
; in which its value is 1.  Two Boolean expressions are equivalent
; if for every environment which assigns values to all the variables
; in either expression, they have the same value in that environment.

; One possibility might be to use satisfiable? to help implement equivalent?

; These procedures will be tested on expressions with few enough
; variables that generating truth tables will be a feasible approach.

; Examples:
;>  (satisfiable? 0)
;#f
;> (satisfiable? 1)
;#t
;>  (satisfiable? (band 'x (band 'y 'z)))
;#t
;> (satisfiable? (band 'x (band 'y (bnot 'y))))
;#f
;>  (satisfiable? (band (bor 'x (bnot 'y)) 0))
;#f
;>  (equivalent? 0 (band 'a (bnot 'a)))
;#t
;>  (equivalent? 0 'a) 
;#f
;>  (equivalent? (bor 'x (bor 'y 'z)) (bor 0 (bor 'z (bor 'x 'y))))
;#t
;> (equivalent? (bor 'x (band 'y 'z)) (band (bor 'x 'y) (bor 'x 'z)))
;#t
;> 
; ****************************************************************

(define (satisfiable? exp)
  (satHELP (tt-rows (truth-table exp))))

;cycles through the tt to see if any of the results are true
(define (satHELP tbl)
  (cond ((empty? tbl) #f)
        ((equal? (entry-value (car tbl)) 1) #t)
        (else (satHELP (cdr tbl)))))


(define (equivalent? exp1 exp2)
    (eqiHELP (tt-rows (truth-table (bor (band exp1 exp2) (band (bnot exp1) (bnot exp2)))))))

;cycles through the given truth table, and if every expression evaluates to 1, returns true
;otherwise, returns false
(define (eqiHELP tt-equ)
  (cond ((empty? tt-equ) #t)
        ((equal? 1 (entry-value (car tt-equ))) (eqiHELP (cdr tt-equ)))
        (else #f)))

; ****************************************************************
; ** problem 8 ** (10 points)
; Write a procedure

; (find-exp tt)

; This procedure takes a truth table
; and returns a Boolean expression 
; for the given truth table.

; You may choose to use the sum-of-products algorithm
; from lecture, or some other method.
; Please include comments explaining your method
; in either case.

;Examples
;>  (boolean-exp? (find-exp tt-and))
;#t
;>  (equivalent? (find-exp tt-and) (band 'x 'y))
;#t
;>  (equivalent? (find-exp tt-imp) (bor (bnot 'a) 'b)) 
;#t
;>  (equivalent? (find-exp tt-xor) (bor (band 'u (bnot 'v)) (band (bnot 'u) 'v)))
;#t
;>  (boolean-exp? (find-exp tt-f1))
;#t
;> 
; ****************************************************************

(define (find-exp tt)
  (if (empty? (tt-vars tt))
      (entry-value (car (tt-rows tt)))
      (findexpHELP (tt-rows tt) '() (tt-vars tt))))

;creates a list of the entries that evaluate to 1
(define (findexpHELP tbl lst varsLst)
  (cond ((empty? tbl) (boolOR lst varsLst))
        ((equal? (entry-value (car tbl)) 1)
               (findexpHELP (cdr tbl) (cons (entry-key (car tbl)) lst) varsLst ))
        (else (findexpHELP (cdr tbl) (reverse lst) varsLst))))

;if the list is greater than two in length, creates a bor and calls boolAND on arg1 and a boolOR on arg2
;if the list is two in length, creates a bor and calls boolAND on arg1 and a boolAND on arg2
;if list is 1 long, calls boolAND
(define (boolOR truTbl varsLst)
  (cond ((> (length truTbl) 2) (bor (boolAND (car truTbl) varsLst) (boolOR (cdr truTbl) varsLst)))
        ((= (length truTbl) 2) (bor (boolAND (car truTbl) varsLst) (boolAND (car (rest truTbl)) varsLst)))
        ((= (length truTbl) 1) (boolAND (car truTbl) varsLst))
        (else #f)
        ))

;if the list is greater than two in length, creates a band and calls boolNOT on arg1 and a boolAND on arg2
;if the list is two in length, creates a band and calls boolNOT on arg1 and a boolNOT on arg2
;if list is 1 long, calls boolNOT
(define (boolAND truTbl varsLst)
  (cond ((> (length truTbl) 2) (bor (boolNOT (car truTbl) (car varsLst)) (boolAND (cdr truTbl) (cdr varsLst))))
        ((= (length truTbl) 2) (band (boolNOT (car truTbl) (car varsLst)) (boolNOT (car (cdr truTbl)) (car (rest varsLst)))))
        ((= (length truTbl) 1) (boolNOT (car (car truTbl)) (car varsLst)))
        (else #f)
        ))

;if num is 0, returns bnot
;otherwise, returns variable
(define (boolNOT num var)
  (cond ((= num 0)(bnot var))
        (else var)))
 
; ****************************************************************
; ** problem 9 ** (10 points)
; Write a procedure

; (substitute-in exp sub-table)

; that takes a Boolean expression exp and a table sub-table each of whose
; entries has a key that is a variable and a value that is a
; Boolean expression, and returns the Boolean expression obtained
; by replacing every occurrence in exp of a variable in the sub-table with
; the corresponding Boolean expression.

; Hint: type-of and deep recursion on the structure of Boolean expressions.

; Examples
;> (substitute-in 0 (list (entry 'x 1)))
;0
;> (substitute-in 'x (list (entry 'x 1)))
;1
;> (substitute-in (band 'x 'y) (list (entry 'x (bnot 'z)) (entry 'y 0)))
;(band (bnot 'z) 0)
;> (substitute-in (band (bor 'x 'y) (bor (bnot 'x) 'y)) (list (entry 'x (bnot 1))))
;(band (bor (bnot 1) 'y) (bor (bnot (bnot 1)) 'y))
;> 
; ****************************************************************

(define (substitute-in exp sub-table)
  (cond ((number? exp) exp)
        ((symbol? exp)
         (if (equal? (lookup exp sub-table) #f)
             exp
             (lookup exp sub-table)))
        ((bnot? exp) (bnot (substitute-in (bnot-arg exp) sub-table)))
        ((bor? exp) (bor (substitute-in (bor-arg1 exp) sub-table) (substitute-in (bor-arg2 exp) sub-table)))
        ((band? exp) (band (substitute-in (band-arg1 exp) sub-table) (substitute-in (band-arg2 exp) sub-table)))))

; ****************************************************************
; ** problem 10 ** (10 points)
; Write a procedure

; (match exp pat)

; that takes two Boolean expressions exp and pat and
; attempts to "match" the expression exp to the pattern pat.
; That is, it tries to find a table sub-table (as in the preceding
; problem) such that (substitute-in pat sub-table) returns
; the expression exp.  If this is not possible, it should return #f.

; Examples
;> (match 1 1)
;;'()
;> (match 0 'x)
;;(list (entry 'x 0))
;> (match 'z 0)
;;#f
;>  (match 'z 'x)
;;(list (entry 'x 'z))
;> (match (band 'y 'x) 'x)
;;(list (entry 'x (band 'y 'x)))
;>  (match (bnot (band 'a 'b)) (bnot 'z))
;;(list (entry 'z (band 'a 'b)))
;> (match (bor (band 'x 'y) 0) (bor 'a 0))
;;(list (entry 'a (band 'x 'y)))
;> (match (band (bor (bnot 'z) 0) (bor (bnot 'z) 'y)) (band (bor 'a 'b) (bor 'a 'c)))
;;(list (entry 'a (bnot 'z)) (entry 'b 0) (entry 'c 'y))
;> (match (band (band 'x 'y) 'z) (band (bor 'a 'b) 'c))   
;;#f
;> (match (band 'x 'y) (band 'a 'a));###################### if it was a and b would it be true? What about (0 (bnot x))
;;#f
;> (substitute-in (band 'a 'a) (match (band (bor 0 1) (bor 0 1)) (band 'a 'a)))
;;(band (bor 0 1) (bor 0 1))
;;> 
; ****************************************************************

(define (match exp pat)
  (let ([x (match2 exp pat)])
    (cond ((list? x)
           (let ([y (remove-duplicates x)])
           (if (unique-keys? y)
               y
               #f)))
           (else #f))))
        
;if exp and pat are equal, return '()
;if pat is a number return #f
;if pat is a symbol, create an entry that designates pat as the key and pat as the value
;if both are bnot, pass arg into match
;if both are band, pass the arg1's into match and the arg2's into match
;     if both return list, append the results, otherwise return false
;if both are bor, pass the arg1's into match and the arg2's into match
;     if both return list, append the results, otherwise return false
;if none of the above, return false
(define (match2 exp pat)
  (cond ((equal? exp pat) '())
        ((number? pat) #f)
        ((symbol? pat) (list (entry pat exp)))
        ((and (bnot? pat) (bnot? exp)) (match2 (bnot-arg exp) (bnot-arg pat)))
        ((and (band? pat) (band? exp))
                                      (if (and (list? (match2 (band-arg1 exp) (band-arg1 pat)))(list? (match2 (band-arg2 exp) (band-arg2 pat))))
                                          (append (match2 (band-arg1 exp) (band-arg1 pat)) (match2 (band-arg2 exp) (band-arg2 pat)))
                                          #t))
        ((and (bor? pat) (bor? exp))
                                     (if (and (list? (match2 (bor-arg1 exp) (bor-arg1 pat)))(list? (match2 (bor-arg2 exp) (bor-arg2 pat))))
                                         (append (match2 (bor-arg1 exp) (bor-arg1 pat)) (match2 (bor-arg2 exp) (bor-arg2 pat)))
                                         #f))
        (else #f)
        ))
                  
; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))


(test 'lookup  (lookup "first" test1) 1)
(test 'lookup  (lookup "third" test1) #f)
(test 'lookup  (lookup "firth" test1) #f)
(test 'lookup  (lookup 'x test2) 0)
(test 'lookup  (lookup 'z test2) 1)
(test 'lookup  (lookup 'z test2) 1)
(test 'lookup  (lookup 'o test2) #f)
(test 'lookup  (lookup 'o test3) #f)

(test 'unique-keys?  (unique-keys? test1) #t)
(test 'unique-keys?  (unique-keys? test2) #f)
(test 'unique-keys?  (unique-keys? test3) #t)
(test 'unique-keys?  (unique-keys? test4) #t)

(test 'boolean-exp?  (boolean-exp? 0) #t)
(test 'boolean-exp?  (boolean-exp? 2) #f)
(test 'boolean-exp?  (boolean-exp? 'o) #t)
(test 'boolean-exp?  (boolean-exp? #t) #f)
(test 'boolean-exp?  (boolean-exp? #f) #f)
(test 'boolean-exp?  (boolean-exp? exp0) #t)
(test 'boolean-exp?  (boolean-exp? (band "hi" 'c)) #f)
(test 'boolean-exp?  (boolean-exp? (band 'o 'c)) #t)
(test 'boolean-exp?  (boolean-exp? exp0) #t)
(test 'boolean-exp?  (boolean-exp? (band 'x (bor 0 (bnot 1)))) #t)
(test 'boolean-exp?  (boolean-exp? (band 'x (bor 0 (bnot #t)))) #f)

(test 'type-of  (type-of 0) 'constant)
(test 'type-of  (type-of 'hi) 'variable)
(test 'type-of  (type-of (bnot (band 'x 0))) 'not)
(test 'type-of  (type-of (bor (band 'x 'y) (band 'x 'z))) 'or)
(test 'type-of  (type-of (band (bor 0 1) (bnot 'x))) 'and)

(test 'all-vars (all-vars 0) '())
(test 'all-vars (all-vars (bnot (band 'x (band 'y (bor 'x 'z))))) '(x y z))
(test 'all-vars (all-vars (band 1 (bor 0 (bnot 'u)))) '(u))
(test 'all-vars (all-vars (band (band 'x 'y) (band 'y 'x))) '(x y))
(test 'all-vars (all-vars (bor (bor (bor 'c 'b) (bor 'a 'b)) 'c)) '(c b a))

(test 'eval-in-env (eval-in-env 1 environ1) 1)
(test 'eval-in-env (eval-in-env (bor 0 0) '()) 0)
(test 'eval-in-env (eval-in-env 'x environ1) 0)
(test 'eval-in-env (eval-in-env 'x environ2) 1)
(test 'eval-in-env (eval-in-env (bnot 'z) environ1) 1)
(test 'eval-in-env (eval-in-env (bor 'y (bnot 'x)) environ2) 0)
(test 'eval-in-env (eval-in-env (band (band (bor 'u 'x) (bor 'w 0)) (bnot (band 'y 'z))) environ2) 1)
(test 'eval-in-env (eval-in-env exp5 environ1) 0)
(test 'eval-in-env (eval-in-env (band 'y (bor 'x 'u)) (list (entry 'x 0) (entry 'y 1))) 'unspecified-variable)
(test 'eval-in-env (eval-in-env 'u (list (entry 'x 0) (entry 'y 1))) 'unspecified-variable)
(test 'eval-in-env (eval-in-env 'x '()) 'unspecified-variable)
(test 'eval-in-env (eval-in-env (bnot 0) '()) 1)
 
(test 'all-combs (all-combs 0) '(()))
(test 'all-combs (all-combs 1) '((0) (1)))
(test 'all-combs (all-combs 2) '((0 0) (0 1) (1 0) (1 1)))
(test 'all-combs (all-combs 3) '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1)))

(test 'truth-table (truth-table exp0) (tt '() (list (entry '() 1))))
(test 'truth-table (truth-table exp1) (tt '(x y) (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1))))
(test 'truth-table (truth-table exp3) (tt '(w x) (list (entry '(0 0) 0) (entry '(0 1) 0) (entry '(1 0) 1) (entry '(1 1) 0))))
(test 'truth-table (truth-table exp5) (tt
				       '(x y z)
				       (list
					(entry '(0 0 0) 1)
					(entry '(0 0 1) 1)
					(entry '(0 1 0) 0)
					(entry '(0 1 1) 0)
					(entry '(1 0 0) 1)
					(entry '(1 0 1) 1)
					(entry '(1 1 0) 1)
					(entry '(1 1 1) 1))))

(test satisfiable?  (satisfiable? 0) #f)
(test satisfiable? (satisfiable? 1) #t)
(test satisfiable?  (satisfiable? (band 'x (band 'y 'z))) #t)
(test satisfiable? (satisfiable? (band 'x (band 'y (bnot 'y)))) #f)
(test satisfiable?  (satisfiable? (band (bor 'x (bnot 'y)) 0)) #f)

(test equivalent?  (equivalent? 0 (band 'a (bnot 'a))) #t)
(test equivalent?  (equivalent? 1 (band 'a (bnot 'a))) #f)
(test equivalent?  (equivalent? 0 'a) #f)
(test equivalent?  (equivalent? 0 0) #t)
(test equivalent?  (equivalent? 1 1) #t)
(test equivalent?  (equivalent? 1 (bor 'x (bnot 'x))) #t)
(test equivalent?  (equivalent? (bor 'x (bor 'y 'z)) (bor 0 (bor 'z (bor 'x 'y)))) #t);***************
(test equivalent?  (equivalent? (bor 'x (band 'y 'z)) (band (bor 'x 'y) (bor 'x 'z))) #t)
(test equivalent?  (equivalent? (bor 'a 'b) (bor 'c 'd)) #f);;*****************************************************************************8*********************************************************************************
(test equivalent?  (equivalent? (bor (bor 'x (bnot 'x)) 'y) (bor 1 'y)) #t)

(test 'find-exp  (boolean-exp? (find-exp tt-and)) #t)
(test 'find-exp  (find-exp (truth-table 0)) 0)
(test 'find-exp  (find-exp (truth-table (bnot 0))) 1)
(test 'find-exp  (equivalent? (find-exp tt-and) (band 'x 'y)) #t)
(test 'find-exp  (equivalent? (find-exp tt-imp) (bor (bnot 'a) 'b)) #t)
(test 'find-exp  (equivalent? (find-exp tt-xor) (bor (band 'u (bnot 'v)) (band (bnot 'u) 'v))) #t)
(test 'find-exp  (boolean-exp? (find-exp tt-f1)) #t)
(test 'find-exp  (find-exp tt-f1) (bor (bor 'a (band 'b 'c)) (bor (bor (bnot 'a) (band 'b 'c)) (bor (bor (bnot 'a) (band 'b (bnot 'c))) (bor 'a (band (bnot 'b) 'c))))))

(test 'substitute-in (substitute-in 0 (list (entry 'x 1))) 0)
(test 'substitute-in (substitute-in 1 (list (entry 'x 1))) 1)
(test 'substitute-in (substitute-in 'x (list (entry 'x 1))) 1)
(test 'substitute-in (substitute-in (band 'x 'y) (list (entry 'x (bnot 'z)) (entry 'y 0))) (band (bnot 'z) 0))
(test 'substitute-in (substitute-in (band (bor 'x 'y) (bor (bnot 'x) 'y)) (list (entry 'x (bnot 1)))) (band (bor (bnot 1) 'y) (bor (bnot (bnot 1)) 'y)))
(test 'substitute-in (substitute-in
      (bor (band (bor 'x 'y) (bor (bnot 'x) 'y)) (band (bnot 'x) (bor 'a 'x))) (list (entry 'x (bnot 1))))
      (bor (band (bor (bnot 1) 'y) (bor (bnot (bnot 1)) 'y)) (band (bnot (bnot 1)) (bor 'a (bnot 1)))))
(test 'substitute-in (substitute-in (band 'x 'y) (list (entry 'y 0) (entry 'x (bnot 'y)))) (band (bnot 'y) 0))

(test 'match (match 1 1) '())
(test 'match (match 'o 'o) '())
(test 'match (match 0 'x) (list (entry 'x 0)))
(test 'match (match 'z 0) #f)
(test 'match (match 1 0) #f)
(test 'match (match 'x (bnot 'z)) #f)
(test 'match (match (band 'x 'y) (bnot (bor (bnot 'a) (bnot 'b))) ) #f)
(test 'match (match 'z 'x) (list (entry 'x 'z)))
(test 'match (match (band 'y 'x) 'x) (list (entry 'x (band 'y 'x))))
(test 'match (match (bnot (band 'a 'b)) (bnot 'z)) (list (entry 'z (band 'a 'b))))
(test 'match (match (bor (band 'x 'y) 0) (bor 'a 0)) (list (entry 'a (band 'x 'y))))
(test 'match (match (band (bor (bnot 'z) 0) (bor (bnot 'z) 'y)) (band (bor 'a 'b) (bor 'a 'c))) (list (entry 'a (bnot 'z)) (entry 'b 0) (entry 'c 'y)))
(test 'match (match (band (band 'x 'y) 'z) (band (bor 'a 'b) 'c)) #f)
(test 'match (match (band 'x 'y) (band 'a 'a)) #f)
(test 'match (substitute-in (band 'a 'a) (match (band (bor 0 1) (bor 0 1)) (band 'a 'a))) (band (bor 0 1) (bor 0 1)))

(test equivalent?  (equivalent? 'a (band 'a (bor 'b (bnot 'b)))) #t)
(test 'match  (match (bor (band 'x 'y) 0) (bor 0 'a)) #f)
(test 'match  (match (bor (band 'x 'y) 0) (bor 'x 'a))
      (list (entry 'x (band 'x 'y)) (entry 'a 0)))
(test 'match  (match (bor (band (bnot (bor (band (bor 'm 'n) 'o) 'p)) 'q) 'r) (bor (band (bor 'a 'b) 'c) 'd))
 #f)

; **************** end of hw #4 *********************************