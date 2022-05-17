#lang pl 02
;;---------------------------------------------------------------------------- Question 1 - BNF ------------------------------------------------------------------------------------#|

#|
1.
The LE grammer

  <LE> ::= <num> 1
           | { <LE> <LE> } 2 
           | { cons <LE> <LE> } 2 
           | { list <LE>} 3
           | { append <LE> <LE> } 4
           | { null } 5
           | { with {<id> <LE>} <LE>} 6
           | <id> 7

Where <id> refers to any valid symbol in Racket, and <num> colud be any Number in Racket.
|#







;;---------------------------------------------------------------------------- Question 2 - WAE Improvment  ------------------------------------------------------------------------------------#|
#|
The AE grammer

  <AE> ::= <num>
           | { <AE> + <AE> }
           | { <AE> - <AE> }
           | { <AE> * <AE> }
           | { <AE> / <AE> }
|#
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])


(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list l '+ r) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list l '- r) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list l '* r) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list l '/ r) (Div (parse-sexpr l)(parse-sexpr r))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))


;assuming we chose prefix form grammer with curly parentheses
(test (parse "{3 + 4 }") => (Add (Num 3)
                                   (Num 4)))
(test (parse "3") => (Num 3))
(test (parse "{{3 - 2} +  4 }") => (Add (Sub (Num 3)
                                              (Num 2))
                                         (Num 4)))
(test (parse "{+ 1 2 3 4}") =error> "bad syntax")



#|
The goal of parse:
Input:  string describing the program
Output: Abstract Syntax Tree (or an exception if the string is not a valid program)

Two main phases:
1. Read -- turn the string into a simple data structure (we will use the Racket type Sexpr).
2. Actual Parsing -- turn an Sexpr into an AST


Definition of the pl type Sexpr:
Basis -- any Number/Symbol is an Sexpr
General -- any list of Sexpr is an Sexpr

|#



#|
;;; ====== EVAL  ==============
; <AE> ::= <num>               a 
;          | { + <AE> <AE> }   b
;          | { - <AE> <AE> }   c

eval(<num>) = <num>
eval({+ E1 E2}) =  eval(E1) + eval(E2)
eval({- E1 E2}) =  eval(E1) - eval(E2)
|#



(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (if(equal? 0 (eval r)) 999 (/ (eval l) (eval r)))]))


(: run : String -> Number)
(define (run code)
  (eval (parse code)))



(test (eval (Num 3)) => 3)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)

(test (eval (parse "{3 + 4 }")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{{3 - 2} +  4 }")) => 5)
(test (eval (parse "{+ 1 2 3 4}")) =error> "bad syntax")

(test (eval (parse "{3 * {5 / 3} }")) => 5)
(test (run "{3 + 4 }") => 7)
(test (run "3") => 3)
(test (run "{{3 - 2} + 4 }") => 5)
(test (run "{+ 1 2 3 4}") =error> "bad syntax")

(test (eval (Div (Num 4) (Num 0))) => 999)
(test (eval (parse "{{{3 + 5} * 4} / {4 - 4}}")) => 999)
(test (run "{{1 + 2} / {3 * 0}}") => 999)



;;---------------------------------------------------------------------------- Question 3 - Sum Of Squares  ------------------------------------------------------------------------------------#|
(: square : Number -> Number)
(define (square num)
  (* num num))

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))

(test (sum-of-squares '(1 2 3))=> 14)







