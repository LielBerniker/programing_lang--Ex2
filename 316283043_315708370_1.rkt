#lang pl 02
;;---------------------------------------------------------------------------- Question 1 - BNF ------------------------------------------------------------------------------------#|

#|
question 1

   BNF description:
   we create the BNF by the instructions in task1.
   this BNF defines the LE language
   rule number 1, <num> 1, is essential so the LE language could return a number
   rule number 2 { <LE> <LE> } , is essential so the LE language could change a LE expression to a several arguments
   rule number 6 { null }, is essential so the LE language could change a LE expression to null
   the list can have any number of arguments,to make it a valid expression we add the rule (4) { list <LE>} and also the rule (2) { <LE> <LE> }
   rule 4 let the list have a LE and rule 2 lets the list have any number of arguments
   the append can have any number of list arguments ,to make it a valid expression we add the rule (5) { append <LE> } and also the rule (2) { <LE> <LE> }
   rule 5 let the append have a LE and rule 2 let the append have any number of list arguments
   the append can have any number of list arguments ,to make it a valid expression we add the rule (5) { append <LE> } and allso the rule (2) { <LE> <LE> }
   rule 5 let the append have a LE and rule 2 let the append have any number of list arguments
   the cons can have in its second argument only an expression that represents a list,
   an expression that works fine in the course language will work in LE,
   to make it a valid expression we add the rule (3) { append <LE> } and also the rules (1) and (4)
   
   solving process: 
   In the solving process of this question, we had difficulty thinking about all the options possible in the BNF for the LE
   we check again the BNF for the wae and realize all the possibilities we need to add to our BNF

   how much time has been invested to solve the question:
   it takes us around two hours to solve this question

   console other people:
   we did not need to console others for this question
|#

#|
1.
  <LE> ::= | <WORD>
           | { cons <ConsArg>} 2
           | { append <LST> (...) } 3
           | { list <ListArg> (...) } 4

  <WORD> ::= | <num>
           | '<sym>
           | { null }

  <WORDS> ::= | <WORD>
              | {<WORDS> <WORD>}

  <ConsArg> ::= | { <WORD> <LST>}
                | { <LST> <LST>}

  <ListArg> ::= | <WORDS>
                | <LST>

  <LST> ::=       | { list <LST> (...) }
                  | { list <WORDS> (...) }
                  | { cons <ConsArg>} 2
                  | { append <AppendArg> }
                  | {null}
|#







;;---------------------------------------------------------------------------- Question 2 - WAE Improvment  ------------------------------------------------------------------------------------#|
#|
question number 2:

   modification description:
   first, we took the code-named: AE with Eval, from the Interpreters Section in the moodlearn
   and we modify it.

   1) in the BNF we place the operator in the middle of the expration ,we moved the operator in between the AE verebels
   for exmplae: we change { + <AE> <AE> } to { <AE> + <AE> }
   we did this change so the BNF will represent currectly the position of the operator

   2) in the parse-sexpr function we moved the operator in the list at the match sxp , to the middle of the list
   for example: we change (list '+ l r) to (list l '+ r)
   we did this change because the string we get as an input will be in an Infix Syntax, which means the operator will be at the beginning

   3) in eval we add a condition in the match
   this condition is only for the div, we check if the number we divide by is 0,
   if it does we return infinity (999) else we return a (/ (eval l) (eval r)) a regular division
   we add this condition so the function will return infinity before try to divided by 0 
   
 
   solving process:
   In the solving process of this question, we had some difficulty figuring out how to handle the division by zero
   we look back at the AE with Eval code and understood that we need to add the condition in the eval function in the div

   how much time has been invested to solve the question:
   it takes us around 45 minutes to solve this question

   console other people:
   we did not need to console others for this question
|#

#| edit the position of the operators|#
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

#| edit the position of the operators in the list|#
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


#| add a condition that will handle the division by zero by return an infinity(999)|#
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
#|
 - Function description:
   The solution of this question consist of implementation of 2 functions - "sum-of-squares" and "square" (helper function).

   "sum-of-squares" implementation using foldl function with "+" as combiner function , 0 as initial value ,
   and mapping the given list with square function- and use it as input list for foldl.

   "square" implementation simply receives a number and returns the power of this number (multiple this number with itself).

 
 - Solving process and difficulties:
   The main difficulty of this question was understanding how to use map with function as an argument on the given list.

   how much time has been invested to solve the question:
   about two hours to solve this question.

   console other people:
   we checked on the internet how to use map.

|#



(: square : Number -> Number)
(define (square num)
  (* num num))

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))


;;test for square function:
(test (square 4)=> 16)
(test (square (- 0 1))=> 1)
(test (square 0)=> 0)
(test (square 10 )=> 100)


;;test for sum-of-squares function:
(test (sum-of-squares '())=> 0)
(test (sum-of-squares '(0))=> 0)
(test (sum-of-squares '(0 1))=> 1)
(test (sum-of-squares '(2 3 4))=> 29)
(test (sum-of-squares '(10 10 10))=> 300)





;;---------------------------------------------------------------------------- Question 4 - Binary Tree  ------------------------------------------------------------------------------------#|

#|
- Function description:
   a - we created a new type (BINTREE) with two variants : "Node" which receives two BINTREE as arguments and "Leaf" which receives Number as an argument as required.
   b - the function checking which variant the BINTREE argument is (with cases) ,
      if it's a Leaf with number n - the function applies the argument function (f) on n and returns Leaf with f(n).
      o.w , the argument is a Node with two child trees (left , right) the function returns a Node with two children:
         - the left child is the return value from the recursive call to the tree-map function on the original left child tree.
         - the right child is the return value from the recursive call to the tree-map function on the original right child tree.
 
 - Solving process and difficulties:
   The main difficulty of this question was to understand how to use "change" the leaves value and how to call recursively on the child trees and return them as trees/leafs .

   how much time has been invested to solve the question:
   about two hours to solve this question.

   console other people:
   we used the training presentations of the course to look for similar functionality.
|#



;;a
(define-type BINTREE
  [Node BINTREE BINTREE]
  [Leaf Number]
 )


;;b
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map f tree)
 (cases tree
    [(Leaf n) (Leaf (f n))]
    [(Node l r) (Node (tree-map f l)(tree-map f r))]))



;;tests for a :
(test (Leaf (- 5 3)) => (Leaf 2))
(test (Leaf 0) => (Leaf 0))
(test (Leaf (+ 0 1)) => (Leaf 1))
(test (Node (Node (Leaf (+ 6 6)) (Leaf (- 6 6))) (Leaf (- 5 4)))
      => (Node (Node (Leaf 12) (Leaf 0)) (Leaf 1)))
(test (Node (Leaf (+ 4 3)) (Node (Node (Leaf 4) (Leaf 6)) (Leaf (- 2 1))))
      => (Node (Leaf 7) (Node (Node (Leaf 4) (Leaf 6)) (Leaf 1))))




;;tests for b :
(test (tree-map square (Node (Leaf 3) (Leaf 4)))
      => (Node (Leaf 9) (Leaf 16)))
(test (tree-map square (Node (Leaf 2) (Node (Leaf (+ 2 3))(Leaf 6))))
      => (Node (Leaf 4) (Node (Leaf 25) (Leaf 36))))
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf
3))))
 => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))

(test (tree-map square (Node (Node (Leaf (+ 6 6)) (Leaf (- 6 6))) (Leaf (- 5 4))))
      => (Node (Node (Leaf 144) (Leaf 0)) (Leaf 1)))

(test (tree-map add1 (Node (Leaf (+ 4 3)) (Node (Node (Leaf 4) (Leaf 6)) (Leaf (- 2 1)))))
      => (Node (Leaf 8) (Node (Node (Leaf 5) (Leaf 7)) (Leaf 2))))



















