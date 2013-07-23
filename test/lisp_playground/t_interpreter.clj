(ns lisp-playground.t-interpreter
  (:require [midje.sweet :refer :all]
            [lisp-playground.interpreter :refer :all]))

(facts "can interpret expressions"

  (fact "atoms"
    ;; We're going to define only two atoms: symbols and
    ;; numbers. Perhaps unfortunately though, interpreting a symbol
    ;; fails because the default interpretation is an environment
    ;; lookup. Evaluating to a symbol actually requires '(quote sym),
    ;; which is not an atom.
    (:val (interpret 2))    => 2
    (:val (interpret '2))   => 2
    (:val (interpret 2.0))  => 2.0
    (interpret 'foo) => (throws))

  (fact "quoting"
    (:val (interpret '(quote foo))) => 'foo
    (:val (interpret '(quote (1 2 3)))) => '(1 2 3)
    (:val (interpret (quote '()))) => ())

  (fact "env lookup"
    (:val (interpret 't)) => true
    (:val (interpret 'null)) => '())

  (fact "application"
    (:val (interpret '(+ 1 2))) => 3
    (:val (interpret '(+ (- 2 1) (- 3 1)))) => 3
    (:val (interpret '(cons 1 '(2 3)))) => '(1 2 3)
    (:val (interpret '(car '(1 2 3)))) => 1
    (:val (interpret '(cdr '(1 2 3)))) => '(2 3)
    (:val (interpret '(car '()))) => '()
    (:val (interpret '(cdr '()))) => '()
    (:val (interpret '(null? '()))) => true
    (:val (interpret '(null? '(1 2 3)))) => '())

  (fact "anonymous functions"
    (:val (interpret '(fn (x) (+ x 1)))) => fn?
    (:val (interpret '((fn (x) (+ x 1)) 10))) => 11
    (:val (interpret '((fn (x y) (cons x (cons y '()))) 1 2))) => '(1 2)
    (:val (interpret '((fn () 1)))) => 1)

  (fact "if"
    (:val (interpret '(if t 1 0))) => 1
    (:val (interpret '(if '() 1 0))) => 0
    (:val (interpret '(if (null? '()) 1 0))) => 1
    (:val (interpret '((fn (n) (if (= n 0) 100 200)) 123))) => 200)

  (fact "macros and macroexpand-1"
    (:val (interpret '(macro (x) (list '+ 1 x)))) => (comp :macro meta)
    (:val (interpret '((macro (x) (list '+ 1 x)) 1))) => 2
    (:val (interpret '(macroexpand-1 ((macro (x) (list '+ 1 x)) 1)))) => '(+ 1 1)
    (:val (interpret '(macroexpand-1 1))) => 1
    (:val (interpret '(macroexpand-1 (+ 1 2)))) => '(+ 1 2))

  (fact "def"
    (:env (interpret '(def x 1))) => #(= 1 (lookup % 'x))
    (:env (interpret '(def x (+ 1 2)))) => #(= 3 (lookup % 'x)))

  (fact "do"
    (:val (interpret '(do 'foo (+ 1 2)))) => 3
    (:val (interpret '(do 'foo))) => 'foo)

  (fact "the dynamic def and do-o"
    (:val (interpret '(do (def inc (fn (x) (+ x 1)))
                          (inc (inc (inc 0))))))
    => 3))

(facts "all together now"

  (fact "functions are closures"
    (:val (interpret
           '(do (def x ((fn (y)
                           (fn () y))
                        1))
                ((fn (y) (x))
                 2))))
    => 1)

  (fact "recursion can be defined using the Y-combinator"
    (:val
     (interpret
      '(do (def Y
             (fn (func)
               ((fn (x)
                  (func (fn (y) ((x x) y))))
                (fn (x)
                  (func (fn (y) ((x x) y)))))))

           (def fact
             (Y (fn (fact)
                  (fn (n)
                    (if (= n 0)
                      1
                      (* n (fact (- n 1))))))))

           (fact 5))))
    => 120)

  (fact "extending fn to support naming"
    (:val
     (interpret
      '(do (def Y
             (fn (func)
               ((fn (x)
                  (func (fn (y) ((x x) y))))
                (fn (x)
                  (func (fn (y) ((x x) y)))))))

           (def fn
             (macro (name args body)
                    (list 'Y (list 'fn (list name)
                                   (list 'fn args body)))))

           (def fact
             (fn recur (n)
               (if (= n 0)
                 1
                 (* n (recur (- n 1))))))

           (fact 5))))
    => 120)

  (fact "can interpret with a lib"
    (:val
     (interpret-with-lib
      '(do (defn fact (n)
             (if (= n 0)
               1
               (* n (fact (- n 1)))))

           (fact 5))))
    => 120))
