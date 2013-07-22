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
    (interpret 2)    => 2
    (interpret '2)   => 2
    (interpret 2.0)  => 2.0
    (interpret 'foo) => (throws))

  (fact "quoting"
    (interpret '(quote foo)) => 'foo
    (interpret '(quote (1 2 3))) => '(1 2 3)
    (interpret (quote '())) => ())

  (fact "env lookup"
    (interpret 't) => true
    (interpret 'null) => '())

  (fact "application"
    (interpret '(+ 1 2)) => 3
    (interpret '(+ (- 2 1) (- 3 1))) => 3
    (interpret '(cons 1 '(2 3))) => '(1 2 3)
    (interpret '(car '(1 2 3))) => 1
    (interpret '(cdr '(1 2 3))) => '(2 3)
    (interpret '(car '())) => '()
    (interpret '(cdr '())) => '()
    (interpret '(null? '())) => true
    (interpret '(null? '(1 2 3))) => '())

  (fact "anonymous functions"
    (interpret '(f (x) (+ x 1))) => fn?
    (interpret '((f (x) (+ x 1)) 10)) => 11
    (interpret '((f (x y) (cons x (cons y '()))) 1 2)) => '(1 2))

  (fact "if"
    (interpret '(if t 1 0)) => 1
    (interpret '(if '() 1 0)) => 0
    (interpret '(if (null? '()) 1 0)) => 1)

  (fact "macros and macroexpand-1"
    (interpret '(macro (x) (list '+ 1 x))) => (comp :macro meta)
    (interpret '((macro (x) (list '+ 1 x)) 1)) => 2
    (interpret '(macroexpand-1 ((macro (x) (list '+ 1 x)) 1))) => '(+ 1 1)
    (interpret '(macroexpand-1 1)) => 1
    (interpret '(macroexpand-1 (+ 1 2))) => '(+ 1 2)))

