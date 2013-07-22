(ns lisp-playground.interpreter
  (:require [clojure.core.match :refer [match]]))

(defprotocol IEnv
  (lookup [e name]
    "Looks up `name` in environment `e`. Throws an exception
     if `name` is not found.")
  (add [e name val]
    "Returns a new environment like `e`, but with `name`
     associated to `val`."))

(defrecord MapEnv [m]
  IEnv
  (lookup [e name]
    (or (get m name)
        (throw (Exception. (str "Name " name
                                " not found in environment "
                                (pr-str e))))))
  (add [e name val]
    (->MapEnv (assoc m name val))))

(defn extend-env
  "Extends an environment using `add` given some associative
   name/value collection."
  [e coll]
  (reduce-kv add e coll))

(def empty-env (->MapEnv {}))

(defn macro?
  "Returns true if the :macro key of f's metadata is set to true."
  [f]
  (= true (:macro (meta f))))

(defn interpret-in-env
  [expr env]
  (match [expr]
    ;; quote form
    ;;   (quote (1 2 3)) => '(1 2 3)
    [(['quote e] :seq)] e

    ;; if form
    ;;   (if (null? ()) 1 0) => 1
    [(['if pred conseq alt] :seq)]
    (if (not= () (interpret-in-env pred env))
      (interpret-in-env conseq env)
      (interpret-in-env alt env))

    ;; anonymous functions
    ;;   (f (x) (+ x 1)) => a Clojure function
    ;;   (f (x y) (cons x (cons y '()))) => also a Clojure function
    [(['f ([& params] :seq) body] :seq)]
    (fn [& args]
      (interpret-in-env body (extend-env env (zipmap params args))))

    ;; anonymous macros
    ;;   (macro (x) (list '+ 1 x)) => a Clojure function w/ :macro metadata
    [(['macro ([& params] :seq) body] :seq)]
    (with-meta (interpret-in-env (list 'f params body) env) {:macro true})

    ;; macroexpansion
    ;;   '(macroexpand-1 ((macro (x) (list '+ 1 x))) 1) => '(+ 1 1)
    [(['macroexpand-1 form] :seq)]
    (if-not (list? form)
      form
      (let [[hd & args] form
            operator (interpret-in-env hd env)]
        (if-not (macro? operator)
          form
          (apply operator args))))

    ;; function and macro application
    ;;   (+ 1 2) => 3
    [([function & args] :seq)]
    (let [f-or-macro (interpret-in-env function env)]
      (if (macro? f-or-macro)
        (interpret-in-env (apply f-or-macro args) env)
        (apply f-or-macro
               (map #(interpret-in-env % env) args))))

    ;; symbol lookup
    ;;   foo => whatever foo is bound to, or an error
    [(sym :guard symbol?)]
    (lookup env sym)

    ;; numbers eval to themselves
    ;;   1 => 1
    ;;   1.0 => 1.0
    [(n :guard number?)]
    n

    [_] (throw (Exception. "fail"))))

(def built-ins
  {'t true
   'null ()
   '+ +
   '- -
   '/ /
   '* *
   'cons cons
   'car #(or (first %) ())
   'cdr rest
   'null? #(or (= % ()) ())
   'list list
   '= =})

(defn interpret
  [expr]
  (interpret-in-env expr (extend-env empty-env built-ins)))
