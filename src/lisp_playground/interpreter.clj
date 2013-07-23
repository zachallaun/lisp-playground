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
    [(['quote e] :seq)]
    {:val e :env env}

    [(['if pred conseq alt] :seq)]
    (if (not= () (:val (interpret-in-env pred env)))
      (interpret-in-env conseq env)
      (interpret-in-env alt env))

    [(['fn ([& params] :seq) body] :seq)]
    {:val (fn [& args]
            (:val (interpret-in-env body (extend-env env (zipmap params args)))))
     :env env}

    [(['macro ([& params] :seq) body] :seq)]
    {:val (with-meta (:val (interpret-in-env (list 'fn params body) env))
                     {:macro true})
     :env env}

    [(['macroexpand-1 form] :seq)]
    {:val (if-not (list? form)
            form
            (let [[hd & args] form
                  operator (:val (interpret-in-env hd env))]
              (if-not (macro? operator)
                form
                (apply operator args))))
     :env env}

    [(['def (name :guard symbol?) val] :seq)]
    (let [binding (:val (interpret-in-env val env))]
      {:val binding
       :env (add env name binding)})

    [(['do form] :seq)]
    (interpret-in-env form env)

    [(['do form & remaining] :seq)]
    (let [{_ :val new-env :env} (interpret-in-env form env)]
      (interpret-in-env (cons 'do remaining) new-env))

    [([function & args] :seq)]
    (let [f-or-macro (:val (interpret-in-env function env))]
      (if (macro? f-or-macro)
        (interpret-in-env (apply f-or-macro args) env)
        {:val (apply f-or-macro
                     (map #(:val (interpret-in-env % env)) args))
         :env env}))

    [(sym :guard symbol?)]
    {:val (lookup env sym) :env env}

    [(n :guard number?)]
    {:val n :env env}

    [_]
    (throw (Exception. "fail"))))

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
   '= #(or (= %1 %2) ())})

(defn interpret
  [expr]
  (interpret-in-env expr (extend-env empty-env built-ins)))

(def stdlib
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

       (def defn
             (macro (name args body)
               (list 'def name
                 (list 'fn name args
                   body))))))

(defn interpret-with-lib
  "A lib is a set of defs within a do"
  ([expr]
     (interpret-with-lib expr stdlib))
  ([expr lib]
     (interpret-in-env expr (:env (interpret lib)))))
