(defproject lisp-playground "0.0.1-SNAPSHOT"
  :description "Playing around with lisp interpreters and compilers"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.match "0.2.0-rc4"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]}})
