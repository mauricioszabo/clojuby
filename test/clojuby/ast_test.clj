(ns clojuby.ast-test
  (:require [clojuby.ast :as ast]
            [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]))

(deftest conversions
  (testing "converts numbers"
    (check (ast/eval 1) => 1)
    (check (ast/eval 1.1) => 1.1))

  (testing "converts strings and symbols"
    (check (ast/eval "foo") => "foo")
    (check (ast/eval :foo) => :foo)))

; (macroexpand-1 '(ast/eval 1))
