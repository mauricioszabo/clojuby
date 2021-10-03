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
    (check (ast/eval :foo) => :foo))

  (testing "converts booleans"
    (check (ast/eval true) => true)
    (check (ast/eval false) => false)
    (check (ast/eval nil) => nil))

  (testing "converts colections"
    (check (ast/eval {:a 10 :b 20}) => {:a 10 :b 20})
    (check (ast/eval [:a]) => [:a])
    (check (ast/eval [1 2]) => [1 2])
    (check (ast/eval [1 2 :a]) => [1 2 :a])
    (check (ast/eval #{1 2}) => #{1 2})))
  ;
  ; (testing "converts blocks"
  ;   (let [f1 (rb/eval "proc { |x| x + 2}")
  ;         f2 (rb/eval "proc { |x, &b| b.call(x) }")]
  ;     (check (f1 10) => 12)
  ;     (check (f2 10 inc) => 11))))

; (macroexpand-1 '(ast/eval 1))
