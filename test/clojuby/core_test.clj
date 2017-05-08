(ns clojuby.core-test
  (:require [midje.sweet :refer :all]
            [clojuby.core :as rb]))

(facts "about ruby literals"
  (fact "converts numbers"
    (rb/eval "1") => 1
    (rb/eval "1.1") => 1.1
    (rb/clj->rb 1) => (rb/raw-eval "1")
    (rb/clj->rb 1.1) => (rb/raw-eval "1.1"))

  (fact "converts strings and symbols"
    (rb/eval "\"foo\"") => "foo"
    (rb/eval ":foo") => :foo
    (rb/clj->rb "foo") => (rb/raw-eval "\"foo\"")
    (rb/clj->rb :foo) => (rb/raw-eval ":foo"))

  (fact "converts booleans"
    (rb/eval "true") => true
    (rb/eval "false") => false
    (rb/eval "nil") => nil
    (rb/clj->rb true) => (rb/raw-eval "true")
    (rb/clj->rb false) => (rb/raw-eval "false")
    (rb/clj->rb nil) => (rb/raw-eval "nil"))

  (fact "converts colections"
    (rb/eval "{a: 10}") => {:a 10}
    (rb/eval "[:a]") => [:a]
    (rb/eval "[:a, :b]") => [:a :b]
    (rb/eval "[1, 2, :a]") => [1 2 :a]
    (rb/eval "require 'set'; [:a, :b].to_set") => #{:a :b}
    (rb/clj->rb {:a 10}) => (rb/raw-eval "{a: 10}")
    (rb/clj->rb [1 2 :a]) => (rb/raw-eval "[1, 2, :a]")
    (rb/clj->rb #{1 2 :a}) => (rb/raw-eval "Set[1, 2, :a]")))

(facts "about Ruby interpretation"
  (fact "calls methods"
    (rb/public-send "to_s" 10) => "10"
    (rb/public-send "to_s" 10 16) => "a")

  (facts "about class creation"
    (fact "creates simple class"
      (let [class (rb/new-class* {"sum_two" (fn [_ a b] (+ a b))})
            instance (rb/new class)]
        (rb/public-send "sum_two" instance 10 20) => 30))

    (fact "calls methods refering to self"
      (let [class (rb/new-class* (rb/eval "String") {"append"
                                                     (fn [self a] (str self "-" a))})
            instance (rb/new class "some-str")]
        (rb/public-send "append" instance "foo") => "some-str-foo"))

    (fact "refers to 'super'"
      (let [class (rb/new-class* (rb/eval "String")
                                 {"upcase" (fn [self]
                                             (str "-" (rb/eval "super") "-" self))})
            instance (rb/new class "str")]
        (rb/public-send "upcase" instance) => "-STR-str"))))
