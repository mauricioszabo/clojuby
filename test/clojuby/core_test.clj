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
    (rb/require "set")
    (rb/eval "[:a, :b].to_set") => #{:a :b}
    (rb/clj->rb {:a 10}) => (rb/raw-eval "{a: 10}")
    (rb/clj->rb [1 2 :a]) => (rb/raw-eval "[1, 2, :a]")
    (rb/clj->rb #{1 2 :a}) => (rb/raw-eval "Set[1, 2, :a]"))

  (fact "converts blocks"
    (let [f1 (rb/eval "proc { |x| x + 2}")
          f2 (rb/eval "proc { |x, &b| b.call(x) }")]
      (f1 10) => 12
      (f2 10 inc) => 11)))

(facts "about Ruby interpretation"
  (fact "calls methods"
    (rb/public-send "to_s" 10) => "10"
    (rb/public-send "to_s" 10 16) => "a")

  (fact "calls methods with blocks"
    (rb/public-send "map" (rb/eval "1..5") inc) => [2 3 4 5 6])

  (facts "about class creation"
    (fact "creates simple class"
      (let [class (rb/new-class* {"sum_two" (fn [_ a b] (+ a b))})
            instance (rb/new class)]
        (rb/public-send "sum_two" instance 10 20) => 30))

    (fact "inherits class methods"
      (let [class (rb/new-class* (rb/eval "File") {})]
        (rb/public-send "exist?" class "foobar.baz") => false))

    (fact "calls methods refering to self"
      (let [class (rb/new-class* (rb/eval "String") {"append"
                                                     (fn [self a] (str (:self self) "-" a))})
            instance (rb/new class "some-str")]
        (rb/public-send "append" instance "foo") => "some-str-foo"))

    (fact "refers to 'super'"
      (let [class (rb/new-class* (rb/eval "String")
                                 {"upcase" (fn [self]
                                             (str "-" ((:super self)) "-" (:self self)))})
            instance (rb/new class "str")]
        (rb/public-send "upcase" instance) => "-STR-str"))

    (fact "defines a constructor and accesses instance variables"
      (let [class (rb/new-class* (rb/eval "String")
                                 {"initialize" (fn [self var]
                                                 (rb/set-variable (:self self) "@var" var))
                                  "foo" (fn [self]
                                          (rb/get-variable (:self self) "@var"))})
            instance (rb/new class :some-var)]
        (rb/public-send "foo" instance) => :some-var))))

(facts "with sugared syntax"
  (fact "calls methods on objects"
    (rb/ruby (.upcase "foo")) => "FOO"
    (rb/ruby (.to-s 'Class)) => "Class")

  (fact "defines classes"
    (rb/ruby
     (defclass SomeClass
       (defn some-method [a b] (+ a b)))
     (.some_method (new SomeClass) 1 2))
    => 3)

  (fact "defines classes subclassing others"
    (rb/ruby
     (defclass SomeClass2 'String)
     (.upcase (new SomeClass2 "foo")))
    => "FOO")

  (fact "defines classes subclassing others"
    (rb/ruby
     (defclass SomeClass3 'String
       (defn upcase [] (str (super) "-" self)))
     (.upcase (new SomeClass3 "bar"))
     => "BAR-bar")))
