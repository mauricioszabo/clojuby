(ns clojuby.core-test
  (:require [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]
            [clojuby.core :as rb]))

(deftest ruby-literals
  (testing "converts numbers"
    (check (rb/eval "1") => 1)
    (check (rb/eval "1.1") => 1.1)
    (check (rb/clj->rb 1) => (rb/raw-eval "1"))
    (check (rb/clj->rb 1.1) => (rb/raw-eval "1.1")))

  (testing "converts strings and symbols"
    (check (rb/eval "\"foo\"") => "foo")
    (check (rb/eval ":foo") => :foo)
    (check (rb/clj->rb "foo") => (rb/raw-eval "\"foo\""))
    (check (rb/clj->rb :foo) => (rb/raw-eval ":foo")))

  (testing "converts booleans"
    (check (rb/eval "true") => true)
    (check (rb/eval "false") => false)
    (check (rb/eval "nil") => nil)
    (check (rb/clj->rb true) => (rb/raw-eval "true"))
    (check (rb/clj->rb false) => (rb/raw-eval "false"))
    (check (rb/clj->rb nil) => (rb/raw-eval "nil")))

  (testing "converts colections"
    (check (rb/eval "{a: 10}") => {:a 10})
    (check (rb/eval "[:a]") => [:a])
    (check (rb/eval "[:a, :b]") => [:a :b])
    (check (rb/eval "[1, 2, :a]") => [1 2 :a])
    (rb/rb-require "set")
    (check (rb/eval "Set[:a, :b]") => #{:a :b})

    (check (rb/clj->rb {:a 10}) => (rb/raw-eval "{a: 10}"))
    (check (rb/clj->rb [1 2 :a]) => (rb/raw-eval "[1, 2, :a]"))
    (check (rb/clj->rb #{1 2 :a}) => (rb/raw-eval "Set[1, 2, :a]")))

  (testing "converts blocks"
    (let [f1 (rb/eval "proc { |x| x + 2}")
          f2 (rb/eval "proc { |x, &b| b.call(x) }")]
      (check (f1 10) => 12)
      (check (f2 10 inc) => 11))))

#_
(facts "about Ruby interpretation"
  (fact "calls methods"
    (rb/public-send "to_s" 10) => "10"
    (rb/public-send "to_s" 10 16) => "a")

  (fact "calls methods with blocks"
    (rb/public-send "map" (rb/eval "1..5") inc) => [2 3 4 5 6])

  (facts "about class creation"
    (fact "creates simple class"
      (let [class (rb/new-class {"sum_two" (fn [_ a b] (+ a b))})
            instance (rb/new class)]
        (rb/public-send "sum_two" instance 10 20) => 30))

    (fact "inherits class methods"
      (let [class (rb/new-class (rb/eval "File") {})]
        (rb/public-send "exist?" class "foobar.baz") => false))

    (fact "calls methods refering to self"
      (let [class (rb/new-class (rb/eval "String") {"append"
                                                     (fn [self a] (str (:self self) "-" a))})
            instance (rb/new class "some-str")]
        (rb/public-send "append" instance "foo") => "some-str-foo"))

    (fact "creates class methods"
      (let [class (rb/new-class {"self.foo" (fn [_] "FOO")})]
        (rb/public-send "foo" class) => "FOO"))

    (fact "refers to 'super'"
      (let [class (rb/new-class (rb/eval "String")
                                {"upcase" (fn [self]
                                            (str "-" ((:super self)) "-" (:self self)))})
            instance (rb/new class "str")]
        (rb/public-send "upcase" instance) => "-STR-str"))

    (fact "defines a constructor and accesses instance variables"
      (let [class (rb/new-class (rb/eval "String")
                                {"initialize" (fn [self var]
                                               (rb/set-variable (:self self) "@var" var))
                                 "foo" (fn [self]
                                         (rb/get-variable (:self self) "@var"))})
            instance (rb/new class :some-var)]
        (rb/public-send "foo" instance) => :some-var))))

#_
(facts "with sugared syntax"
  (fact "calls methods on objects"
    (rb/ruby (.upcase "foo")) => "FOO"
    (rb/ruby (.to-s (rb/rb Class))) => "Class"
    (rb/ruby (.name (rb/rb File.Constants))) => "File::Constants")

  (fact "defines classes"
    (rb/ruby
     (defclass SomeClass
       (defn some-method [a b] (+ a b)))
     (.some_method (new SomeClass) 1 2))
    => 3)

  (fact "defines classes subclassing others"
    (rb/ruby
     (defclass SomeClass2 (rb/rb String))
     (.upcase (new SomeClass2 "foo")))
    => "FOO")

  (fact "defines classes subclassing others"
    (rb/ruby
     (defclass SomeClass3 (rb/rb String)
       (defn upcase [] (str (super) "-" self)))
     (.upcase (new SomeClass3 "bar")))
    => "BAR-bar")

  (fact "understands bindings"
    (rb/ruby (defclass SomeClass4 (defn x [] 10)))
    (rb/ruby (.instance-exec (new SomeClass4) 2 (fn [two] (+ two (.x self))))) => 12)

  (fact "plays nice with doto"
    (let [glob (atom 0)]
      (rb/ruby
       (doto (new (defclass DotoExample
                    (defn upd [a] (swap! glob + a))))
             (.upd 10)
             (.upd 2)))
      @glob => 12))

  (fact "plays nice with other macros"
    (rb/ruby
     (-> "some-string" .upcase .chop)) => "SOME-STRIN"))
