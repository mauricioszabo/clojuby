(ns clojuby.core-test
  (:require [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]
            [clojuby.core :as rb]))

(deftest convertion-from-ruby
  (testing "converts numbers"
    (check (rb/eval "1") => 1)
    (check (rb/eval "1.1") => 1.1))

  (testing "converts strings and symbols"
    (check (rb/eval "\"foo\"") => "foo")
    (check (rb/eval ":foo") => :foo))

  (testing "converts booleans"
    (check (rb/eval "true") => true)
    (check (rb/eval "false") => false)
    (check (rb/eval "nil") => nil))

  (testing "converts colections"
    (check (rb/eval "{a: 10}") => {:a 10})
    (check (rb/eval "[:a]") => [:a])
    (check (rb/eval "[:a, :b]") => [:a :b])
    (check (rb/eval "[1, 2, :a]") => [1 2 :a])
    (rb/rb-require "set")
    (check (rb/eval "Set[:a, :b]") => #{:a :b}))

  (testing "converts blocks"
    (let [f1 (rb/eval "proc { |x| x + 2}")
          f2 (rb/eval "proc { |x, &b| b.call(x) }")]
      (check (f1 10) => 12)
      (check (f2 10 (rb/& inc)) => 11))))

(deftest conversion-to-ruby
  (testing "converts numbers"
    (check (rb/clj->rb 1) => (rb/raw-eval "1"))
    (check (rb/clj->rb 1.1) => (rb/raw-eval "1.1")))

  (testing "converts strings and symbols"
    (check (rb/clj->rb "foo") => (rb/raw-eval "\"foo\""))
    (check (rb/clj->rb :foo) => (rb/raw-eval ":foo")))

  (testing "converts booleans"
    (check (rb/clj->rb true) => (rb/raw-eval "true"))
    (check (rb/clj->rb false) => (rb/raw-eval "false"))
    (check (rb/clj->rb nil) => (rb/raw-eval "nil")))

  (testing "converts colections"
    (check (rb/clj->rb {:a 10}) => (rb/raw-eval "{a: 10}"))
    (check (rb/clj->rb [1 2 :a]) => (rb/raw-eval "[1, 2, :a]"))
    (check (rb/clj->rb #{1 2 :a}) => (rb/raw-eval "Set[1, 2, :a]")))

  (testing "converts functions"
    (let [plus-2 (rb/clj->rb (fn [a b] (+ a b)))
          plus-n (rb/clj->rb +)
          block-2 (rb/eval "proc { |x| x.call(1, 2) }")
          block-n (rb/eval "proc { |x| x.call(1, 2, 3, 4) }")
          block-block (rb/eval "proc { |x, &b| b.call(x, 1) }")]
      (check (block-2 plus-2) => 3)
      (check (block-n plus-n) => 10)
      (check (block-block 10 (rb/& +)) => 11))))

(deftest ruby-interpretation
  (testing "calls methods"
    (check (rb/public-send 10 "to_s") => "10")
    (check (rb/public-send 10 "to_s" 16) => "a"))

  (testing "calls methods with blocks"
    (check (rb/public-send (rb/eval "1..5") "map" (rb/& inc)) => [2 3 4 5 6])
    (check (rb/public-send& (rb/eval "1..5") "map" inc) => [2 3 4 5 6])
    (check (rb/public-send [1 2 3 4] "map" (rb/& inc)) => [2 3 4 5]))

  (testing "about class creation"
    (testing "creates simple class"
      (let [class (rb/new-class {"sum_two" (fn [_ a b] (+ a b))})
            instance (rb/new class)]
        (check (rb/public-send instance "sum_two" 10 20) => 30)))

    (testing "inherits class methods"
      (let [class (rb/new-class (rb/eval "File") {})]
        (check (rb/public-send class "exist?" "foobar.baz") => false)))

    (testing "calls methods refering to self"
      (let [class (rb/new-class (rb/eval "String") {"append"
                                                     (fn [self a] (str (:self self) "-" a))})
            instance (rb/new class "some-str")]
        (check (rb/public-send instance "append" "foo") => "some-str-foo")))

    (testing "creates class methods"
      (let [class (rb/new-class {"self.foo" (fn [_] "FOO")})]
        (check (rb/public-send class "foo") => "FOO")))

    (testing "refers to 'super'"
      (let [class (rb/new-class (rb/eval "String")
                                {"upcase" (fn [self]
                                            (str "-" ((:super self)) "-" (:self self)))})
            instance (rb/new class "str")]
        (check (rb/public-send instance "upcase") => "-STR-str")))

    (testing "defines a constructor and accesses instance variables"
      (let [class (rb/new-class (rb/eval "String")
                                {"initialize" (fn [self var]
                                               (rb/set-variable (:self self) "@var" var))
                                 "foo" (fn [self]
                                         (rb/get-variable (:self self) "@var"))})
            instance (rb/new class :some-var)]
        (check (rb/public-send instance "foo") => :some-var)))))

(deftest sugared-syntax
  (testing "renames rb/* to pure Ruby calls"
    (check (rb/ruby rb/Object) => (rb/eval "Object")))

  (testing "calls methods on objects"
    (check (rb/ruby (. "foo" upcase)) => "FOO")
    (check (rb/ruby (. rb/Class to-s)) => "Class")
    (check (rb/ruby (. rb/File.Constants name)) => "File::Constants"))

  (testing "self test"
    (check (rb/ruby (. rb/Object instance-eval (& (fn [_]
                                                    (rb/ruby rb/self)))))
           => (rb/raw-eval "Object"))))
  ; (testing "defines classes"
  ;   (rb/ruby
  ;    (defclass SomeClass
  ;      (defn some-method [a b] (+ a b)))
  ;    (.some_method (new SomeClass) 1 2))
  ;   => 3)
  ;
  ; (testing "defines classes subclassing others"
  ;   (rb/ruby
  ;    (defclass SomeClass2 (rb/rb String))
  ;    (.upcase (new SomeClass2 "foo")))
  ;   => "FOO")
  ;
  ; (testing "defines classes subclassing others"
  ;   (rb/ruby
  ;    (defclass SomeClass3 (rb/rb String)
  ;      (defn upcase [] (str (super) "-" self)))
  ;    (.upcase (new SomeClass3 "bar")))
  ;   => "BAR-bar")
  ;
  ; (testing "understands bindings"
  ;   (rb/ruby (defclass SomeClass4 (defn x [] 10)))
  ;   (rb/ruby (.instance-exec (new SomeClass4) 2 (fn [two] (+ two (.x self))))) => 12)
  ;
  ; (testing "plays nice with doto"
  ;   (let [glob (atom 0)]
  ;     (rb/ruby
  ;      (doto (new (defclass DotoExample
  ;                   (defn upd [a] (swap! glob + a))))
  ;            (.upd 10)
  ;            (.upd 2)))
  ;     @glob => 12))
  ;
  ; (testing "plays nice with other macros"
  ;   (rb/ruby
  ;    (-> "some-string" .upcase .chop)) => "SOME-STRIN"))
