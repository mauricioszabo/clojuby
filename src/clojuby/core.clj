(ns clojuby.core
  (:refer-clojure :exclude [eval])
  (:import [org.jruby Ruby RubyFixnum RubyHash RubyFloat RubyArray
            RubySymbol RubyString RubyBoolean RubyNil RubyObject
            RubyClass]
           [org.jruby.runtime Block Visibility Arity]
           [org.jruby.internal.runtime.methods DynamicMethod]))

(def ^:private runtime (Ruby/getGlobalRuntime))
(def ^:private context (.getCurrentContext runtime))
(defn raw-eval [code]
  (.evalScriptlet runtime code))

(def ^:private ruby-nil (raw-eval "nil"))
(def ^:private ruby-object (raw-eval "Object"))

(defprotocol CljRubyObject (rb->clj [this]))
(extend-protocol CljRubyObject
  RubyFixnum
  (rb->clj [this] (.getLongValue this))

  RubySymbol
  (rb->clj [this] (-> this .asJavaString keyword))

  RubyString
  (rb->clj [this] (.decodeString this))

  RubyFloat
  (rb->clj [this] (.getDoubleValue this))

  RubyHash
  (rb->clj [this] (->> this
                       (map (fn [[k v]] [(rb->clj k) (rb->clj v)]))
                       (into {})))

  RubyArray
  (rb->clj [this] (mapv rb->clj this))

  RubyBoolean
  (rb->clj [this] (.isTrue this))

  RubyNil
  (rb->clj [_] nil)

  RubyObject
  (rb->clj [this] (case (-> this .getType .toString)
                    "Set" (->> (.callMethod this context "to_a")
                               (map rb->clj)
                               (into #{}))
                    this))

  Object
  (rb->clj [this] this))

(defprotocol RubyCljObject (clj->rb [this]))
(extend-protocol RubyCljObject
  java.lang.Long
  (clj->rb [this] (RubyFixnum. runtime this))

  java.lang.Double
  (clj->rb [this] (RubyFloat. runtime this))

  java.lang.String
  (clj->rb [this] (RubyString/newString runtime this))

  clojure.lang.Keyword
  (clj->rb [this] (RubySymbol/newSymbol runtime (name this)))

  java.lang.Boolean
  (clj->rb [this] (RubyBoolean/newBoolean runtime this))

  nil
  (clj->rb [_] ruby-nil)

  java.util.Map
  (clj->rb [this] (->> this
                       (map (fn [[k v]] [(clj->rb k) (clj->rb v)]))
                       (into {})
                       (#(RubyHash/newHash runtime % ruby-nil))))

  java.util.List
  (clj->rb [this] (->> this
                       (map clj->rb)
                       (#(RubyArray/newArray runtime %))))

  java.util.Set
  (clj->rb [this] (->> this
                       (map clj->rb)
                       (#(.callMethod (RubyArray/newArray runtime %)
                                      context
                                      "to_set"))))

  Object
  (clj->rb [this] this))

(defn- normalize-args [args]
  (->> args (map clj->rb) (into-array RubyObject)))

(defn public-send [method obj & args]
  (-> obj
      clj->rb
      (.callMethod context method (normalize-args args) Block/NULL_BLOCK)
      rb->clj))

(defn eval [code]
  (-> code raw-eval rb->clj))

(defn ruby [x]
  (println x "Hello, World!"))

(defn- arity-of-fn [f]
  (let [m (-> f class .getDeclaredMethods)]
    (if (= 2 (count m))
      (-> m second .getParameterTypes alength dec -)
      (-> m first .getParameterTypes alength dec))))

(defn new-class*
  ([methods] (new-class* ruby-object methods))
  ([superclass methods]
   (let [class (RubyClass/newClass runtime superclass)]
     (doseq [[name fun] methods
             :let [arity (arity-of-fn fun)
                   gen (fn gen [] (proxy [DynamicMethod] []
                                    (call [context self class name args block]
                                      (clj->rb (apply fun self (map rb->clj args))))
                                    (getArity [] (Arity/createArity arity))
                                    (dup []
                                      (gen))))]]
       (.addMethod class name (gen)))
     class)))

(defn new [class & args]
  (let [arguments (->> args (map clj->rb) (into-array RubyObject))]
    (.newInstance class context arguments Block/NULL_BLOCK)))
