(ns clojuby.core
  (:refer-clojure :exclude [eval])
  (:require [clojuby.sugar :as sugar]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import [org.jruby Ruby RubyFixnum RubyHash RubyFloat RubyArray
            RubySymbol RubyString RubyBoolean RubyNil RubyObject
            RubyClass RubyProc]
           [org.jruby.javasupport JavaUtil]
           [org.jruby.runtime Block Visibility Arity CallBlock BlockCallback]
           [org.jruby.runtime.builtin IRubyObject]
           [org.jruby.internal.runtime.methods DynamicMethod CallConfiguration]))

(def ^:private runtime (Ruby/getGlobalRuntime))
(def ^:private context (.getCurrentContext runtime))
(defn raw-eval [code]
  (.evalScriptlet runtime code))

(def ^:private ruby-nil (raw-eval "nil"))
(def ^:private ruby-class (raw-eval "Class"))
(def ruby-object (raw-eval "Object"))
(def ^:private ruby-main (raw-eval "self"))
(def ^:private ruby-proc-wrapper
  (raw-eval "proc { |fn|
               proc { |*args, &b|
                 args << b if b
                 fn.invoke(self).invoke(*args)
               }
             }"))

(defn- arity-of-fn [f]
  (let [methods (-> f class .getDeclaredMethods)]
    (if (some #(= "getRequiredArity" (.getName %)) methods)
      (-> f .getRequiredArity inc -)
      (->> methods (filter #(= "invoke" (.getName %)))
           first .getParameterTypes alength))))

(defprotocol CljRubyObject (rb->clj [this]))
(defprotocol RubyCljObject (clj->rb [this]))

(defn- normalize-args [args]
  (->> args (map clj->rb) (into-array RubyObject)))

(defn- normalize-block [args]
  (let [possible-block (last args)]
    (if (fn? possible-block)
      [(vec (butlast args)) (clj->rb possible-block)]
      [args Block/NULL_BLOCK])))

(defn public-send [method obj & args]
  (let [[args block] (normalize-block args)]
    (-> obj
        clj->rb
        (.callMethod context method (normalize-args args) block)
        rb->clj)))


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

  RubyProc
  (rb->clj [this]
    (fn [ & args]
      (let [[args block] (normalize-block args)]
        (rb->clj (.call this context (normalize-args args) block)))))

  RubyNil
  (rb->clj [_] nil)

  RubyObject
  (rb->clj [this] (case (-> this .getType .toString)
                    "Set" (->> (.callMethod this context "to_a")
                               (map rb->clj)
                               (into #{}))
                    this))

  IRubyObject
  (rb->clj [this] this)

  Object
  (rb->clj [this] this))

(defn- generate-proc-from-fn [me]
  (let [callback (proxy [BlockCallback] []
                    (call [context args block]
                      (let [args (cond-> (mapv rb->clj args)
                                         (not= block Block/NULL_BLOCK) (conj block))]
                        (clj->rb (apply me args)))))]

    (CallBlock/newCallClosure ruby-main
                              ruby-object
                              (Arity/createArity (arity-of-fn me))
                              callback
                              context)))

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

  clojure.lang.Fn
  (clj->rb [me]
    (cond
      (-> me meta :binding) (.getBlock
                             (.callMethod
                              ruby-proc-wrapper
                              context
                              "call"
                              (into-array RubyObject [(JavaUtil/convertJavaToUsableRubyObject runtime me)])
                              Block/NULL_BLOCK))

      (-> me meta :dont-convert?) me
      :else (generate-proc-from-fn me)))


  IRubyObject
  (clj->rb [this] this)

  Object
  (clj->rb [this] (JavaUtil/convertJavaToUsableRubyObject runtime this)))

(defn eval [code]
  (-> code raw-eval rb->clj))

(defn rb-require [string]
  (let [norm (str/replace string #"\"" "\"\"")]
    (eval (str "require \"" norm "\""))))

(defn- define-super-fn [parent-class self name]
  (fn [ & args]
    (let [unbound (.instance_method parent-class (RubySymbol/newSymbol runtime name))
          bound (.bind unbound context self)]
      (.call bound context (normalize-args args) Block/NULL_BLOCK))))

(defn new-class*
  ([methods] (new-class* ruby-object methods))
  ([superclass methods]
   (let [class (public-send "new" ruby-class superclass)]
     (doseq [[name fun] methods
             :let [arity (arity-of-fn fun)
                   bindings (fn [self] {:self self
                                        :super (define-super-fn superclass self name)})
                   gen (fn gen [] (proxy [DynamicMethod] [class
                                                          Visibility/PUBLIC
                                                          CallConfiguration/BACKTRACE_AND_SCOPE
                                                          name]
                                    (call [context self class name args block]
                                      (clj->rb (apply fun
                                                 (bindings self)
                                                 (map rb->clj args))))
                                    (getArity [] (Arity/createArity arity))
                                    (dup []
                                      (gen))))]]
       (.addMethod class name (gen)))
     class)))

(defn set-variable [self name value]
  (.setInstanceVariable self name (clj->rb value)))

(defn get-variable [self name]
  (rb->clj (.getInstanceVariable self name)))

(defn new [class & args]
  (let [arguments (->> args (map clj->rb) (into-array RubyObject))]
    (.newInstance class context arguments Block/NULL_BLOCK)))

(defmacro ruby [ & forms]
  (sugar/sugarify forms))

(defmacro rb [obj]
  `(raw-eval ~(str/replace obj "." "::")))

(defn as-ruby-obj [form]
  `(rb ~form))

(defn eval-ruby [form]
  `(ruby ~form))
