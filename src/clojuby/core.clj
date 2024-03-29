(ns clojuby.core
  (:refer-clojure :exclude [eval send])
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import [org.jruby Ruby RubyFixnum RubyHash RubyFloat RubyArray
            RubySymbol RubyString RubyBoolean RubyNil RubyObject
            RubyClass RubyProc RubyModule]
           [org.jruby.ext.set RubySet]
           [org.jruby.javasupport JavaUtil]
           [org.jruby.runtime Block Block$Type Visibility Arity Signature
            CallBlock BlockCallback ThreadContext]
           [org.jruby.runtime.builtin IRubyObject]
           [org.jruby.internal.runtime.methods DynamicMethod CallConfiguration]))

(def ^:private runtime (Ruby/getGlobalRuntime))
(def ^:private context (.getCurrentContext runtime))
(defn raw-eval [code]
  (.evalScriptlet runtime code))

(def ^:private ruby-nil (raw-eval "nil"))
(def ^:private ruby-set (raw-eval "require 'set'; Set"))
(def ^:private ruby-class (raw-eval "Class"))
(def ruby-object (raw-eval "Object"))
(def ruby-module (raw-eval "Module"))
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
  (def args args)
  (->> args (map clj->rb) (into-array IRubyObject)))

(defn- normalize-block [args]
  (let [possible-block (last args)]
    (if (instance? Block possible-block)
      [(vec (butlast args)) possible-block]
      [args Block/NULL_BLOCK])))

(defn send [obj method & args]
  (let [[args block] (normalize-block args)]
    (-> obj
        clj->rb
        (.callMethod context method (normalize-args args) block)
        rb->clj)))

(defrecord Callback [function]
  BlockCallback
  (^IRubyObject call [this
                      ^ThreadContext context
                      ^"[Lorg.jruby.runtime.builtin.IRubyObject;" args
                      ^Block block-param]
    (->> args
         (map rb->clj)
         (apply function)
         clj->rb)))

(defn & [function]
  (CallBlock/newCallClosure ruby-main
                            ruby-object
                            (-> function arity-of-fn
                                Arity/createArity Signature/from)
                            (->Callback function)
                            context))

(def ^:private special-proc
  (raw-eval "
proc { |&f|
  proc { |*args, &b| f.call(self, *args, &b) }
}"))

(defn && [function]
  (let [block (& function)]
    (.. special-proc
        (call context block)
        getBlock)))
    ; #_
    ; (CallBlock/newCallClosure ruby-main
    ;                           ruby-object
    ;                           (-> function arity-of-fn
    ;                               Arity/createArity Signature/from)
    ;                           (->Callback function)
    ;                           context)))

(defn proc [function]
  (let [block (& function)]
    (RubyProc/newProc runtime block Block$Type/PROC)))

(defn send& [obj method & args]
  (let [block (-> args last &)
        args (butlast args)]
    (-> obj
        clj->rb
        (.callMethod context method (normalize-args args) block)
        rb->clj)))

(prefer-method print-method java.util.Map org.jruby.runtime.builtin.IRubyObject)
(prefer-method print-method java.util.RandomAccess org.jruby.runtime.builtin.IRubyObject)
(prefer-method print-method java.util.Set org.jruby.runtime.builtin.IRubyObject)
(prefer-method print-method java.util.List org.jruby.runtime.builtin.IRubyObject)

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
  (rb->clj [this] this)

  nil
  (rb->clj [this] nil))

(extend-protocol RubyCljObject
  java.lang.Long
  (clj->rb [this] (RubyFixnum. runtime this))

  java.lang.Double
  (clj->rb [this] (RubyFloat. runtime this))

  java.lang.String
  (clj->rb [this] (RubyString/newString runtime this))

  clojure.lang.Keyword
  (clj->rb [this] (.fastNewSymbol runtime (name this)))

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
  (clj->rb [this]
           (->> this
                (map clj->rb)
                (into-array IRubyObject)
                (RubySet/create context ruby-set)))

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
      :else (proc me)))

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

(defn create-class!
  ([class-name parent] (create-class! class-name nil parent))
  ([class-name ruby-class-name parent]
   (let [class (send ruby-class "new" parent)]
     (.setBaseName class class-name)
     (when ruby-class-name
       (.defineConstant ruby-object
         (if (string? ruby-class-name)
           ruby-class-name
           class-name)
         class))
     class)))

(defn new-method [class superclass method-name fun]
  (let [bindings (fn [self] {:self self
                             :super (define-super-fn superclass self method-name)})
        call-fn (fn [context self class name args block]
                  (clj->rb (apply fun
                             (bindings self)
                             (map rb->clj args))))
        arity (arity-of-fn fun)
        function-meta (meta fun)
        visibility (cond
                     (:private function-meta) Visibility/PRIVATE
                     (:protected function-meta) Visibility/PROTECTED
                     :else Visibility/PUBLIC)
        method-gen (fn method-gen []
                     (proxy [DynamicMethod] [class
                                             visibility
                                             CallConfiguration/BACKTRACE_AND_SCOPE
                                             method-name]
                       (call [^ThreadContext context, ^IRubyObject self,
                              ^RubyModule class, ^String name,
                              ^"[Lorg.jruby.runtime.builtin.IRubyObject;" args,
                              ^Block block]
                             (call-fn context self class name args block))
                       (getArity [] (Arity/createArity arity))
                       (dup [] (method-gen))))]
    (method-gen)))

(defn add-method! [^RubyClass ruby-class name function]
  (let [superclass (.getSuperClass ruby-class)
        method-name (str/replace-first name "self." "")
        receiving-class (if (str/starts-with? name "self.")
                          (.getMetaClass ruby-class)
                          ruby-class)
        method (new-method ruby-class superclass method-name function)]
    (.addMethod receiving-class method-name method)))

(defn new-class
  ([methods] (new-class ruby-object methods))
  ([superclass methods]
   (let [class (send ruby-class "new" superclass)]
     (doseq [[name fun] methods
             :let [method-name (str/replace-first name "self." "")
                   receiving-class (if (str/starts-with? name "self.")
                                     (.getMetaClass class)
                                     class)
                   method (new-method class superclass method-name fun)]]
       (.addMethod receiving-class method-name method))
     class)))

(spec/def ::ruby-class (spec/and qualified-symbol? #(-> % namespace (= "rb"))))
(spec/def ::rb-or-clj (spec/or :ruby ::ruby-class :clj symbol?))
(spec/def ::extends-part (spec/cat :arrow '#{<} :superclass ::rb-or-clj))
(spec/def ::includes-extends
  (spec/alt :include (spec/cat :_ '#{include} :module ::rb-or-clj)
            :extend (spec/cat :_ '#{extend} :module ::rb-or-clj)
            :prepend (spec/cat :_ '#{prepend} :module ::rb-or-clj)))

(spec/def ::method-definition (spec/cat :name simple-symbol?
                                        :arglist vector?
                                        :body (spec/* any?)))

(spec/def ::new-class-syntax
  (spec/cat :extends (spec/? ::extends-part)
            :body (spec/* (spec/or
                           :extends (spec/spec ::includes-extends)
                           :method (spec/spec ::method-definition)))))

(defn- get-ruby-class [[kind sym-name]]
  (if (= :ruby kind)
    `(raw-eval ~(-> sym-name name (str/replace #"\." "::")))
    sym-name))

(defmacro defclass [class-name & body]
  (let [comformed (spec/conform ::new-class-syntax body)
        {:keys [extends body]} comformed
        ruby-class (gensym "ruby-class-")]
    `(let [~ruby-class (create-class! ~(str class-name)
                                      ~(some-> class-name meta (:ruby-class (str class-name)))
                                      ~(get-ruby-class (:superclass
                                                        extends
                                                        [:ruby 'rb/Object])))]
       ~@(for [[command params] body]
           (case command
             :method `(add-method! ~ruby-class
                                   ~(-> params :name str (str/replace #"-" "_"))
                                   (fn ~(:arglist params) ~@(:body params)))))
       (def ~class-name ~ruby-class))))

(defn set-variable [self name value]
  (.setInstanceVariable self name (clj->rb value)))

(defn get-variable [self name]
  (rb->clj (.getInstanceVariable self name)))

(defn new [class & args]
  (let [arguments (->> args (map clj->rb) (into-array RubyObject))]
    (.newInstance class context arguments Block/NULL_BLOCK)))

(defprotocol SugarifiedSyntax
  (sugarify [_]))

(extend-protocol SugarifiedSyntax
  clojure.lang.Symbol
  (sugarify [this]
    ; (case this
    ;   rb/self `clojuby.core/self
      (if (-> this namespace (= "rb"))
        `(eval ~(-> this name (str/replace #"\." "::")))
        this))

  clojure.lang.PersistentList
  (sugarify [this]
    (let [[fst & rst] this]
      (if (symbol? fst)
        (case fst
          . `(send ~(-> rst first sugarify)
                   ~(-> rst second name (str/replace #"-" "_"))
                   ~@(->> rst (drop  2) (map sugarify)))
          & `(clojuby.core/& ~@(map sugarify rst))
          && `(clojuby.core/&& ~@(map sugarify rst))
          (if (-> fst resolve meta :macro)
            this
            (map sugarify this))))))

  Object
  (sugarify [this] `(clj->rb ~this)))

(defmacro ruby [ & forms]
  (let [forms (map sugarify forms)]
    `(do ~@forms)))

(defmacro rb [obj]
  `(raw-eval ~(str/replace obj "." "::")))

(defmethod print-method IRubyObject [this ^java.io.Writer w]
  (.write w (send this "inspect")))

; -- Helper macros for readers --
(defn as-ruby-obj [form]
  `(rb ~form))

(defn eval-ruby [form]
  `(ruby ~form))
