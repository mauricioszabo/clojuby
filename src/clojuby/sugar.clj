(ns clojuby.sugar
  (:require [clojure.string :as str]
            [clojure.walk :as walk])
  (:import [org.jruby.runtime.builtin IRubyObject]))

(defn- normalize-method [name]
  (-> name
      (str/replace-first #"^\." "")
      (str/replace #"\-" "_")))

(defn- clj-or-rb [symbol]
  (when (or (symbol? symbol) (instance? IRubyObject symbol))
    symbol))

(defn as-class-body [body]
  (let [norm-body (fn [sym] (cond
                              (= sym 'super) '(:super self)
                              (= sym 'self) '(:self self)
                              :else sym))
        separate-code #(let [[fun name params & body] %2
                             body (walk/postwalk norm-body body)]
                         (assoc %1 (normalize-method name)
                                 `(fn ~name ~(->> params (cons 'self) vec) ~@body)))]
    (reduce separate-code {} body)))

(defmulti to-ruby-form #(and (list? %) (first %)))

(defn- separate-superclass [forms]
  (let [[head & rest] forms]
    (if (= (list? head) (= (first head) 'defn))
      ['clojuby.core/ruby-object forms]
      [head rest])))

(defmethod to-ruby-form 'defclass [[_ name & rest]]
  (let [[sub rest] (separate-superclass rest)]
    `(let [body# ~(as-class-body rest)
           class# (clojuby.core/new-class* ~sub body#)]
       (def ~name class#)
       (clojuby.core/public-send "const_set" clojuby.core/ruby-object
                                 ~(keyword name) class#)
       class#)))

(defmethod to-ruby-form 'fn [[_ & form]]
  `(with-meta (fn [~'self]
                (with-meta (fn ~@form) {:dont-convert? true}))
     {:binding true}))

(defmethod to-ruby-form :default [form]
  form)

(def ^:private replace-forms {'new 'clojuby.core/new})

(defn sugarify [forms]
  (let [to-replace (->> forms flatten
                        (filter #(and (symbol? %) (str/starts-with? % ".")))
                        (map (fn [name] [name (gensym (normalize-method name))]))
                        (into {}))
        lets (->> to-replace
                  (mapcat (fn [[sym name]]
                            [name `(partial ~'clojuby.core/public-send
                                            ~(normalize-method sym))])))
        replace (merge replace-forms to-replace)]
    `(let ~(vec lets)
       ~@(->> forms
              (walk/postwalk-replace replace)
              (walk/prewalk to-ruby-form)))))
