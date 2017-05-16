(ns clojuby.sugar
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- clj-or-rb [symbol]
  (cond
    (and (seq? symbol) (-> symbol first (= 'quote)))
    `(clojuby.core/raw-eval (str/replace (quote ~(second symbol)) #"\." "::"))

    (symbol? symbol)
    symbol))

(defn- normalize-method [name]
  (-> name
      (str/replace-first #"^\." "")
      (str/replace #"\-" "_")))

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

(defmethod to-ruby-form 'defclass [[_ name & rest]]
  (let [[sub rest] (if-let [sub (-> rest first clj-or-rb)]
                     [sub (drop 1 rest)]
                     ['clojuby.core/ruby-object rest])]
    `(let [body# ~(as-class-body rest)
           class# (clojuby.core/new-class* ~sub body#)]
       (def ~name class#)
       class#)))

(defmethod to-ruby-form 'fn [[_ & form]]
  `(let [~'-binding (atom nil)]
     (with-meta (fn ~@form) {:binding ~'-binding})))

(defmethod to-ruby-form :default [form]
  form)

(defn to-ruby-sym [sym]
  (cond
    (= sym 'new) 'clojuby.core/new

    (and (symbol? sym) (str/starts-with? sym "."))
    `(partial ~'clojuby.core/public-send
              ~(normalize-method sym))

    (and (symbol? sym) (str/starts-with? sym "-binding."))
    (list 'clojuby.core/public-send
          (normalize-method (str/replace-first sym "-binding." ""))
          '@-binding)

    :else sym))
