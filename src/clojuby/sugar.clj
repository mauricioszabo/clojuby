(ns clojuby.sugar
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn- clj-or-rb [symbol]
  (cond
    (and (seq? symbol) (-> symbol first (= 'quote)))
    `(clojuby.core/raw-eval (str (quote ~(second symbol))))

    (symbol? symbol)
    symbol))

(defn- normalize-method [name]
  (str/replace name #"\-" "_"))

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

(defmulti to-ruby-form first)

(defmethod to-ruby-form 'new [[_ class & params]]
  `(clojuby.core/new ~class ~@params))

(defmethod to-ruby-form 'defclass [[_ name & rest]]
  (let [[sub rest] (if-let [sub (-> rest first clj-or-rb)]
                     [sub (drop 1 rest)]
                     ['clojuby.core/ruby-object rest])]
    `(let [body# ~(as-class-body rest)
           class# (clojuby.core/new-class* ~sub body#)]
       (def ~name class#)
       class#)))

(defmethod to-ruby-form :default [forms]
  (if (-> forms first (str/starts-with? "."))
    (let [[first obj & rest] forms]
      (->> rest
           (cons (or (clj-or-rb obj) obj))
           (cons (-> first (str/replace #"^\." "") normalize-method))
           (cons 'clojuby.core/public-send)))
    forms))
