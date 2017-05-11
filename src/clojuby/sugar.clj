(ns clojuby.sugar
  (:require [clojure.string :as str]))

(defmulti to-ruby-form first)

(defmethod to-ruby-form :default [[first & forms]]
  (if (str/starts-with? first ".")
    (->> forms
         (cons (str/replace first #"^\." ""))
         (cons 'clojuby.core/public-send))
    (cons first forms)))
