(ns sinatra
  (:require [clojuby.core :refer [ruby] :as rb]))

(rb/rb-require "sinatra")
; (def App
;   (rb/eval "class App < Sinatra::Base;end;App"))

(ruby
 (defclass App 'Sinatra.Base))

(ruby
 (doto App
       (.get "/" (fn [] "Hello, world!"))
       (.get "/params" (fn [ & params] (str "Hello with params: " App)))
       (.run!)))

(clojure.walk/macroexpand-all '
               (ruby
                (doto App
                      (.get "/" (fn [] "Hello, world!"))
                      (.get "/params" (fn [ & params] (str "Hello with params: " App)))
                      (.run!))))

(macroexpand-1 '
               (ruby
                (doto App
                      (.get "/" (fn [] "Hello, world!")))))


(macroexpand-1 '
               (ruby
                (clojure.core/let
                 [G__37397 App]
                 (.get G__37397 "/" (fn [] "Hello, world!"))
                 G__37397)))
