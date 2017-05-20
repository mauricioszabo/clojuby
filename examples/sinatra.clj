(ns sinatra (:require [clojuby.core :refer [ruby] :as rb]))

(rb/rb-require "sinatra")
(ruby
 (doto (defclass App 'Sinatra.Base)
       (.get "/" (fn [] "Hello, world!"))
       (.get "/params" (fn [ & params]
                         (str "Hello, " (-> self .params (get "name")))))

       (.run!)))
