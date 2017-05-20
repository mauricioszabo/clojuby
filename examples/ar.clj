(ns ar (:require [clojuby.core :refer [rb rb-require ruby] :as rb]))

(rb/rb-require "activerecord-jdbcsqlite3-adapter")

(ruby
 (doto (rb ActiveRecord.Base)
       (.establish-connection {:adapter :sqlite3
                               :database ":memory:"})))

(ruby
 (doto (defclass User (rb ActiveRecord.Base)
         (defn self.greater [age] (.where self "users.age > ?" age)))
       (.validates-presence-of :name))

 (defclass CreateUsers (rb ActiveRecord.Migration)
   (defn change []
     (.create-table self :users (fn [t]
                                  (doto t
                                        (.string :name)
                                        (.integer :age)
                                        (.string :likes)
                                        (.timestamps))))
     (doto User
           (.create! {:name "Timbó" :age 42 :likes "Ruby"})
           (.create! {:name "Szabo" :age 22 :likes "Clojure"})
           (.create! {:name "Rodolfo" :age 20 :likes "Java"}))))
 (.migrate CreateUsers :up))


(ruby
 (-> User (.greater 21)))

(ruby
 (-> User (.create {:age 22}) .errors .full-messages))
