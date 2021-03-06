# Clojuby

Because you can rubyfy Clojure, but cannot clojurify Ruby...

... or whatever that means

## Usage

Let's say we have ActiveRecord and Sinatra gems installed:

```clojure
(ns sinatra-ar (:require [clojuby.core :refer [rb rb-require ruby] :as rb]))

(rb-require "activerecord-jdbcsqlite3-adapter")
(rb-require "sinatra")
(ruby
 (doto (rb ActiveRecord.Base)
       (.establish-connection {:adapter :sqlite3
                               :database ":memory:"}))

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
           (.create! {:name "Foo" :age 42 :likes "Ruby"})
           (.create! {:name "Bar" :age 22 :likes "Clojure"})
           (.create! {:name "Baz" :age 20 :likes "Java"}))))
 (.migrate CreateUsers :up))

(ruby
 (doto (defclass App (rb Sinatra.Base))
       (.get "/" (fn []
                   (.erb self "
<html><body>
<% User.all.each do |user| %>
  <p>
    <b><%= user.name %></b>
    <%= user.age %>
    <a href='<%= user.id %>'>Edit</a>
<% end %>
</body></html>
")))
;; AND SO ON...
       (.run!)))

```

A full example is on `examples/sinatra_ar.clj`.

## License

Copyright © 2017 Maurício Szabo

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
