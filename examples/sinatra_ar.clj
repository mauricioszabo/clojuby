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

(defn edit [self user]
  (ruby
   (.erb self "
<html><body>
<% if user.errors %>
  <%= user.errors.full_messages.join('<br />') %>
  <br /><hr />
  <br />
<% end %>
<form method='POST' action='<%= user.id %>'
<label>
  Name:
  <input type='string' name='user[name]' value='<%= user.name %>'>
</label>
<br /> <br />
<label>
  Age:
  <input type='string' name='user[age]' value='<%= user.age %>'>
</label>
<br /> <br />
<label>
  Likes:
  <input type='string' name='user[likes]' value='<%= user.likes %>'>
</label>
<br /> <br />

<input type='submit' value='Save'>
</form>
</body></html>
"
         {:locals {:user user}})))

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
       (.get "/:id" (fn [id]
                      (println self)
                      (edit self (.find User id))))

       (.post "/:id" (fn [id]
                       (let [user (.find User id)]
                         (if (.update-attributes user (-> self .params (get "user")))
                           (.redirect self "/")
                           (edit self user)))))

       (.run!)))
