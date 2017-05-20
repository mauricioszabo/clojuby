(defproject clojuby "0.1.0"
  :description "Calling Ruby from Clojure, with Clojure structures"
  :url "https://github.com/mauricioszabo/clojuby"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.jruby/jruby "9.1.8.0"]]
  :profiles {:dev {:src-paths ["dev"]
                   :dependencies [[midje "1.8.3"]]
                   :plugins [[lein-midje "3.2.1"]]}})
