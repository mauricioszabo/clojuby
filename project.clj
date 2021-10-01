(defproject clojuby "0.1.0"
  :description "Calling Ruby from Clojure, with Clojure structures"
  :url "https://github.com/mauricioszabo/clojuby"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.jruby/jruby "9.2.19.0"]]
  :profiles {:dev {:src-paths ["dev"]
                   :dependencies [[check "0.2.1-SNAPSHOT"]]}})
                   ; :plugins [[lein-midje "3.2.1"]]}})
