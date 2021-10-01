(ns clojuby.ast
  (:refer-clojure :exclude [eval])
  (:require [clojure.string :as str]
            [clojuby.core :as rb])
  (:import [org.jruby Ruby]
           [org.jruby.runtime.builtin IRubyObject]
           [org.jruby.util ByteList]
           [org.jruby.ast StrNode FixnumNode FloatNode StrNode SymbolNode]))

(def runtime (Ruby/getGlobalRuntime))
(def context (.getCurrentContext runtime))

(defrecord SimplePos [file line]
  org.jruby.lexer.yacc.ISourcePosition
  (^String getFile [_] file)
  (getLine [_] line))

(def pos (memoize ->SimplePos))

(defprotocol AST
  (ast-for [this row]))

(extend-protocol AST
  Long
  (ast-for [this row] `(FixnumNode. (pos ~*file* ~row) ~this))

  Double
  (ast-for [this row] `(FloatNode. (pos ~*file* ~row) ~this))

  String
  (ast-for [this row] `(StrNode. (pos ~*file* ~row)
                                 (ByteList. (.getBytes ~this))))

  clojure.lang.Keyword
  (ast-for [this row] `(SymbolNode. (pos ~*file* ~row)
                                    (rb/clj->rb ~this))))

(defmacro eval [body]
  (let [{:keys [line]} (meta body)
        ast (ast-for body (or line 0))
        root `(org.jruby.ast.RootNode. (.getPosition ~ast)
                                       (.getCurrentScope context)
                                       ~ast
                                       *file*)]
    (prn :AST root)
    `(rb/rb->clj (.runNormally runtime ~root))))
