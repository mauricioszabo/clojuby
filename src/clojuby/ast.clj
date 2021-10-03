(ns clojuby.ast
  (:refer-clojure :exclude [eval])
  (:require [clojure.string :as str]
            [clojuby.core :as rb])
  (:import [org.jruby Ruby]
           [org.jruby.runtime.builtin IRubyObject]
           [org.jruby.util ByteList KeyValuePair]
           [org.jruby.ast StrNode FixnumNode FloatNode StrNode SymbolNode
            TrueNode FalseNode NilNode
            HashNode ArrayNode CallNode ConstNode]))

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
  (ast-for [this row] `(FixnumNode. ~row ~this))

  Double
  (ast-for [this row] `(FloatNode. ~row ~this))

  String
  (ast-for [this row] `(StrNode. ~row
                                 (ByteList. (.getBytes ~this))))

  Boolean
  (ast-for [this row] `(~(if this `TrueNode. `FalseNode.) ~row))

  nil
  (ast-for [_ row] `(NilNode. ~row))

  clojure.lang.Keyword
  (ast-for [this row] `(SymbolNode. ~row
                                    (rb/clj->rb ~this)))

  clojure.lang.IPersistentMap
  (ast-for [this row]
    (let [hash-sym (gensym "hashmap-")]
      `(let [~hash-sym (HashNode. ~row)]
         ~@(for [[k v] this
                 :let [k-ast (ast-for k row)
                       v-ast (ast-for v row)]]
             `(.add ~hash-sym (KeyValuePair. ~k-ast ~v-ast)))
         ~hash-sym)))

  clojure.lang.IPersistentVector
  (ast-for [this row]
    (let [hash-sym (gensym "hashmap-")]
      `(let [~hash-sym (ArrayNode. ~row)]
         ~@(for [val this
                 :let [val-ast (ast-for val row)]]
             `(.add ~hash-sym ~val-ast))
         ~hash-sym)))

  clojure.lang.IPersistentSet
  (ast-for [this row]
           (let [args (ast-for (vec this) row)]
             `(CallNode. ~row
                         (ConstNode. ~row (.fastNewSymbol runtime  "Set"))
                         (.fastNewSymbol runtime "[]")
                         ~args
                         nil
                         false))))

(defmacro eval [body]
  (let [{:keys [line]} (meta body)
        ast (ast-for body (or line 0))
        root `(org.jruby.ast.RootNode. (.getLine ~ast)
                                       (.getCurrentScope context)
                                       ~ast
                                       *file*)]
    ; (prn :AST root)
    `(rb/rb->clj (.runInterpreter runtime ~root))))
