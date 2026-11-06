(ns scad-clj-workflow.extra.calc
  (:require [clojure.string :as string]))

(defn resolve-token-infix [s-expr]
  (cond
    (number? s-expr)
    s-expr

    (list? s-expr)
    (str "(" (string/join " " (map resolve-token-infix s-expr)) ")")

    (symbol? s-expr)
    (let [name (name s-expr)]
      (if (string/ends-with? name "#")
        (if-let [value (some-> name (string/replace  #"#$" "") symbol resolve deref)]
          value
          (throw (ex-info (str "Cannot resolve symbol: " (-> name (string/replace  #"#$" ""))) {})))
        name))))

(defmacro calc-infix
  "Generates math expression mixing Clojure symbols, numbers, operators, and OpenSCAD symbols.

   For example: ` (calc-infix my-radius# + wall-thickness# + (5 * $t))`

   Symbols that should be evaluated must be tagged with trailing '#'"
  [& body]
  (let [tokens (map resolve-token-infix body)]
    `(list ~@tokens)))

(defn resolve-token [s-expr]
  (cond
    (number? s-expr)
    s-expr

    (list? s-expr)
    (let [[op & operands] s-expr
          single? (= 1 (count operands))
          operands (cond
                     (and (= op '-)
                          single?) (concat [0] operands)
                     (and (= op '/)
                          single?) (concat [1] operands)
                     :else
                     operands)]
      (str "("
           (string/join (str " " op " ")
                        (map resolve-token operands))
           ")"))

    (symbol? s-expr)
    (let [name (name s-expr)]
      (if (string/ends-with? name "#")
        (if-let [value (some-> name (string/replace  #"#$" "") symbol resolve deref)]
          value
          (throw (ex-info (str "Cannot resolve symbol: " (-> name (string/replace  #"#$" ""))) {})))
        name))))

(defmacro calc
  "Generates math expression mixing Clojure symbols, numbers, operators, and OpenSCAD symbols.

   For example: `(calc (+ my-radius# wall-thickness# (* 5 $t))`

   Symbols that should be evaluated must be tagged with trailing '#'

   Note: May not handle special interpretation of your favorite operand with a single parameter,
   though it does know - and /."
  [& body]
  (let [tokens (map resolve-token body)]
    `(list ~@tokens)))

(comment
  (def dome-radius 5)
  (def wall-thickness 1)
  (calc-infix dome-radius# + wall-thickness# + (55 * $t))
  (calc (+ dome-radius#))
  (calc (+ dome-radius# wall-thickness# (* 55 $t dome-radius#)))
  (calc (/ (- wall-thickness#)))
  )
