(ns clojure-algorithms.data-structures.stacks.infix-to-postfix-conversion
  "Port of data_structures/stacks/infix_to_postfix_conversion.py.
  Shunting-yard infix to postfix."
  (:require [clojure-algorithms.data-structures.stacks.balanced-parentheses
             :refer [balanced-parentheses]]))

(def precedences {\+ 1 \- 1 \* 2 \/ 2 \^ 3})

(def associativities {\+ "LR" \- "LR" \* "LR" \/ "LR" \^ "RL"})

(defn precedence
  "Precedence of operator CHAR (-1 when unknown)."
  [char]
  (get precedences char -1))

(defn associativity
  "Associativity (\"LR\"/\"RL\") of operator CHAR."
  [char]
  (get associativities char))

(defn infix-to-postfix
  "Convert infix EXPRESSION-STR to space-separated postfix."
  [expression-str]
  (when-not (balanced-parentheses expression-str)
    (throw (ex-info "Mismatched parentheses" {:type :value-error})))
  (loop [chars (seq expression-str) stack [] postfix []]
    (if (empty? chars)
      (clojure.string/join " " (into postfix (reverse stack)))
      (let [ch (first chars)
            rest-chars (rest chars)]
        (cond
          (or (Character/isLetter ^char ch) (Character/isDigit ^char ch))
          (recur rest-chars stack (conj postfix ch))
          (= ch \()
          (recur rest-chars (conj stack ch) postfix)
          (= ch \))
          (let [[stack postfix]
                (loop [stack stack postfix postfix]
                  (if (= (peek stack) \()
                    [(pop stack) postfix]
                    (recur (pop stack) (conj postfix (peek stack)))))]
            (recur rest-chars stack postfix))
          :else
          (let [[stack postfix]
                (loop [stack stack postfix postfix]
                  (if (empty? stack)
                    [(conj stack ch) postfix]
                    (let [cp (precedence ch)
                          tp (precedence (peek stack))]
                      (cond
                        (> cp tp) [(conj stack ch) postfix]
                        (< cp tp) (recur (pop stack) (conj postfix (peek stack)))
                        (= (associativity ch) "RL") [(conj stack ch) postfix]
                        :else (recur (pop stack)
                                     (conj postfix (peek stack)))))))]
            (recur rest-chars stack postfix)))))))
