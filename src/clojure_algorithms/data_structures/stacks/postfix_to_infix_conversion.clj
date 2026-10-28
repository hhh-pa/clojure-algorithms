(ns clojure-algorithms.data-structures.stacks.postfix-to-infix-conversion
  "Port of data_structures/stacks/postfix_to_infix_conversion.py.")

(def operator-set #{\+ \- \* \/ \^})

(defn postfix-to-infix
  "Convert a no-whitespace postfix string to parenthesized infix."
  [postfix-expression]
  (when (or (nil? postfix-expression) (empty? postfix-expression))
    (throw (ex-info "Invalid postfix expression." {:type :value-error})))
  (let [stack
        (reduce (fn [stack item]
                  (if-not (contains? operator-set item)
                    (conj stack (str item))
                    (let [operand-2 (peek stack)
                          stack (pop stack)
                          operand-1 (peek stack)
                          stack (pop stack)]
                      (conj stack (str "(" operand-1 item operand-2 ")")))))
                [] postfix-expression)]
    (peek stack)))
