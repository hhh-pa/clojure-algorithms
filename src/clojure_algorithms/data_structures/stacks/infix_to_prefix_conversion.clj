(ns clojure-algorithms.data-structures.stacks.infix-to-prefix-conversion
  "Port of data_structures/stacks/infix_to_prefix_conversion.py.
  The Python version prints a trace table; this port returns the
  converted strings purely (same conversion semantics).")

(def priority {\^ 3 \* 2 \/ 2 \% 2 \+ 1 \- 1})

(defn infix-2-postfix
  "Convert infix string to postfix (operands pass through, so unary
  minus works positionally)."
  [infix]
  (let [[stack postfix]
        (reduce (fn [[stack postfix] x]
                  (cond
                    (or (Character/isLetter ^char x)
                        (Character/isDigit ^char x))
                    [stack (conj postfix x)]
                    (= x \() [(conj stack x) postfix]
                    (= x \))
                    (do
                      (when (empty? stack)
                        (throw (ex-info "list index out of range"
                                        {:type :index-error})))
                      (loop [stack stack postfix postfix]
                        (if (= (peek stack) \()
                          [(pop stack) postfix]
                          (recur (pop stack) (conj postfix (peek stack))))))
                    (empty? stack) [(conj stack x) postfix]
                    :else (let [[stack postfix]
                                (loop [stack stack postfix postfix]
                                  (if (and (seq stack)
                                           (not= (peek stack) \()
                                           (<= (priority x)
                                               (priority (peek stack))))
                                    (recur (pop stack)
                                           (conj postfix (peek stack)))
                                    [stack postfix]))]
                            [(conj stack x) postfix])))
                [[] []] infix)]
    (loop [stack stack postfix postfix]
      (if (empty? stack)
        (apply str postfix)
        (do
          (when (= (peek stack) \()
            (throw (ex-info "invalid expression" {:type :value-error})))
          (recur (pop stack) (conj postfix (peek stack))))))))

(defn infix-2-prefix
  "Convert infix string to prefix via reversed postfix."
  [infix]
  (let [reversed-swapped
        (mapv (fn [ch] (cond (= ch \() \) (= ch \)) \( :else ch))
              (reverse infix))]
    (apply str (reverse (infix-2-postfix (apply str reversed-swapped))))))
