(ns clojure-algorithms.data-structures.stacks.prefix-evaluation
  "Port of data_structures/stacks/prefix_evaluation.py.
  evaluate works on space-separated strings; evaluate-recursive
  consumes a token vector and returns [value remaining].")

(def operators
  {"+" + "-" - "*" * "/" (fn [x y] (/ (double x) (double y)))})

(defn is-operand
  "True when C is all digits."
  [c]
  (boolean (re-matches #"\d+" (str c))))

(defn evaluate
  "Evaluate prefix EXPRESSION string."
  [expression]
  (let [stack
        (reduce (fn [stack c]
                  (if (is-operand c)
                    (conj stack (Long/parseLong c))
                    (let [o1 (peek stack)
                          stack (pop stack)
                          o2 (peek stack)
                          stack (pop stack)]
                      (conj stack ((get operators c) o1 o2)))))
                [] (reverse (clojure.string/split expression #"\s+")))]
    (peek stack)))

(defn evaluate-recursive
  "Evaluate prefix TOKENS; returns [value remaining-tokens]."
  [tokens]
  (let [op (first tokens)
        rest-tokens (vec (rest tokens))]
    (if (is-operand op)
      [(Long/parseLong (str op)) rest-tokens]
      (let [[a rest-tokens] (evaluate-recursive rest-tokens)
            [b rest-tokens] (evaluate-recursive rest-tokens)]
        [((get operators (str op)) a b) rest-tokens]))))
