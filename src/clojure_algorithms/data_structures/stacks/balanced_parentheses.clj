(ns clojure-algorithms.data-structures.stacks.balanced-parentheses
  "Port of data_structures/stacks/balanced_parentheses.py.")

(def pairs {\( \) \[ \] \{ \}})

(defn balanced-parentheses
  "True when every opener in EXPRESSION is properly closed."
  [expression]
  (loop [chars (seq expression) stack []]
    (if (empty? chars)
      (empty? stack)
      (let [ch (first chars)]
        (cond
          (contains? pairs ch) (recur (rest chars) (conj stack ch))
          (some #{ch} (vals pairs))
          (if (and (seq stack) (= ch (pairs (peek stack))))
            (recur (rest chars) (pop stack))
            false)
          :else (recur (rest chars) stack))))))
