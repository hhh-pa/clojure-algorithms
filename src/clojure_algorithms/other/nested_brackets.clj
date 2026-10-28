(ns clojure-algorithms.other.nested-brackets
  "Balanced-brackets check.

  Port of TheAlgorithms/Python `other/nested_brackets.py`.
  Non-bracket characters are ignored, as in the original.")

(def ^:private open-to-closed {\{ \} \[ \] \( \)})

(def ^:private closed-set (set (vals open-to-closed)))

(defn balanced?
  "True when every bracket in string `s` is properly nested."
  [s]
  (loop [chars (seq s) stack []]
    (if (nil? chars)
      (empty? stack)
      (let [ch (first chars)]
        (cond
          (contains? open-to-closed ch)
          (recur (next chars) (conj stack ch))
          (contains? closed-set ch)
          (if (and (seq stack) (= (open-to-closed (peek stack)) ch))
            (recur (next chars) (pop stack))
            false)
          :else
          (recur (next chars) stack))))))
