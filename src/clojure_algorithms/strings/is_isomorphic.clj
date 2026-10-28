(ns clojure-algorithms.strings.is-isomorphic
  "Isomorphic strings check. Ported from TheAlgorithms/Python.")

(defn is-isomorphic
  "True when characters of `s` can be consistently remapped to get `t`."
  [s t]
  (if (not= (count s) (count t))
    false
    (loop [pairs (map vector s t) mapping {} mapped #{}]
      (if (empty? pairs)
        true
        (let [[cs ct] (first pairs)]
          (cond
            (contains? mapping cs)
            (if (= (get mapping cs) ct)
              (recur (rest pairs) mapping mapped)
              false)

            (contains? mapped ct) false

            :else
            (recur (rest pairs) (assoc mapping cs ct) (conj mapped ct))))))))
