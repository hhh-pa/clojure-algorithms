(ns clojure-algorithms.searches.simple-binary-search
  "Boolean binary search over sorted collections.

  Port of TheAlgorithms/Python searches/simple_binary_search.py.")

(defn binary-search
  "True when `item` occurs in ascending `a-list`, false otherwise."
  [a-list item]
  (let [v (vec a-list)]
    (cond (empty? v) false
          :else (let [mid (quot (count v) 2)
                      c (compare (nth v mid) item)]
                  (cond (zero? c) true
                        (pos? c) (recur (subvec v 0 mid) item)
                        :else (recur (subvec v (inc mid)) item))))))
