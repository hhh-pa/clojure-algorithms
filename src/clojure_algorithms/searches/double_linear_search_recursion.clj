(ns clojure-algorithms.searches.double-linear-search-recursion
  "Recursive linear search narrowing in from both ends.

  Port of TheAlgorithms/Python searches/double_linear_search_recursion.py.
  Like the Python source, an explicit right bound of 0 means the last
  index (`right or len - 1`).")

(defrecord Person [first-name])

(defn search
  "Index of `key` in `list-data` between `left` and `right`, or -1.

  With two arguments the whole collection is searched."
  ([list-data key] (search list-data key 0 0))
  ([list-data key left right]
   (let [v (vec list-data)
         right (if (zero? right) (dec (count v)) right)]
     (cond (> left right) -1
           (= (nth v left) key) left
           (= (nth v right) key) right
           :else (recur v key (inc left) (dec right))))))
