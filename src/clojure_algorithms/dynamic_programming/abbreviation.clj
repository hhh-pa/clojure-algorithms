(ns clojure-algorithms.dynamic-programming.abbreviation
  "Can string `a` become `b` by capitalizing some lowercase letters and
  deleting the remaining lowercase letters?
  Ported from TheAlgorithms/Python dynamic_programming/abbreviation.py.")

(defn- lower-case?
  "True for lowercase ASCII letters."
  [ch]
  (Character/isLowerCase ^char ch))

(defn abbr
  "True when `a` can be abbreviated to `b`."
  [a b]
  (let [n (count a)
        m (count b)]
    (loop [i 0 dp (assoc (vec (repeat (inc n) (vec (repeat (inc m) false))))
                         0 (assoc (vec (repeat (inc m) false)) 0 true))]
      (if (>= i n)
        (get-in dp [n m])
        (recur (inc i)
               (reduce (fn [d j]
                         (if-not (get-in d [i j])
                           d
                           (cond-> d
                             (and (< j m)
                                  (= (Character/toUpperCase ^char (nth a i))
                                     (nth b j)))
                             (assoc-in [(inc i) (inc j)] true)

                             (lower-case? (nth a i))
                             (assoc-in [(inc i) j] true))))
                       dp
                       (range (inc m))))))))
