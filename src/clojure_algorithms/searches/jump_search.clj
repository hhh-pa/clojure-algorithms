(ns clojure-algorithms.searches.jump-search
  "Jump search over sorted collections.

  Port of TheAlgorithms/Python searches/jump_search.py: leap ahead in
  sqrt(n) blocks, then scan linearly inside the matching block.")

(defn jump-search
  "Index of `item` in sorted `arr`, or -1 if absent."
  [arr item]
  (let [a (vec arr)
        n (count a)]
    (if (zero? n)
      -1
      (let [block (int (Math/sqrt n))]
        (loop [prev 0 step block]
          (if (neg? (compare (nth a (dec (min step n))) item))
            (let [prev step step (+ step block)]
              (if (>= prev n)
                -1
                (recur prev step)))
            (loop [prev prev]
              (if (neg? (compare (nth a prev) item))
                (let [prev (inc prev)]
                  (if (= prev (min step n))
                    -1
                    (recur prev)))
                (if (= (nth a prev) item) prev -1)))))))))
