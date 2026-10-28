(ns clojure-algorithms.maths.find-min
  "Find the minimum number in a list.")

(defn find-min-iterative
  "Find minimum number in a list."
  [nums]
  (when (empty? nums)
    (throw (ex-info "find_min_iterative() arg is an empty sequence"
                    {:type :ValueError})))
  (reduce min nums))

(defn find-min-recursive
  "Find min value in nums between indices left and right (inclusive,
  Python-style negative indices allowed) via divide and conquer."
  [nums left right]
  (when (empty? nums)
    (throw (ex-info "find_min_recursive() arg is an empty sequence"
                    {:type :ValueError})))
  (let [n (count nums)]
    (when (or (>= left n) (< left (- n)) (>= right n) (< right (- n)))
      (throw (ex-info "list index out of range" {:type :IndexError})))
    (if (= left right)
      (nth nums (mod left n))
      (let [mid (bit-shift-right (+ left right) 1)
            left-min (find-min-recursive nums left mid)
            right-min (find-min-recursive nums (inc mid) right)]
        (if (<= left-min right-min) left-min right-min)))))
