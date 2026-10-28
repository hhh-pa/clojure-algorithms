(ns clojure-algorithms.maths.find-max
  "Port of maths/find_max.py")

(defn find-max-iterative
  "Find the maximum value in a list by scanning it once."
  [nums]
  (when (empty? nums)
    (throw (ex-info "find_max_iterative() arg is an empty sequence"
                    {:type :ValueError})))
  (reduce (fn [m x] (if (> x m) x m)) (first nums) (rest nums)))

(defn- resolve-index
  "Resolve a possibly negative index against a list of length n,
matching Python's negative indexing."
  [nums n i]
  (nth nums (if (neg? i) (+ i n) i)))

(defn find-max-recursive
  "Find the maximum value in a list using a Divide and Conquer approach.
Searches the subarray nums[left..right] (both inclusive); negative indices
count from the end as in Python."
  [nums left right]
  (let [n (count nums)]
    (when (zero? n)
      (throw (ex-info "find_max_recursive() arg is an empty sequence"
                      {:type :ValueError})))
    (when (or (>= left n) (< left (- n)) (>= right n) (< right (- n)))
      (throw (ex-info "list index out of range" {:type :IndexError})))
    (if (= left right)
      (resolve-index nums n left)
      (let [mid (bit-shift-right (+ left right) 1)
            left-max (find-max-recursive nums left mid)
            right-max (find-max-recursive nums (inc mid) right)]
        (if (>= left-max right-max) left-max right-max)))))
