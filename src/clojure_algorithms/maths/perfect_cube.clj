(ns clojure-algorithms.maths.perfect-cube
  "Port of maths/perfect_cube.py")

(defn perfect-cube
  "Returns true if `n` is a perfect cube, using floating-point arithmetic."
  [n]
  (let [val (Math/pow n (/ 1.0 3))]
    (== (* val val val) n)))

(defn perfect-cube-binary-search
  "Returns true if the integer `n` is a perfect cube, using binary search.
  Time complexity: O(log(n)). Space complexity: O(1)."
  [n]
  (when-not (integer? n)
    (throw (ex-info "perfect_cube_binary_search() only accepts integers"
                    {:type :TypeError})))
  (let [n (if (neg? n) (- n) n)]
    (loop [lo 0, hi n]
      (if (> lo hi)
        false
        (let [mid (+ lo (quot (- hi lo) 2))
              cube (* mid mid mid)]
          (cond
            (== cube n) true
            (< cube n) (recur (inc mid) hi)
            :else (recur lo (dec mid))))))))
