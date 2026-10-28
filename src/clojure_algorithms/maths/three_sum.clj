(ns clojure-algorithms.maths.three-sum
  "https://en.wikipedia.org/wiki/3SUM")

(defn- triplets-from
  "Finds unique triplets starting at index i that sum to zero,
  using two pointers over the sorted vector nums."
  [nums i]
  (let [n (count nums)
        target (- (nums i))]
    (loop [low (inc i) high (dec n) acc []]
      (if (>= low high)
        acc
        (let [s (+ (nums low) (nums high))]
          (cond (= s target)
                (let [low (loop [low low]
                            (if (and (< low high)
                                     (= (nums low) (nums (inc low))))
                              (recur (inc low))
                              low))
                      high (loop [high high]
                             (if (and (< low high)
                                      (= (nums high) (nums (dec high))))
                               (recur (dec high))
                               high))]
                  (recur (inc low) (dec high)
                         (conj acc [(nums i) (nums low) (nums high)])))
                (< s target) (recur (inc low) high acc)
                :else (recur low (dec high) acc)))))))

(defn three-sum
  "Finds all unique triplets in a sorted array of integers that sum up to zero.

  Args:
      nums: A sorted list of integers.

  Returns:
      A list of lists containing unique triplets that sum up to zero."
  [nums]
  (let [nums (vec (sort nums))
        n (count nums)]
    (loop [i 0 ans []]
      (if (> i (- n 3))
        ans
        (recur (inc i)
               (if (and (pos? i) (= (nums i) (nums (dec i))))
                 ans
                 (into ans (triplets-from nums i))))))))
