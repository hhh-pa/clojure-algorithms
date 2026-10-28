(ns clojure-algorithms.project-euler.problem-049.sol1
  "Euler Problem 49: find the 12-digit number formed by concatenating the three
  terms of the other 4-digit increasing prime permutation arithmetic sequence
  besides 1487, 4817, 8147.")

(defn prime?
  "Checks whether number is prime in O(sqrt(n)). A number is prime if it has
  exactly two factors: 1 and itself."
  [number]
  (cond
    (and (> number 1) (< number 4)) true
    (or (< number 2) (zero? (mod number 2)) (zero? (mod number 3))) false
    :else (let [limit (inc (int (Math/sqrt number)))]
            (loop [i 5]
              (cond
                (>= i limit) true
                (or (zero? (mod number i)) (zero? (mod number (+ i 2)))) false
                :else (recur (+ i 6)))))))

(defn search
  "Returns true if target is in the sorted prime-list, using binary search."
  [target prime-list]
  (let [v (vec prime-list)]
    (loop [left 0 right (dec (count v))]
      (if (> left right)
        false
        (let [middle (quot (+ left right) 2)
              mid-val (v middle)]
          (cond
            (= mid-val target) true
            (< mid-val target) (recur (inc middle) right)
            :else (recur left (dec middle))))))))

(defn- permutations
  "Returns a lazy sequence of all permutations of the string s as strings."
  [s]
  (let [v (vec s)]
    (if (<= (count v) 1)
      (list (apply str v))
      (for [i (range (count v))
            rest-p (permutations (apply str (into (subvec v 0 i)
                                                  (subvec v (inc i)))))]
        (str (v i) rest-p)))))

(defn- find-triple
  "Returns the first sorted 3-term arithmetic progression in the sorted
  candidate vector, or nil."
  [candidate]
  (let [length (count candidate)]
    (some identity
          (for [i (range length)
                j (range (inc i) length)
                k (range (inc j) length)
                :let [a (candidate i) b (candidate j) c (candidate k)]
                :when (and (= (abs (- a b)) (abs (- b c)))
                           (= 3 (count #{a b c})))]
            (sort [a b c])))))

(defn solution
  "Returns the 12-digit concatenation of the three terms of the qualifying
  prime permutation sequence (other than the 1487 example)."
  []
  (let [prime-list (vec (filter prime? (range 1001 10000 2)))
        candidates (for [number prime-list
                         :let [tmp (sort (for [member (permutations (str number))
                                               :let [p (Integer/parseInt member)]
                                               :when (and (odd? p) (search p prime-list))]
                                           p))]
                         :when (>= (count tmp) 3)]
                     (vec tmp))
        answer (into #{} (comp (map find-triple)
                               (filter some?)
                               (map (fn [seq] (apply str seq))))
                     candidates)]
    (apply max (map #(Long/parseLong %) answer))))
