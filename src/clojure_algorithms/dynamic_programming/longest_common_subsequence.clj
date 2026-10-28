(ns clojure-algorithms.dynamic-programming.longest-common-subsequence
  "Longest common subsequence: returns [length subsequence].
  Ported from TheAlgorithms/Python
  dynamic_programming/longest_common_subsequence.py.")

(defn longest-common-subsequence
  "Return [length subsequence] of the LCS of strings `x` and `y`."
  [x y]
  (assert (some? x))
  (assert (some? y))
  (let [m (count x)
        n (count y)
        dp (atom (vec (repeat (inc m) (vec (repeat (inc n) 0)))))]
    (doseq [i (range 1 (inc m))
            j (range 1 (inc n))]
      (let [match (if (= (nth x (dec i)) (nth y (dec j))) 1 0)]
        (swap! dp assoc-in [i j]
               (max (get-in @dp [(dec i) j])
                    (get-in @dp [i (dec j)])
                    (+ (get-in @dp [(dec i) (dec j)]) match)))))
    (loop [i m j n result ""]
      (if (or (zero? i) (zero? j))
        [(get-in @dp [m n]) result]
        (let [match (if (= (nth x (dec i)) (nth y (dec j))) 1 0)]
          (cond
            (= (get-in @dp [i j]) (+ (get-in @dp [(dec i) (dec j)]) match))
            (recur (dec i) (dec j) (if (= match 1)
                                     (str (nth x (dec i)) result)
                                     result))
            (= (get-in @dp [i j]) (get-in @dp [(dec i) j]))
            (recur (dec i) j result)
            :else (recur i (dec j) result)))))))
