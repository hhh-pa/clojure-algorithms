(ns clojure-algorithms.maths.trailing-zeroes
  "Count trailing zeroes of an integer.")

(defn trailing-zeroes
  "Number of zeroes at the end of num (-1 for negatives, 1 for zero)."
  [num]
  (cond (neg? num) -1
        (zero? num) 1
        :else (loop [n num
                     ans 0]
                (if (or (<= n 0) (not (zero? (mod n 10))))
                  ans
                  (recur (quot n 10) (inc ans))))))
