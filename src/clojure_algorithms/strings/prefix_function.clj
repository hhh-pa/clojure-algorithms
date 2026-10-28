(ns clojure-algorithms.strings.prefix-function
  "KMP prefix function (longest prefix which is also a suffix).
  See https://cp-algorithms.com/string/prefix-function.html
  Ported from TheAlgorithms/Python.")

(defn prefix-function
  "Prefix-function array for `s`."
  [s]
  (let [n (count s)]
    (loop [i 1 pi (vec (repeat n 0))]
      (if (>= i n)
        pi
        (let [j (loop [j (nth pi (dec i))]
                  (if (and (pos? j) (not= (nth s i) (nth s j)))
                    (recur (nth pi (dec j)))
                    j))
              j2 (if (= (nth s i) (nth s j)) (inc j) j)]
          (recur (inc i) (assoc pi i j2)))))))

(defn longest-prefix
  "Length of the longest proper prefix of `s` that is also a suffix."
  [s]
  (if (empty? s) 0 (apply max (prefix-function s))))
