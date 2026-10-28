(ns clojure-algorithms.maths.special-numbers.weird-number
  "https://en.wikipedia.org/wiki/Weird_number

Fun fact: The set of weird numbers has positive asymptotic density.")

(defn- isqrt
  "Integer square root of n: the greatest integer r with r*r <= n.
Assumes n is a non-negative integer."
  [n]
  (loop [x (long (Math/sqrt (double n)))]
    (cond
      (> (*' x x) n) (recur (dec x))
      (<= (*' (inc x) (inc x)) n) (recur (inc x))
      :else x)))

(defn factors
  "Returns a sorted vector of the proper divisors of `number` (with 1 always
included, following the upstream implementation)."
  [number]
  (when (neg? number)
    (throw (ex-info "math domain error" {:type :ValueError})))
  (let [limit (isqrt number)]
    (vec (sort (concat [1]
                       (mapcat (fn [i]
                                 (let [j (quot number i)]
                                   (if (= i j) [i] [i j])))
                               (filter #(zero? (rem number %))
                                       (range 2 (inc limit)))))))))

(defn abundant
  "Returns true if `number` is abundant: the sum of its proper divisors
exceeds the number itself."
  [n]
  (> (reduce +' 0 (factors n)) n))

(defn semi-perfect
  "Returns true if `number` is semi-perfect: some subset of its proper
divisors sums to the number itself."
  [number]
  (let [reachable (reduce (fn [sums v]
                            (into sums (map #(+ % v)) sums))
                          #{0}
                          (factors number))]
    (contains? reachable number)))

(defn weird
  "Returns true if `number` is weird: abundant but not semi-perfect."
  [number]
  (and (abundant number) (not (semi-perfect number))))
