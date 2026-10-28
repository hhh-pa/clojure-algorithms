(ns clojure-algorithms.maths.gcd-of-n-numbers
  "Gcd of N Numbers
Reference: https://en.wikipedia.org/wiki/Greatest_common_divisor")

(defn get-factors
  "Return a map of prime factor to exponent for the positive integer number.
A factor can be all numbers from 2 to number: if number % factor == 0,
recur on number // factor, else increase factor by one."
  ([number] (get-factors number nil 2))
  ([number factors] (get-factors number factors 2))
  ([number factors factor]
   (cond
     (and (integer? number) (= number 1))
     {1 1}
     (not (and (integer? number) (pos? number)))
     (throw (ex-info "number must be integer and greater than zero"
                     {:type :TypeError}))
     :else
     (loop [n number f factor acc (if (seq factors) factors {})]
       (cond
         (= n f)
         (update acc f (fnil inc 0))
         (pos? (mod n f))
         (recur n (inc f) acc)
         :else
         (recur (quot n f) f (update acc f (fnil inc 0))))))))

(defn get-greatest-common-divisor
  "Get the gcd of n numbers via common prime factors (minimum exponents)."
  [& numbers]
  (when (empty? numbers)
    (throw (ex-info "not enough values to unpack (expected at least 1, got 0)"
                    {:type :ValueError})))
  (let [all (try
              (mapv get-factors numbers)
              (catch clojure.lang.ExceptionInfo e
                (if (= :TypeError (:type (ex-data e)))
                  (throw (ex-info "numbers must be integer and greater than zero"
                                  {:type :Exception}))
                  (throw e))))
        same (reduce (fn [a b]
                       (into {} (for [[k v] a :when (contains? b k)]
                                  [k (min v (get b k))])))
                     all)]
    (reduce *' 1 (map (fn [[f p]] (reduce *' 1 (repeat p f))) same))))
