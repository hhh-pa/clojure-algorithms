(ns clojure-algorithms.maths.chudnovsky-algorithm
  "Port of maths/chudnovsky_algorithm.py")

(defn- factorial
  "Exact factorial of a non-negative integer."
  [n]
  (reduce *' 1 (range 1 (inc n))))

(defn pi
  "Compute pi to `precision` significant digits with the Chudnovsky algorithm,
  a fast method based on Ramanujan's pi formulae (about 14 digits per
  iteration). Returns the digits as a string.
  https://en.wikipedia.org/wiki/Chudnovsky_algorithm"
  [precision]
  (let [precision (if (boolean? precision) (if precision 1 0) precision)]
    (when-not (integer? precision)
      (throw (ex-info "Undefined for non-integers" {:type :TypeError})))
    (when (< precision 1)
      (throw (ex-info "Undefined for non-natural numbers" {:type :ValueError})))
    (let [mc (java.math.MathContext. (int precision)
                                     java.math.RoundingMode/HALF_EVEN)
          num-iterations (long (Math/ceil (/ (double precision) 14.0)))
          constant-term (.multiply (BigDecimal. "426880")
                                   (.sqrt (BigDecimal. "10005") mc)
                                   mc)]
      (loop [k 1
             linear-term (bigint 13591409)
             exponential-term (bigint 1)
             partial-sum (BigDecimal. "13591409")]
        (if (>= k num-iterations)
          (let [s (.toPlainString (.divide constant-term partial-sum mc))]
            (subs s 0 (dec (count s))))
          (let [fact-k (factorial k)
                multinomial-term (quot (factorial (* 6 k))
                                       (*' (factorial (* 3 k)) fact-k fact-k fact-k))
                linear-term' (+ linear-term 545140134)
                exponential-term' (* exponential-term -262537412640768000)
                term (.divide (BigDecimal. (biginteger (* multinomial-term
                                                          linear-term')))
                              (BigDecimal. (biginteger exponential-term'))
                              mc)]
            (recur (inc k)
                   linear-term'
                   exponential-term'
                   (.add partial-sum term mc))))))))
