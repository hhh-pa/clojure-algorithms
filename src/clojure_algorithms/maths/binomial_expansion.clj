(ns clojure-algorithms.maths.binomial-expansion
  "Value of (a + b)^n computed with the Binomial Theorem.
  See https://en.wikipedia.org/wiki/Binomial_theorem")

(defn- binomial-coefficient
  "Number of ways to choose `k` items from `n` (exact integer arithmetic)."
  [n k]
  (let [k (min k (- n k))]
    (quot (reduce *' 1 (range (inc (- n k)) (inc n)))
          (reduce *' 1 (range 1 (inc k))))))

(defn- int-pow
  "`base` raised to the non-negative integer `exp`."
  [base exp]
  (reduce * 1 (repeat exp base)))

(defn binomial-expansion
  "Computes (a + b)^n using the Binomial Theorem. Works for positive and
  negative integer exponents. Raises ZeroDivisionError when a + b is 0
  and n is negative."
  [a b n]
  (when (and (zero? (+ a b)) (neg? n))
    (throw (ex-info "Cannot raise 0 to the negative power" {:type :ZeroDivisionError})))
  (let [m (abs (long n))
        value (reduce + (map (fn [i]
                               (* (binomial-coefficient m i)
                                  (int-pow a (- m i))
                                  (int-pow b i)))
                             (range (inc m))))]
    (if (neg? n) (/ 1.0 value) value)))
