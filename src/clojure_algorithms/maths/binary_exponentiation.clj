(ns clojure-algorithms.maths.binary-exponentiation
  "Binary Exponentiation

  This is a method to find a^b in O(log b) time complexity and is one of the
  most commonly used methods of exponentiation. The method is also useful for
  modular exponentiation, when the solution to (a^b) % c is required.

  To calculate a^b:
  - If b is even, then a^b = (a * a)^(b / 2)
  - If b is odd, then a^b = a * a^(b - 1)
  Repeat until b = 1 or b = 0

  For modular exponentiation, we use the fact that
  (a * b) % c = ((a % c) * (b % c)) % c")

(defn- check-exponent
  [exponent]
  (when (neg? exponent)
    (throw (ex-info "Exponent must be a non-negative integer"
                    {:type :ValueError}))))

(defn- floored-mod
  "Python-style floored modulo, preserving integer results for integer inputs."
  [a m]
  (if (and (integer? a) (integer? m))
    (mod a m)
    (- a (* m (Math/floor (/ (double a) (double m)))))))

(defn binary-exp-recursive
  "Computes a^b recursively, where a is the base and b is the exponent."
  [base exponent]
  (check-exponent exponent)
  (cond (zero? exponent) 1
        (odd? exponent) (*' (binary-exp-recursive base (dec exponent)) base)
        :else (let [b (binary-exp-recursive base (quot exponent 2))]
                (*' b b))))

(defn binary-exp-iterative
  "Computes a^b iteratively, where a is the base and b is the exponent."
  [base exponent]
  (check-exponent exponent)
  (loop [res 1 b base e exponent]
    (if (zero? e)
      res
      (recur (if (odd? e) (*' res b) res)
             (*' b b)
             (quot e 2)))))

(defn binary-exp-mod-recursive
  "Computes a^b % c recursively, where a is the base, b is the exponent,
  and c is the modulus."
  [base exponent modulus]
  (check-exponent exponent)
  (when (<= modulus 0)
    (throw (ex-info "Modulus must be a positive integer" {:type :ValueError})))
  (cond (zero? exponent) 1
        (odd? exponent)
        (floored-mod (*' (binary-exp-mod-recursive base (dec exponent) modulus)
                         base)
                     modulus)
        :else (let [r (binary-exp-mod-recursive base (quot exponent 2) modulus)]
                (floored-mod (*' r r) modulus))))

(defn binary-exp-mod-iterative
  "Computes a^b % c iteratively, where a is the base, b is the exponent,
  and c is the modulus."
  [base exponent modulus]
  (check-exponent exponent)
  (when (<= modulus 0)
    (throw (ex-info "Modulus must be a positive integer" {:type :ValueError})))
  (loop [res 1 b base e exponent]
    (if (zero? e)
      res
      (recur (if (odd? e)
               (floored-mod (*' (floored-mod res modulus)
                                (floored-mod b modulus))
                            modulus)
               res)
             (*' b b)
             (quot e 2)))))
