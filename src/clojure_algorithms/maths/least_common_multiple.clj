(ns clojure-algorithms.maths.least-common-multiple
  "Port of maths/least_common_multiple.py.")

(defn- gcd
  "Greatest common divisor via Euclid's algorithm."
  [a b]
  (loop [a (abs (long a)) b (abs (long b))]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn least-common-multiple-slow
  "Finds the least common multiple of two numbers by incrementally
  testing multiples of the larger number.
  Learn more: https://en.wikipedia.org/wiki/Least_common_multiple"
  [first-num second-num]
  (if (or (zero? first-num) (zero? second-num))
    0
    (let [step (max first-num second-num)]
      ;; Note: upstream continues while either remainder is positive
      ;; (Python floored modulo); that exact condition is preserved.
      (loop [common-mult step]
        (if (or (pos? (mod common-mult first-num))
                (pos? (mod common-mult second-num)))
          (recur (+ common-mult step))
          common-mult)))))

(defn least-common-multiple-fast
  "Finds the least common multiple of two numbers using the greatest
  common divisor.
  https://en.wikipedia.org/wiki/Least_common_multiple#Using_the_greatest_common_divisor"
  [first-num second-num]
  (if (or (zero? first-num) (zero? second-num))
    0
    (* (quot first-num (gcd first-num second-num)) second-num)))

(defn benchmark
  [& _args]
  ;; Upstream benchmark only prints timeit timings; there is no portable
  ;; pure-Clojure behavior, so it is kept as a stub.
  (throw (ex-info "benchmark requires timeit; no port applicable"
                  {:type :UnsupportedOperation})))
