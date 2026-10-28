(ns clojure-algorithms.maths.pollard-rho
  "Pollard's Rho algorithm for integer factorization.
https://en.wikipedia.org/wiki/Pollard%27s_rho_algorithm")

(defn- gcd
  "Greatest common divisor, always non-negative."
  [a b]
  (loop [a (bigint a) b (bigint b)]
    (if (zero? b)
      (if (neg? a) (- a) a)
      (recur b (mod a b)))))

(defn pollard-rho
  "Use Pollard's Rho algorithm to return a nontrivial factor of num.
The returned factor may be composite and require further factorization.
Returns nil if no factor is found within the given attempts; if num
is prime, this algorithm always returns nil."
  ([num] (pollard-rho num 2 1 3))
  ([num seed] (pollard-rho num seed 1 3))
  ([num seed step] (pollard-rho num seed step 3))
  ([num seed step attempts]
   (let [num (bigint num)]
     (when (< num 2)
       (throw (ex-info "The input value cannot be less than 2"
                       {:type :ValueError})))
     (if (and (> num 2) (zero? (mod num 2)))
       2
       (let [rand-fn (fn [value step modulus]
                       (mod (+ (* value value) step) modulus))]
         (loop [attempt 0 seed (bigint seed) step (bigint step)]
           (if (>= attempt attempts)
             nil
             (let [[divisor hare]
                   (loop [tortoise seed hare seed]
                     (let [tortoise (rand-fn tortoise step num)
                           hare (rand-fn (rand-fn hare step num) step num)
                           divisor (gcd (- hare tortoise) num)]
                       (cond
                         (= divisor 1) (recur tortoise hare)
                         (= divisor num) [nil hare]
                         :else [divisor hare])))]
               (if divisor
                 divisor
                 (recur (inc attempt) hare (inc step)))))))))))
