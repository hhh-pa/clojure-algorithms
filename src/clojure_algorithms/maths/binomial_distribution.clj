(ns clojure-algorithms.maths.binomial-distribution
  "For more information about the Binomial Distribution -
https://en.wikipedia.org/wiki/Binomial_distribution")

(defn- factorial
  "Exact factorial of a non-negative integer."
  [n]
  (reduce *' 1 (range 1 (inc n))))

(defn binomial-distribution
  "Return the probability of k successes out of n tries, with p probability
  for one success, using the binomial coefficient n! / (k! (n-k)!)."
  [successes trials prob]
  (when-not (and (number? successes) (number? trials))
    (throw (ex-info "the function is defined for non-negative integers"
                    {:type :TypeError})))
  (when (> successes trials)
    (throw (ex-info "successes must be lower or equal to trials"
                    {:type :ValueError})))
  (when (or (< trials 0) (< successes 0))
    (throw (ex-info "the function is defined for non-negative integers"
                    {:type :ValueError})))
  (when-not (and (integer? successes) (integer? trials))
    (throw (ex-info "the function is defined for non-negative integers"
                    {:type :ValueError})))
  (when-not (number? prob)
    (throw (ex-info "prob has to be in range of 1 - 0" {:type :TypeError})))
  (when-not (< 0 prob 1)
    (throw (ex-info "prob has to be in range of 1 - 0" {:type :ValueError})))
  (let [probability (* (Math/pow (double prob) successes)
                       (Math/pow (- 1.0 (double prob)) (- trials successes)))
        coefficient (/ (double (factorial trials))
                       (* (factorial successes)
                          (factorial (- trials successes))))]
    (* probability coefficient)))
