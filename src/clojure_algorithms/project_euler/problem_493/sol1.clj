(ns clojure-algorithms.project-euler.problem-493.sol1
  "Project Euler Problem 493: expected distinct colours.

  70 coloured balls are placed in an urn, 10 for each of the seven rainbow
  colours. Computes the expected number of distinct colours in 20 randomly
  picked balls via the probability that any one colour is missing.")

(def balls-per-colour
  "Number of balls of each colour."
  10)

(def num-colours
  "Number of distinct colours."
  7)

(def num-balls
  "Total number of balls."
  (* balls-per-colour num-colours))

(defn- ncr
  "Binomial coefficient C(n, r) computed with exact integer arithmetic."
  [n r]
  (cond
    (or (neg? r) (> r n)) 0
    :else
    (let [r (min r (- n r))]
      (reduce (fn [acc i] (quot (*' acc (- n (dec i))) i))
              1
              (range 1 (inc r))))))

(defn solution
  "Calculates the expected number of distinct colours in num-picks
  randomly picked balls, formatted with nine digits after the decimal
  point."
  ([] (solution 20))
  ([num-picks]
   (let [total (ncr num-balls num-picks)
         missing-colour (ncr (- num-balls balls-per-colour) num-picks)
         result (* num-colours (- 1 (/ (double missing-colour) total)))]
     (format "%.9f" result))))
