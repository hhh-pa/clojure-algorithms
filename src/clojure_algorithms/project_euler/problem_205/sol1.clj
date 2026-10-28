(ns clojure-algorithms.project-euler.problem-205.sol1
  "Project Euler Problem 205: dice game probability.

  Peter has nine four-sided (pyramidal) dice and Colin has six six-sided
  (cubic) dice. Both roll and compare totals; the highest total wins.
  Computes the probability that Pyramidal Peter beats Cubic Colin, rounded
  to seven decimal places.")

(defn- dice-combos
  "Generates all face combinations for dice-number dice with sides-number
  sides."
  [sides-number dice-number]
  (if (zero? dice-number)
    [[]]
    (for [rest-combo (dice-combos sides-number (dec dice-number))
          face (range 1 (inc sides-number))]
      (conj rest-combo face))))

(defn total-frequency-distribution
  "Returns the frequency distribution of the total of dice-number dice with
  sides-number sides."
  [sides-number dice-number]
  (let [max-total (* sides-number dice-number)
        frequencies (long-array (inc max-total) 0)]
    (doseq [combo (dice-combos sides-number dice-number)]
      (let [total (reduce + combo)]
        (aset frequencies total (inc (aget frequencies total)))))
    (vec frequencies)))

(defn solution
  "Returns the probability that Pyramidal Peter beats Cubic Colin, rounded
  to seven decimal places."
  []
  (let [peter-frequencies (total-frequency-distribution 4 9)
        colin-frequencies (total-frequency-distribution 6 6)
        peter-wins (reduce + 0
                           (for [peter-total (range 9 (inc (* 4 9)))]
                             (* (nth peter-frequencies peter-total)
                                (reduce + 0
                                        (subvec (vec colin-frequencies)
                                                6
                                                peter-total)))))
        total-games (* (long (Math/pow 4 9)) (long (Math/pow 6 6)))
        probability (/ (double peter-wins) total-games)]
    (/ (Math/round (* probability 1e7)) 1e7)))
