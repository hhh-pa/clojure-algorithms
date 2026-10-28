(ns clojure-algorithms.project-euler.problem-121.sol1
  "Disc game prize fund.

  A bag contains one red disc and one blue disc. Each turn a disc is drawn and
  returned, then an extra red disc is added. The player pays 1 pound to play
  and wins if they have taken more blue discs than red discs at the end of
  the game. Finds the maximum prize fund for a game in which fifteen turns
  are played.")

(defn- bit-series
  "Lazily generates all 0/1 series of the given length."
  [num-turns]
  (if (zero? num-turns)
    [[]]
    (for [rest-series (bit-series (dec num-turns))
          b [0 1]]
      (conj rest-series b))))

(defn solution
  "Finds the maximum prize fund that should be allocated to a single game in
  which num-turns turns are played."
  ([] (solution 15))
  ([num-turns]
   (let [total-prob (reduce + 0.0
                            (for [series (bit-series num-turns)
                                  :let [num-blue (count (filter #(= 1 %) series))
                                        num-red (- num-turns num-blue)]
                                  :when (< num-red num-blue)]
                              (reduce * 1.0
                                      (map (fn [idx col]
                                             (let [ind (+ idx 2)]
                                               (if (zero? col)
                                                 (/ (double (dec ind)) ind)
                                                 (/ 1.0 ind))))
                                           (range)
                                           series))))]
     (long (/ 1 total-prob)))))
