(ns clojure-algorithms.project-euler.problem-033.sol1
  "Euler Problem 33: there are exactly four non-trivial digit-cancelling
  fractions less than one with two digits in numerator and denominator. If the
  product of the four is given in lowest terms, find the denominator.")

(defn digit-cancelling?
  "Returns true when num/den is a non-trivial digit-cancelling fraction, i.e.
  cancelling the shared digit gives an equal value."
  [num den]
  (and (not= num den)
       (= (mod num 10) (quot den 10))
       (== (/ (quot num 10) (mod den 10)) (/ num den))))

(defn fraction-list
  "Returns the digit-cancelling fractions found for the given digit length as
  strings like \"49/98\"."
  [digit-len]
  (let [last-digit (Integer/parseInt (str "1" (apply str (repeat digit-len "0"))))]
    (vec (for [num (range 11 last-digit)
               den (range (if (= num 11) 11 10) 100)
               :when (and (not= num den)
                          (= (mod num 10) (quot den 10))
                          (not (zero? (mod den 10)))
                          (digit-cancelling? num den))]
           (str num "/" den)))))

(defn solution
  "Returns the denominator of the product of the digit-cancelling fractions in
  lowest terms."
  ([] (solution 2))
  ([n]
   (long (reduce * 1 (map (fn [frac]
                            (let [[a b] (map #(Integer/parseInt %) (.split frac "/"))
                                  r (/ a b)]
                              (/ (denominator r) (numerator r))))
                          (fraction-list n))))))
