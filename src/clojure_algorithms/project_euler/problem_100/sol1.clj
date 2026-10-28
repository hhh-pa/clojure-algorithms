(ns clojure-algorithms.project-euler.problem-100.sol1
  "Project Euler 100: arranged probability.

  If a box contains twenty-one coloured discs, composed of fifteen blue
  discs and six red discs, and two discs were taken at random, the
  probability of taking two blue discs is 1/2. By finding the first
  arrangement to contain over 10^12 discs in total, determine the number
  of blue discs that the box would contain.")

(defn solution
  "Returns the number of blue discs for the first arrangement to contain
  over min-total discs in total."
  ([] (solution 1000000000000))
  ([min-total]
   (loop [prev-numerator 1 numerator 1 prev-denominator 0 denominator 1]
     (if (> numerator (dec (* 2 min-total)))
       (quot (inc denominator) 2)
       (let [prev-numerator (+ prev-numerator (* 2 numerator))
             numerator (+ numerator (* 2 prev-numerator))
             prev-denominator (+ prev-denominator (* 2 denominator))
             denominator (+ denominator (* 2 prev-denominator))]
         (recur prev-numerator numerator prev-denominator denominator))))))
