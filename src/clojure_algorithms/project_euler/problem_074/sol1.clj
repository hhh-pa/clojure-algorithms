(ns clojure-algorithms.project-euler.problem-074.sol1
  "Project Euler 74: digit factorial chains.

  Starting with 69 produces a chain of five non-repeating terms, but the
  longest non-repeating chain with a starting number below one million
  is sixty terms. How many chains, with a starting number below one
  million, contain exactly sixty non-repeating terms?")

(def ^:private digit-factorials
  {\0 1 \1 1 \2 2 \3 6 \4 24 \5 120 \6 720 \7 5040 \8 40320 \9 362880})

(def ^:private cache-sum-digit-factorials (atom {145 145}))

(def ^:private chain-length-cache
  (atom {145 0 169 3 36301 3 1454 3 871 2 45361 2 872 2}))

(defn sum-digit-factorials
  "Returns the sum of the factorial of the digits of n."
  [n]
  (if-let [cached (get @cache-sum-digit-factorials n)]
    cached
    (let [ret (reduce + (map digit-factorials (str n)))]
      (swap! cache-sum-digit-factorials assoc n ret)
      ret)))

(defn chain-length
  "Calculates the length of the chain of non-repeating terms starting
  with n. Previous is a set containing the previous members of the
  chain."
  ([n] (chain-length n #{}))
  ([n previous]
   (if-let [cached (get @chain-length-cache n)]
     cached
     (let [next-number (sum-digit-factorials n)]
       (if (contains? previous next-number)
         (do (swap! chain-length-cache assoc n 0) 0)
         (let [ret (inc (chain-length next-number (conj previous n)))]
           (swap! chain-length-cache assoc n ret)
           ret))))))

(defn solution
  "Returns the number of chains with a starting number below max-start
  which contain exactly num-terms non-repeating terms."
  ([] (solution 60 1000000))
  ([num-terms] (solution num-terms 1000000))
  ([num-terms max-start]
   (count (filter #(= num-terms (chain-length %)) (range 1 max-start)))))
