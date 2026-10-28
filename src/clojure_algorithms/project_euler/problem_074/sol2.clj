(ns clojure-algorithms.project-euler.problem-074.sol2
  "Project Euler 74: digit factorial chains (iterative variant).

  How many chains, with a starting number below one million, contain
  exactly sixty non-repeating terms? This solution loops over starting
  numbers, generating each chain of non-repeating items using the cached
  sizes of previous chains, stopping before a repeating item or when the
  chain grows longer than desired.")

(def ^:private digit-factorial
  {\0 1 \1 1 \2 2 \3 6 \4 24 \5 120 \6 720 \7 5040 \8 40320 \9 362880})

(defn digit-factorial-sum
  "Performs the sum of the factorial of all the digits in number."
  [number]
  (when-not (integer? number)
    (throw (IllegalArgumentException. "Parameter number must be int")))
  (when (neg? number)
    (throw (IllegalArgumentException.
            "Parameter number must be greater than or equal to 0")))
  (reduce + (map digit-factorial (str number))))

(defn solution
  "Returns the number of numbers below number-limit that produce chains
  with exactly chain-length non-repeating elements."
  ([] (solution 60 1000000))
  ([chain-length] (solution chain-length 1000000))
  ([chain-length number-limit]
   (when-not (and (integer? chain-length) (integer? number-limit))
     (throw (IllegalArgumentException.
             "Parameters chain_length and number_limit must be int")))
   (when (or (<= chain-length 0) (<= number-limit 0))
     (throw (IllegalArgumentException.
             "Parameters chain_length and number_limit must be greater than 0")))
   (loop [start 1 chains-counter 0 chain-lengths {}]
     (if (>= start number-limit)
       chains-counter
       (let [[chain-set chain-set-length chain-element]
             (loop [elem start cset #{} clen 0]
               (if (and (not (contains? chain-lengths elem))
                        (not (contains? cset elem))
                        (<= clen chain-length))
                 (recur (digit-factorial-sum elem)
                        (conj cset elem) (inc clen))
                 [cset clen elem]))
             total (+ chain-set-length (get chain-lengths chain-element 0))]
         (recur (inc start)
                (if (= total chain-length) (inc chains-counter) chains-counter)
                (assoc chain-lengths start total)))))))
