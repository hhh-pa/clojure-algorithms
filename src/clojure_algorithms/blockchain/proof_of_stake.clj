(ns clojure-algorithms.blockchain.proof-of-stake
  "Proof-of-stake validator selection: the next block creator is chosen
  at random with probability proportional to stake.")

(defrecord Validator [name stake])

(defn make-validator
  "Creates a Validator with `name` and `stake`."
  [name stake]
  (->Validator name stake))

(defn choose-validator
  "Selects one validator from `validators` with probability proportional
  to stake. Accepts an optional `java.util.Random` for deterministic tests."
  ([validators] (choose-validator validators (java.util.Random.)))
  ([validators rng]
   (let [total (reduce + (map :stake validators))
         roll (* (.nextDouble rng) total)]
     (loop [[v & rest] validators, acc 0]
       (let [acc (+ acc (:stake v))]
         (if (or (< roll acc) (empty? rest))
           v
           (recur rest acc)))))))
