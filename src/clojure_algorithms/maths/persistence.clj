(ns clojure-algorithms.maths.persistence
  "Persistence of a number.
https://en.wikipedia.org/wiki/Persistence_of_a_number")

(defn- check-persistence-arg
  "Validates the argument for the persistence functions."
  [fname num]
  (when-not (integer? num)
    (throw (ex-info (str fname "() only accepts integral values")
                    {:type :ValueError})))
  (when (neg? num)
    (throw (ex-info (str fname "() does not accept negative values")
                    {:type :ValueError}))))

(defn- digit-values
  "Returns the decimal digits of the string s as numbers."
  [s]
  (map #(- (int %) (int \0)) s))

(defn- persistence-steps
  "Counts the steps needed to reduce num to a single digit by repeatedly
combining its digits with f starting from start."
  [num f start]
  (loop [s (str num), steps 0]
    (if (= 1 (count s))
      steps
      (recur (str (reduce f start (digit-values s))) (inc steps)))))

(defn multiplicative-persistence
  "Return the multiplicative persistence of a given number."
  [num]
  (check-persistence-arg "multiplicative_persistence" num)
  (persistence-steps num *' 1))

(defn additive-persistence
  "Return the additive persistence of a given number."
  [num]
  (check-persistence-arg "additive_persistence" num)
  (persistence-steps num +' 0))
