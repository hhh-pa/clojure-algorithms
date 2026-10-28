(ns clojure-algorithms.project-euler.problem-191.sol1
  "Project Euler Problem 191: prize strings.

  A school forfeits the prize for children absent three consecutive days or
  late on more than one occasion. During an n-day period a trinary string is
  formed of L (late), O (on time), and A (absent). Exactly forty-three
  strings lead to a prize over 4 days. Counts the prize strings over a
  30-day period.")

(def cache
  "Memoization cache for the recursive prize string calculation."
  (atom {}))

(defn calculate
  "Helper for the recursion with the number of days and the initial values
  for the number of consecutive absent days and total late days."
  [days absent late]
  (cond
    (= late 3) 0
    (= absent 2) 0
    (zero? days) 1
    :else
    (let [key [days absent late]]
      (if-let [cached (get @cache key)]
        cached
        (let [prize-strings (+ (calculate (dec days) absent (inc late))
                               (calculate (dec days) (inc absent) 0)
                               (calculate (dec days) absent 0))]
          (swap! cache assoc key prize-strings)
          prize-strings)))))

(defn solution
  "Returns the number of possible prize strings for a particular number of
  days, using a simple recursive function with caching to speed it up."
  ([] (solution 30))
  ([days]
   (calculate days 0 0)))
