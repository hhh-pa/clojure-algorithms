(ns clojure-algorithms.maths.recursive-digit-sum
  "The super digit problem is defined as follows:
  Given an integer n represented as a string and an integer k,
  the goal is to find the super digit of the number formed by concatenating
  the integer n k times.

  The super digit of a number is defined recursively:
  - If the number has only one digit, that digit is the super digit.
  - Otherwise, the super digit is the super digit of the sum of its digits.

  For example, for n = \"9875\" and k = 4, the concatenated number is:
  super_digit(9875987598759875), which can be reduced by summing its digits.")

(defn- digit-sum
  "Sums the decimal digits of a string or number."
  [n]
  (reduce + 0 (map (fn [ch]
                     (let [d (Character/digit ch 10)]
                       (when (neg? d)
                         (throw (ex-info (str "invalid literal for int(): " ch)
                                         {:type :ValueError})))
                       d))
                   (str n))))

(defn super-digit
  "Computes the super digit of a number formed by concatenating
  n-str repeated times.

  Parameters:
  n-str (str): The string representation of the integer.
  repetitions (int): The number of times to concatenate n_str.

  Returns:
  int: The super digit of the concatenated number."
  [n-str repetitions]
  (loop [total (* (digit-sum n-str) repetitions)]
    (if (< total 10)
      total
      (recur (digit-sum total)))))
