(ns clojure-algorithms.maths.power-using-recursion
  "Raise base to the power of exponent using recursion.")

(defn- mul-strict
  "Multiply with Python semantics: numbers multiply, and a string repeated
by an integer concatenates; anything else raises a TypeError."
  [a b]
  (cond
    (and (number? a) (number? b)) (*' a b)
    (and (string? a) (integer? b)) (apply str (repeat (max 0 b) a))
    (and (integer? a) (string? b)) (apply str (repeat (max 0 a) b))
    (string? a) (throw (ex-info (str "can't multiply sequence by non-int of type '"
                                     "str'")
                                {:type :TypeError}))
    :else (throw (ex-info "unsupported operand for *" {:type :TypeError}))))

(defn- dec-strict
  "Decrement with Python semantics: non-numbers raise a TypeError."
  [x]
  (if (number? x)
    (dec x)
    (throw (ex-info (str "unsupported operand type(s) for -: '"
                         (if (string? x) "str" "unknown")
                         "' and 'int'")
                    {:type :TypeError}))))

(defn power
  "Calculate the power of a base raised to an exponent using recursion."
  [base exponent]
  (if (and (number? exponent) (zero? exponent))
    1
    (if (number? exponent)
      (mul-strict base (power base (dec-strict exponent)))
      (dec-strict exponent))))
