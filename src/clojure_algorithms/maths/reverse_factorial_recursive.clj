(ns clojure-algorithms.maths.reverse-factorial-recursive
  "Find x such that x! equals a given value.")

(defn reverse-factorial-recursive
  "Return x such that x! == value, otherwise return -1."
  ([value] (reverse-factorial-recursive value 1))
  ([value current-divisor]
   (when (not (integer? value))
     (throw (ex-info "value must be an integer" {:type :TypeError})))
   (when (not (integer? current-divisor))
     (throw (ex-info "current_divisor must be an integer" {:type :TypeError})))
   (when (< value 1)
     (throw (ex-info "value must be a positive integer" {:type :ValueError})))
   (cond (and (= value 1) (= current-divisor 1)) 1
         (= value 1) (dec current-divisor)
         (not (zero? (mod value current-divisor))) -1
         :else (recur (quot value current-divisor) (inc current-divisor)))))
