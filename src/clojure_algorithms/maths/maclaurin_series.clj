(ns clojure-algorithms.maths.maclaurin-series
  "https://en.wikipedia.org/wiki/Taylor_series#Trigonometric_functions")

(defn- factorial
  [n]
  (reduce *' 1 (range 1 (inc n))))

(defn- check-args
  [fname theta accuracy]
  (when-not (number? theta)
    (throw (ex-info (str fname "() requires either an int or float for theta")
                    {:type :ValueError})))
  (when-not (and (integer? accuracy) (pos? accuracy))
    (throw (ex-info (str fname "() requires a positive int for accuracy")
                    {:type :ValueError}))))

(defn- reduce-angle
  "Reduces theta modulo 2*pi, matching upstream's floored division."
  [theta]
  (let [t (double theta)
        div (Math/floor (/ t (* 2 Math/PI)))]
    (- t (* 2 div Math/PI))))

(defn maclaurin-sin
  "Finds the maclaurin approximation of sin for the angle theta."
  ([theta] (maclaurin-sin theta 30))
  ([theta accuracy]
   (check-args "maclaurin_sin" theta accuracy)
   (let [theta (reduce-angle theta)]
     (reduce + 0.0
             (map (fn [r]
                    (/ (* (Math/pow -1.0 r)
                          (Math/pow theta (inc (* 2 r))))
                       (double (factorial (inc (* 2 r))))))
                  (range accuracy))))))

(defn maclaurin-cos
  "Finds the maclaurin approximation of cos for the angle theta."
  ([theta] (maclaurin-cos theta 30))
  ([theta accuracy]
   (check-args "maclaurin_cos" theta accuracy)
   (let [theta (reduce-angle theta)]
     (reduce + 0.0
             (map (fn [r]
                    (/ (* (Math/pow -1.0 r)
                          (Math/pow theta (* 2 r)))
                       (double (factorial (* 2 r)))))
                  (range accuracy))))))
