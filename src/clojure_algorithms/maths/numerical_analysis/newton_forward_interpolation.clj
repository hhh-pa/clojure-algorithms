(ns clojure-algorithms.maths.numerical-analysis.newton-forward-interpolation
  "Newton forward interpolation.
https://www.geeksforgeeks.org/newton-forward-backward-interpolation/")

(defn ucal
  "Compute u * (u - 1) * ... * (u - p + 1)."
  [u p]
  (loop [i 1
         temp u]
    (if (>= i p)
      temp
      (recur (inc i) (* temp (- u i))))))

(defn- factorial
  [n]
  (reduce *' 1 (range 1 (inc n))))

(defn main
  "Read interpolation data from stdin and print the interpolated value."
  []
  (let [n (Integer/parseInt (do (print "enter the numbers of values: ")
                                (flush)
                                (read-line)))]
    (println "enter the values of parameters in a list: ")
    (let [x (mapv #(Integer/parseInt %) (.split (read-line) " "))]
      (println "enter the values of corresponding parameters: ")
      (let [first-col (mapv (fn [_] (Double/parseDouble (read-line))) (range n))
            value (Integer/parseInt (do (print "enter the value to interpolate: ")
                                        (flush)
                                        (read-line)))
            u (/ (double (- value (nth x 0))) (- (nth x 1) (nth x 0)))
            table (loop [i 1
                         table (mapv (fn [v] (assoc (vec (repeat n 0)) 0 (double v)))
                                     first-col)]
                    (if (>= i n)
                      table
                      (recur (inc i)
                             (reduce (fn [table j]
                                       (assoc-in table [j i]
                                                 (- (get-in table [(inc j) (dec i)])
                                                    (get-in table [j (dec i)]))))
                                     table
                                     (range (- n i))))))
            result (reduce (fn [summ i]
                             (+ summ (/ (* (ucal u i) (get-in table [0 i]))
                                        (factorial i))))
                           (double (get-in table [0 0]))
                           (range 1 n))]
        (println (str "the value at " value " is " result))))))
