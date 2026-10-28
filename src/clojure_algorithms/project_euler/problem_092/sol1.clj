(ns clojure-algorithms.project-euler.problem-092.sol1
  "Project Euler 92: square digit chains.

  A number chain is created by continuously adding the square of the
  digits in a number to form a new number until it has been seen before.
  Every starting number will eventually arrive at 1 or 89. How many
  starting numbers below ten million will arrive at 89?

  Uses digit DP: chain endpoints are precomputed for the small range of
  possible digit-square sums, then a digit DP over the decimal digits of
  (number - 1) counts how many integers have each digit-square sum.")

(defn digit-square-sum
  "Returns the sum of the squares of the decimal digits of n."
  [n]
  (loop [n n total 0]
    (if (zero? n)
      total
      (recur (quot n 10) (+ total (let [d (rem n 10)] (* d d)))))))

(defn- ends-at-89-table
  "Returns a boolean array where entry i tells whether the digit-square
  chain starting at i ends at 89."
  [limit]
  (let [ends (boolean-array limit)]
    (doseq [i (range 1 limit)]
      (loop [n i]
        (cond (= n 1) (aset ends i false)
              (= n 89) (aset ends i true)
              :else (recur (digit-square-sum n)))))
    ends))

(defn solution
  "Returns how many starting numbers below number will arrive at 89 in
  the digit-square chain."
  ([] (solution 10000000))
  ([number]
   (let [num-digits (if (> number 1) (count (str (dec number))) 1)
         limit (inc (* num-digits 81))
         ends (ends-at-89-table limit)
         digits (if (> number 1)
                  (mapv #(Character/digit ^char % 10) (str (dec number)))
                  [0])]
     (loop [digits digits tight {0 1} free {}]
       (if (empty? digits)
         (reduce + 0
                 (map val
                      (filter (fn [[dss _]]
                                (and (pos? dss)
                                     (< dss limit)
                                     (aget ends dss)))
                              (concat tight free))))
         (let [lim (first digits)
               [new-tight new-free]
               (reduce (fn [[new-tight new-free] [dss cnt]]
                         (reduce (fn [[new-tight new-free] d]
                                   (let [new-val (+ dss (* d d))]
                                     (if (>= new-val limit)
                                       [new-tight new-free]
                                       (if (= d lim)
                                         [(assoc new-tight new-val
                                                 (+ (get new-tight new-val 0)
                                                    cnt))
                                          new-free]
                                         [new-tight
                                          (assoc new-free new-val
                                                 (+ (get new-free new-val 0)
                                                    cnt))]))))
                                 [new-tight new-free]
                                 (range (inc lim))))
                       [{} {}]
                       tight)
               new-free
               (reduce (fn [new-free [dss cnt]]
                         (reduce (fn [new-free d]
                                   (let [new-val (+ dss (* d d))]
                                     (if (>= new-val limit)
                                       new-free
                                       (assoc new-free new-val
                                              (+ (get new-free new-val 0)
                                                 cnt)))))
                                 new-free
                                 (range 10)))
                       new-free
                       free)]
           (recur (rest digits) new-tight new-free)))))))
