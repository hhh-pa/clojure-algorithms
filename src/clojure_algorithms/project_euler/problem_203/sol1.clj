(ns clojure-algorithms.project-euler.problem-203.sol1
  "Project Euler Problem 203: squarefree Pascal coefficients.

  The first eight rows of Pascal's triangle contain twelve distinct numbers,
  of which all except 4 and 20 are squarefree; their sum is 105. Finds the
  sum of the distinct squarefree numbers in the first 51 rows of Pascal's
  triangle.")

(defn get-pascal-triangle-unique-coefficients
  "Returns the unique coefficients of a Pascal's triangle of depth depth."
  [depth]
  (loop [i 2
         coefficients #{1}
         previous [1]]
    (if (> i depth)
      coefficients
      (let [current (mapv + (conj (vec previous) 0) (vec (cons 0 previous)))]
        (recur (inc i) (into coefficients current) current)))))

(defn get-squarefrees
  "Calculates the squarefree numbers inside unique-coefficients. Any
  non-squarefree n decomposes as n = p*p*r with p prime, so any coefficient
  lower than p*p is squarefree."
  [unique-coefficients]
  (let [non-squarefrees (into #{}
                              (filter (fn [number]
                                        (loop [divisor 2
                                               copy-number number]
                                          (cond
                                            (> (* divisor divisor) copy-number)
                                            false
                                            :else
                                            (let [[remaining multiplicity]
                                                  (loop [c copy-number
                                                         m 0]
                                                    (if (zero? (mod c divisor))
                                                      (recur (quot c divisor)
                                                             (inc m))
                                                      [c m]))]
                                              (if (>= multiplicity 2)
                                                true
                                                (recur (inc divisor)
                                                       remaining))))))
                                      unique-coefficients))]
    (apply disj (set unique-coefficients) non-squarefrees)))

(defn solution
  "Returns the sum of squarefrees for a given Pascal's triangle of depth
  n."
  ([] (solution 51))
  ([n]
   (reduce + 0
           (get-squarefrees (get-pascal-triangle-unique-coefficients n)))))
