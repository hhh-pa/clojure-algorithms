(ns clojure-algorithms.other.scoring-algorithm
  "Range-based percentual proximity scoring.

  Port of TheAlgorithms/Python `other/scoring_algorithm.py`.
  Pure: `procentual-proximity` returns new rows instead of mutating.")

(defn get-data
  "Transpose row-oriented `source-data` into per-column float lists."
  [source-data]
  (apply mapv (fn [& col] (mapv double col)) source-data))

(defn calculate-each-score
  "Score each column: weight 0 rewards low values, weight 1 rewards
  high values. Throws on any other weight."
  [data-lists weights]
  (mapv (fn [dlist weight]
          (let [mind (apply min dlist)
                maxd (apply max dlist)
                span (- maxd mind)]
            (case weight
              0 (mapv (fn [item]
                        (if (zero? span) 1 (double (- 1 (/ (- item mind) span)))))
                      dlist)
              1 (mapv (fn [item]
                        (if (zero? span) 0 (double (/ (- item mind) span))))
                      dlist)
              (throw (IllegalArgumentException.
                      (format "Invalid weight of %f provided" (double weight)))))))
        data-lists weights))

(defn generate-final-scores
  "Sum per-column scores into one total per row."
  [score-lists]
  (apply mapv + score-lists))

(defn procentual-proximity
  "Append a total score to each row of `source-data` using `weights`
  (0 = lower is better, 1 = higher is better)."
  [source-data weights]
  (let [final-scores (-> source-data
                         get-data
                         (calculate-each-score weights)
                         generate-final-scores)]
    (mapv (fn [row score] (conj (vec row) score))
          source-data final-scores)))
