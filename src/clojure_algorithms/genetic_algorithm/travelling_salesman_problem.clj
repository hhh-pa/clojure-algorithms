(ns clojure-algorithms.genetic-algorithm.travelling-salesman-problem)

;; Genetic algorithm for the travelling salesman problem.
;; Randomness goes through an explicit java.util.Random for testability.

(def cities
  {0 [0 0] 1 [0 5] 2 [3 8] 3 [8 10]
   4 [12 8] 5 [12 4] 6 [8 0] 7 [6 2]})

(defn- rand-int-between
  "Inclusive random int in [lo, hi] from rng."
  [rng lo hi]
  (+ lo (.nextInt ^java.util.Random rng (inc (- hi lo)))))

(defn- shuffled
  "Random permutation of coll using rng (Fisher-Yates)."
  [rng coll]
  (let [a (java.util.ArrayList. ^java.util.List (vec coll))]
    (java.util.Collections/shuffle a ^java.util.Random rng)
    (vec a)))

(defn distance
  "Euclidean distance between two [x y] points."
  [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2))))

(defn init
  "Random initial population: each chromosome starts and ends at city 0
  with the other cities in random order. Returns [chromosomes cities-list]."
  ([cities-map population-size] (init cities-map population-size (java.util.Random.)))
  ([cities-map population-size rng]
   (let [keys-ordered (sort (keys cities-map))]
     (when (empty? keys-ordered)
       (throw (ex-info "list assignment index out of range" {:cities cities-map})))
     (let [cities-list (vec (rest keys-ordered))]
       [(vec (repeatedly population-size
                         (fn [] (vec (concat [0] (shuffled rng cities-list) [0])))))
        cities-list]))))

(defn tour-length
  "Total length of a chromosome tour."
  [cities-map chromosome]
  (reduce + (map (fn [a b] (distance (cities-map a) (cities-map b)))
                 chromosome (rest chromosome))))

(defn fitness
  "Fitness of each chromosome (1 / tour length); tracks the best tour.
  Returns [fitness-matrix best-path best-distance]."
  [cities-map chromosomes best-path best-distance]
  (reduce (fn [[matrix best-p best-d] chromosome]
            (let [total (tour-length cities-map chromosome)]
              [(conj matrix (/ 1 total))
               (if (< total best-d) chromosome best-p)
               (min total best-d)]))
          [[] best-path best-distance]
          chromosomes))

(defn chose-ts
  "Tournament selection: pick the fitter of two random chromosomes,
  population-size times."
  [rng fitness-matrix chromosomes population-size]
  (vec (repeatedly population-size
                   (fn []
                     (let [x1 (rand-int-between rng 0 (dec population-size))
                           x2 (rand-int-between rng 0 (dec population-size))]
                       (if (>= (nth fitness-matrix x1) (nth fitness-matrix x2))
                         (nth chromosomes x1)
                         (nth chromosomes x2)))))))

(defn chose-rws
  "Roulette-wheel selection over fitness-matrix."
  [rng fitness-matrix chromosomes population-size]
  (let [total (reduce + fitness-matrix)
        probabilities (mapv #(/ % total) fitness-matrix)]
    (reduce (fn [result i]
              (let [r (.nextDouble ^java.util.Random rng)]
                (loop [j 0 k 0.0]
                  (let [k (+ k (nth probabilities j))]
                    (if (<= r k)
                      (assoc result i (nth chromosomes j))
                      (recur (inc j) k))))))
            (vec chromosomes)
            (range population-size))))

(defn crossing
  "Crossover of two chromosomes with crossover-probability; duplicate
  cities introduced by the swap are repaired with missing cities.
  Returns [new-a new-b]."
  [rng chromosome-a chromosome-b crossover-probability cities-list]
  (if (> (.nextDouble ^java.util.Random rng) crossover-probability)
    [chromosome-a chromosome-b]
    (let [[lo hi] (sort [(rand-int-between rng 0 (dec (count chromosome-a)))
                         (rand-int-between rng 0 (dec (count chromosome-a)))])
          swapped (reduce (fn [[a b] k]
                            [(assoc a (+ lo k) (nth b (+ lo k)))
                             (assoc b (+ lo k) (nth a (+ lo k)))])
                          [(vec chromosome-a) (vec chromosome-b)]
                          (range (inc (- hi lo))))]
      (mapv (fn [chromosome]
              (if (= (count (set chromosome)) (count chromosome))
                chromosome
                (reduce (fn [chrom segment-index]
                          (let [pos (+ lo segment-index)
                                target (first (filter (fn [idx]
                                                        (and (not= idx pos)
                                                             (= (nth chrom idx) (nth chrom pos))))
                                                      (range 1 (dec (count chrom)))))]
                            (if (nil? target)
                              chrom
                              (let [missing (remove (set chrom) cities-list)]
                                (assoc chrom target
                                       (nth missing (.nextInt ^java.util.Random rng
                                                              (count missing))))))))
                        chromosome
                        (range (inc (- hi lo))))))
            swapped))))

(defn mutate
  "With mutation-probability, swap two interior cities (endpoints stay 0)."
  [rng chromosome mutation-probability]
  (if (> (.nextDouble ^java.util.Random rng) mutation-probability)
    chromosome
    (do
      (when (< (count chromosome) 3)
        (throw (ex-info "empty range for randrange()" {})))
      (let [i (rand-int-between rng 1 (- (count chromosome) 2))
            j (rand-int-between rng 1 (- (count chromosome) 2))]
        (assoc chromosome i (nth chromosome j) j (nth chromosome i))))))

(defn run-ga
  "Run the genetic algorithm. Returns [best-path best-distance].
  Options: :rng."
  [cities-map population-size iterations-num crossover-probability
   mutation-probability & {:keys [rng]}]
  (let [rng (or rng (java.util.Random.))
        [chromosomes cities-list] (init cities-map population-size rng)
        [_ best-path best-distance] (fitness cities-map chromosomes [] Double/POSITIVE_INFINITY)]
    (loop [iteration 0 chromosomes chromosomes
           best-path best-path best-distance best-distance]
      (if (>= iteration iterations-num)
        [best-path best-distance]
        (let [selected (chose-rws rng (first (fitness cities-map chromosomes [] Double/POSITIVE_INFINITY))
                                  chromosomes population-size)
              half (quot population-size 2)
              crossed (reduce (fn [chroms x]
                                (let [[a b] (crossing rng (nth chroms x) (nth chroms (+ x half))
                                                      crossover-probability cities-list)]
                                  (assoc chroms x a (+ x half) b)))
                              (vec selected)
                              (range half))
              mutated (mapv #(mutate rng % mutation-probability) crossed)
              [matrix best-path best-distance] (fitness cities-map mutated best-path best-distance)]
          (recur (inc iteration) mutated best-path best-distance))))))
