(ns clojure-algorithms.genetic-algorithm.basic-string)

;; Classic 4-phase genetic algorithm (evaluation, selection, crossover,
;; mutation) evolving a random string population toward a target string.
;; Randomness goes through an explicit java.util.Random for testability.

(def n-population 200)
(def n-selected 50)
(def mutation-probability 0.4)

(defn- rand-int-between
  "Inclusive random int in [lo, hi] from rng."
  [rng lo hi]
  (+ lo (.nextInt ^java.util.Random rng (inc (- hi lo)))))

(defn evaluate
  "Score item against main-target by counting chars in the right
  position. Returns [item score]."
  [item main-target]
  [item (double (count (filter true? (map = item main-target))))])

(defn crossover
  "Slice and combine two parent strings at a random point.
  Returns [child-1 child-2]."
  ([parent-1 parent-2] (crossover (java.util.Random.) parent-1 parent-2))
  ([rng parent-1 parent-2]
   (let [slice (.nextInt ^java.util.Random rng (count parent-1))]
     [(str (subs parent-1 0 slice) (subs parent-2 slice))
      (str (subs parent-2 0 slice) (subs parent-1 slice))])))

(defn mutate
  "With mutation-probability, replace one random gene of child with a
  random gene from genes."
  ([child genes] (mutate (java.util.Random.) child genes mutation-probability))
  ([rng child genes mutation-p]
   (if (< (.nextDouble ^java.util.Random rng) mutation-p)
     (let [i (dec (.nextInt ^java.util.Random rng (inc (count child))))
           idx (mod i (count child))]
       (apply str (assoc (vec child) idx (nth genes (.nextInt ^java.util.Random rng (count genes))))))
     child)))

(defn select
  "Select the second parent and generate a new population from parent-1.
  More children are generated for fitter parents (up to 10 rounds)."
  [rng parent-1 population-score genes n-selected mutation-p]
  (let [child-n (min 10 (inc (int (* (second parent-1) 100))))]
    (vec (mapcat (fn [_]
                   (let [parent-2 (first (nth population-score
                                              (rand-int-between rng 0 n-selected)))
                         [child-1 child-2] (crossover rng (first parent-1) parent-2)]
                     [(mutate rng child-1 genes mutation-p)
                      (mutate rng child-2 genes mutation-p)]))
                 (range child-n)))))

(defn basic
  "Evolve a random population until it matches target.
  Returns [generation total-population best-string].
  Options: :rng, :debug (default true), :n-population, :n-selected,
  :mutation-probability."
  [target genes & {:keys [rng debug n-population n-selected mutation-probability]
                   :or {debug true
                        n-population n-population
                        n-selected n-selected
                        mutation-probability mutation-probability}}]
  (let [rng (or rng (java.util.Random.))]
    (when (< n-population n-selected)
      (throw (ex-info (str n-population " must be bigger than " n-selected) {})))
    (let [missing (sort (set (filter #(not (some (fn [g] (= g %)) genes)) target)))]
      (when (seq missing)
        (throw (ex-info (str (vec missing) " is not in genes list, evolution cannot converge") {}))))
    (loop [population (vec (repeatedly n-population
                                       #(apply str (repeatedly (count target)
                                                               (fn [] (nth genes (.nextInt rng (count genes))))))))
           generation 0
           total-population 0]
      (let [generation (inc generation)
            total-population (+ total-population (count population))
            scored (sort-by second > (map #(evaluate % target) population))]
        (if (= (ffirst scored) target)
          [generation total-population (ffirst scored)]
          (do
            (when (and debug (zero? (mod generation 10)))
              (println (str "\nGeneration: " generation
                            "\nTotal Population: " total-population
                            "\nBest score: " (second (first scored))
                            "\nBest string: " (first (first scored)))))
            (let [kept (vec (take (quot n-population 3) population))
                  normalized (mapv (fn [[item score]] [item (/ score (count target))]) scored)
                  grown (loop [i 0 pop kept]
                          (if (or (>= i n-selected) (> (count pop) n-population))
                            pop
                            (recur (inc i)
                                   (into pop (select rng (nth normalized i)
                                                     normalized genes n-selected
                                                     mutation-probability)))))]
              (recur grown generation total-population))))))))
