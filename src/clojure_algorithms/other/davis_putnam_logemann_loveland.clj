(ns clojure-algorithms.other.davis-putnam-logemann-loveland
  "Davis-Putnam-Logemann-Loveland (DPLL) SAT solver for CNF formulae.

  Port of TheAlgorithms/Python `other/davis_putnam_logemann_loveland.py`.
  Clauses are vectors of literal strings (e.g. [\"A1\" \"A2'\" \"A3\");
  formulae are vectors of clauses; models map symbol strings to
  booleans. Immutable maps replace the Python in-place assignment,
  which also fixes the original's aliased-branching quirk.")

(defn clause-str
  "Render a clause in CNF style."
  [clause]
  (str "{" (clojure.string/join " , " clause) "}"))

(defn formula-str
  "Render a formula in CNF style."
  [formula]
  (str "{" (clojure.string/join " , " (map clause-str formula)) "}"))

(defn- symbol-of
  "Uncomplemented symbol of a literal (\"A5'\" -> \"A5\")."
  [literal]
  (if (clojure.string/ends-with? literal "'")
    (subs literal 0 (dec (count literal)))
    literal))

(defn- complement-of
  [literal]
  (if (clojure.string/ends-with? literal "'")
    (subs literal 0 (dec (count literal)))
    (str literal "'")))

(defn- assigned-values
  "Value of each literal under `model` (true/false/nil)."
  [clause model]
  (mapv (fn [literal]
          (let [sym (symbol-of literal)]
            (when (contains? model sym)
              (let [v (get model sym)]
                (if (and (some? v)
                         (clojure.string/ends-with? literal "'"))
                  (not v)
                  v)))))
        clause))

(defn clause-evaluate
  "Evaluate `clause` under `model`: true, false, or nil (undecided)."
  [clause model]
  (let [lits (set clause)]
    (if (some #(contains? lits (complement-of %)) clause)
      true
      (let [values (assigned-values clause model)
            decided (filter #(or (true? %) (nil? %)) values)]
        (if (seq decided)
          (first decided)
          (boolean (some true? values)))))))

(defn generate-parameters
  "Return `[clauses symbols]` for `formula`."
  [formula]
  [formula
   (vec (distinct (mapcat #(map symbol-of %) formula)))])

(defn find-pure-symbols
  "Return `[pure-symbols assignment]`: symbols occurring in only one
  polarity among not-yet-true clauses."
  [clauses symbols model]
  (let [literals (vec (mapcat identity
                              (remove #(true? (clause-evaluate % model))
                                      clauses)))
        lit-set (set literals)
        pure (filterv (fn [s]
                        (let [c (str s "'")]
                          (not= (contains? lit-set s)
                                (contains? lit-set c))))
                      symbols)]
    [pure (into {} (map (fn [s]
                          [s (if (contains? lit-set s) true false)])
                        pure))]))

(defn find-unit-clauses
  "Return `[unit-symbols assignment]` for unit clauses under `model`."
  [clauses model]
  (let [unit-lits
        (into []
              (comp (map (fn [clause]
                           (cond
                             (= 1 (count clause)) (first clause)
                             :else
                             (let [values (assigned-values clause model)
                                   falses (count (filter false? values))
                                   nones (filter #(nil? (second %))
                                                 (map vector clause values))]
                               (when (and (= falses (dec (count clause)))
                                          (= 1 (count nones)))
                                 (ffirst nones))))))
                    (filter some?))
              clauses)
        assignment (into {} (map (fn [lit]
                                   [(symbol-of lit)
                                    (not (clojure.string/ends-with? lit "'"))])
                                 unit-lits))]
    [(mapv symbol-of unit-lits) assignment]))

(defn dpll
  "Run DPLL on `clauses` with unassigned `symbols` under `model`.
  Return `[satisfiable? model-or-nil]`."
  [clauses symbols model]
  (let [checks (mapv #(clause-evaluate % model) clauses)]
    (cond
      (some false? checks) [false nil]
      (every? true? checks) [true model]
      :else
      (let [[pure-syms pure-assign] (find-pure-symbols clauses symbols model)]
        (if (seq pure-syms)
          (let [p (first pure-syms)]
            (recur clauses (vec (remove #(= % p) symbols))
                   (assoc model p (pure-assign p))))
          (let [[unit-syms unit-assign] (find-unit-clauses clauses model)]
            (if (seq unit-syms)
              (let [p (first unit-syms)]
                (recur clauses (vec (remove #(= % p) symbols))
                       (assoc model p (unit-assign p))))
              (let [p (first symbols)
                    rest-syms (vec (rest symbols))
                    [sat :as result]
                    (dpll clauses rest-syms (assoc model p true))]
                ;; NOTE: the Python original uses `or` between two
                ;; 2-tuples, which never tries the False branch; the
                ;; explicit check here keeps the search complete.
                (if (true? sat)
                  result
                  (dpll clauses rest-syms (assoc model p false)))))))))))

(defn solve
  "Solve CNF `formula`; return `[satisfiable? model-or-nil]`."
  [formula]
  (let [[clauses symbols] (generate-parameters formula)]
    (dpll clauses symbols {})))

(defn generate-clause
  "Random clause over symbols A1..A5."
  []
  (loop [lits []]
    (if (= (count lits) (inc (rand-int 5)))
      lits
      (let [lit (str "A" (inc (rand-int 5))
                     (when (zero? (rand-int 2)) "'"))]
        (recur (if (some #(= % lit) lits) lits (conj lits lit)))))))

(defn generate-formula
  "Random formula of 1..10 random clauses."
  []
  (let [n (inc (rand-int 10))]
    (loop [clauses #{}]
      (if (>= (count clauses) n)
        (vec clauses)
        (recur (conj clauses (generate-clause)))))))
