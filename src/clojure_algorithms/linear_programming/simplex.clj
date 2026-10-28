(ns clojure-algorithms.linear-programming.simplex
  "Simplex algorithm for solving linear programs in tableau form with
  `>=`, `<=` and `=` constraints and non-negative decision variables.
  Tableaus are vectors of vectors of doubles; integer-only input is
  rejected, mirroring the float64 requirement upstream.")

(def ^:dynamic Tableau-maxiter
  "Maximum simplex iterations before giving up (prevents cycling)."
  100)

(defrecord Tableau [tableau
                    n-vars
                    n-artificial-vars
                    n-stages
                    n-slack
                    objectives
                    col-titles
                    row-idx
                    col-idx
                    stop-iter
                    n-rows])

(defn- -col-titles
  [n-vars n-slack]
  (vec (concat (map #(str "x" (inc %)) (range n-vars))
               (map #(str "s" (inc %)) (range n-slack))
               ["RHS"])))

(defn make-Tableau
  "Builds a simplex tableau from the nested vector tableau with n-vars
  decision variables and n-artificial-vars artificial variables."
  [tableau n-vars n-artificial-vars]
  (let [type-error (ex-info "Tableau must have type float64" {:type :TypeError})]
    (when-not (and (sequential? tableau)
                   (seq tableau)
                   (every? sequential? tableau)
                   (apply = (map count tableau)))
      (throw type-error))
    (let [elements (vec (apply concat tableau))]
      (when (or (not (every? number? elements))
                (every? integer? elements))
        (throw type-error))
      (let [t (mapv (fn [row] (mapv double row)) tableau)]
        (when-not (every? #(>= % 0) (map peek t))
          (throw (ex-info "RHS must be > 0" {:type :ValueError})))
        (when (or (< n-vars 2) (< n-artificial-vars 0))
          (throw (ex-info "number of (artificial) variables must be a natural number"
                          {:type :ValueError})))
        (let [n-rows (count t)
              n-cols (count (first t))
              n-stages (if (pos? n-artificial-vars) 2 1)
              n-slack (- n-cols n-vars n-artificial-vars 1)
              objectives (if (pos? n-artificial-vars) ["max" "min"] ["max"])]
          (->Tableau t
                     n-vars
                     n-artificial-vars
                     n-stages
                     n-slack
                     objectives
                     (-col-titles n-vars n-slack)
                     nil
                     nil
                     false
                     n-rows))))))

(defn Tableau-generate-col-titles
  "Generates the column titles for the tableau dimensions."
  [self]
  (-col-titles (:n-vars self) (:n-slack self)))

(defn- -find-pivot
  "Returns [row-idx col-idx stop?] for the tableau."
  [self]
  (let [t (:tableau self)
        sign (if (= "min" (peek (:objectives self))) 1 -1)
        objective-row (pop (vec (first t)))
        ;; argmax over sign-scaled entries; max-key keeps the first maximum
        col-idx (first (apply max-key (fn [[_ v]] (* sign v))
                              (map-indexed vector objective-row)))]
    (if (<= (* sign (nth objective-row col-idx)) 0)
      [0 0 true]
      (let [rhs-col (dec (count (first t)))
            quotients (mapv (fn [i]
                              (let [divisor (get-in t [i col-idx])]
                                (if (pos? divisor)
                                  (/ (get-in t [i rhs-col]) divisor)
                                  Double/NaN)))
                            (range (:n-stages self) (:n-rows self)))
            valid (vec (keep-indexed (fn [k q]
                                       (when-not (Double/isNaN q) [k q]))
                                     quotients))]
        (when (empty? valid)
          (throw (ex-info "All-NaN slice encountered" {:type :ValueError})))
        (let [[k _] (apply min-key second valid)]
          [(+ k (:n-stages self)) col-idx false])))))

(defn Tableau-find-pivot
  "Returns [row-idx col-idx] of the pivot element."
  [self]
  (let [[r c _stop?] (-find-pivot self)]
    [r c]))

(defn Tableau-pivot
  "Pivots on the element at (row-idx, col-idx).
  Returns the updated tableau matrix."
  [self row-idx col-idx]
  (let [t (:tableau self)
        scaled (mapv #(/ % (get-in t [row-idx col-idx])) (nth t row-idx))]
    (mapv (fn [i row]
            (if (= i row-idx)
              scaled
              (let [coeff (nth row col-idx)]
                (mapv (fn [a b] (- a (* coeff b))) row scaled))))
          (range)
          t)))

(defn- -advance-stage
  "Returns the tableau state after leaving the current simplex stage."
  [self]
  (let [objectives (pop (:objectives self))]
    (if (empty? objectives)
      (assoc self :objectives objectives)
      (let [t (:tableau self)
            n (:n-artificial-vars self)
            ncols (count (first t))
            drop-from (- ncols n 1)
            drop-to (dec ncols)
            kept (fn [row]
                   (vec (keep-indexed (fn [j v]
                                        (when-not (and (<= drop-from j)
                                                       (< j drop-to))
                                          v))
                                      row)))]
        (assoc self
               :objectives objectives
               :tableau (mapv kept (subvec t 1))
               :n-stages 1
               :n-rows (dec (:n-rows self))
               :n-artificial-vars 0
               :stop-iter false)))))

(defn Tableau-change-stage
  "Leaves the current stage of the two-stage method, deleting the
  artificial rows and columns. Returns the updated tableau matrix."
  [self]
  (:tableau (-advance-stage self)))

(defn Tableau-interpret-tableau
  "Reads the optimal variable values off the final tableau into a map."
  [self]
  (let [t (:tableau self)
        out {"P" (Math/abs (double (peek (first t))))}]
    (reduce (fn [out i]
              (let [col (mapv #(nth % i) t)
                    nonzero (vec (keep-indexed (fn [r v]
                                                 (when-not (zero? v) [r v]))
                                               col))]
                (if (and (= 1 (count nonzero))
                         (== 1 (second (first nonzero))))
                  (assoc out
                         (nth (:col-titles self) i)
                         (peek (nth t (first (first nonzero)))))
                  out)))
            out
            (range (:n-vars self)))))

(defn Tableau-run-simplex
  "Pivots until the objective cannot be improved, then returns the
  optimal values map."
  [self]
  (loop [state self iteration 0]
    (if (>= iteration Tableau-maxiter)
      (throw (ex-info (str "No convergence within " Tableau-maxiter
                           " iterations; may be cycling/unbounded.")
                      {:type :ValueError}))
      (if (empty? (:objectives state))
        (Tableau-interpret-tableau state)
        (let [[r c stop?] (-find-pivot state)]
          (recur (if stop?
                   (-advance-stage state)
                   (assoc state :tableau (Tableau-pivot state r c)))
                 (inc iteration)))))))
