(ns clojure-algorithms.data-structures.arrays.sudoku-solver
  "Port of data_structures/arrays/sudoku_solver.py (Norvig's solver).
  Constraint propagation + depth-first search over immutable maps of
  square -> candidate-digit string. Returns false on contradiction.
  from-file reads grids with slurp; solve-all returns a result summary.")

(def digits "123456789")
(def rows "ABCDEFGHI")
(def cols digits)

(defn cross
  "Cross product of the characters of A and B as concatenated strings."
  [items-a items-b]
  (vec (for [a items-a b items-b] (str a b))))

(def squares (cross rows cols))

(def unitlist
  (vec (concat (map #(cross rows (str %)) cols)
               (map #(cross (str %) cols) rows)
               (for [rs ["ABC" "DEF" "GHI"] cs ["123" "456" "789"]]
                 (cross rs cs)))))

(def units
  (into {} (map (fn [s] [s (vec (filter #(some #{s} %) unitlist))])) squares))

(def peers
  (into {} (map (fn [s] [s (disj (set (apply concat (units s))) s)])) squares))

(declare sudoku-assign)

(defn sudoku-eliminate
  "Eliminate digit D from VALUES[S] with propagation; false on contradiction."
  [values s d]
  (let [cur (values s)]
    (if (not (clojure.string/includes? cur (str d)))
      values
      (let [values (assoc values s (clojure.string/replace cur (str d) ""))
            cur2 (values s)]
        (cond
          (empty? cur2) false
          :else
          (let [values (if (= 1 (count cur2))
                         (let [d2 cur2]
                           (reduce (fn [vs s2]
                                     (if (false? vs) false
                                         (sudoku-eliminate vs s2 d2)))
                                   values (peers s)))
                         values)]
            (if (false? values)
              false
              (reduce (fn [vs u]
                        (if (false? vs)
                          false
                          (let [dplaces (vec (filter #(clojure.string/includes?
                                                       (vs %) (str d)) u))]
                            (cond
                              (empty? dplaces) false
                              (and (= 1 (count dplaces))
                                   (false? (sudoku-assign vs (first dplaces) d)))
                              false
                              :else vs))))
                      values (units s)))))))))

(defn sudoku-assign
  "Assign digit D to VALUES[S] by eliminating the others; false on contradiction."
  [values s d]
  (let [others (clojure.string/replace (values s) (str d) "")]
    (reduce (fn [vs d2]
              (if (false? vs) false (sudoku-eliminate vs s d2)))
            values (map str others))))

(defn grid-values
  "Map of square -> grid char for the 81-char GRID."
  [grid]
  (let [chars (vec (filter #(or (clojure.string/includes? digits (str %))
                                (#{\0 \.} %))
                           grid))]
    (assert (= 81 (count chars)))
    (zipmap squares chars)))

(defn parse-grid
  "Convert GRID to {square -> candidates}, or false on contradiction."
  [grid]
  (let [values (zipmap squares (repeat digits))]
    (reduce (fn [vs [s d]]
              (if (false? vs)
                false
                (if (clojure.string/includes? digits (str d))
                  (sudoku-assign vs s d)
                  vs)))
            values (grid-values grid))))

(defn solved?
  "True when VALUES assigns every unit a permutation of 1-9."
  [values]
  (and (not (false? values))
       (every? (fn [u] (= (set (map values u)) (set (map str digits))))
               unitlist)))

(defn- search-values [values]
  (cond
    (false? values) false
    (every? #(= 1 (count (values %))) squares) values
    :else (let [s (first (sort-by #(vector (count (values %)) %)
                                  (filter #(> (count (values %)) 1) squares)))]
            (some #(let [r (search-values
                            (sudoku-assign (into {} values) s (str %)))]
                     (when-not (false? r) r))
                  (map str (values s))))))

(defn solve
  "Solve GRID; returns {square -> digit} or false."
  [grid]
  (let [r (search-values (parse-grid grid))]
    (if (nil? r) false r)))

(defn display-grid
  "Render VALUES as a 2-D grid string."
  [values]
  (let [width (inc (apply max (map #(count (values %)) squares)))
        line (clojure.string/join "+" (repeat 3 (apply str (repeat (* width 3) \-))))]
    (clojure.string/join "\n"
                         (concat
                          (mapcat (fn [r]
                                    [(apply str (map (fn [c]
                                                       (let [v (values (str r c))
                                                             pad (apply str (repeat (- width (count v)) \ ))]
                                                         (str pad v (if (#{\3 \6} c) "|" ""))))
                                                     cols))])
                                  rows)))))

(defn from-file
  "Read FILENAME and split into grid strings on SEP."
  ([filename] (from-file filename "\n"))
  ([filename sep]
   (clojure.string/split (clojure.string/trim (slurp filename))
                         (re-pattern (java.util.regex.Pattern/quote sep)))))

(defn shuffled
  "Randomly shuffled copy of SEQ (Fisher-Yates)."
  [seq]
  (let [v (vec seq)]
    (loop [i (dec (count v)) v v]
      (if (<= i 0)
        v
        (let [j (rand-int (inc i))]
          (recur (dec i) (assoc v i (nth v j) j (nth v i))))))))

(defn random-puzzle
  "Random puzzle string with at least ASSIGNMENTS givens."
  ([] (random-puzzle 17))
  ([assignments]
   (loop []
     (let [result
           (loop [values (zipmap squares (repeat digits))
                  sqs (shuffled squares)]
             (if (empty? sqs)
               nil
               (let [s (first sqs)
                     vs (values s)
                     r (sudoku-assign values s (str (rand-nth (vec vs))))]
                 (if (false? r)
                   nil
                   (let [ds (vec (for [sq squares :when (= 1 (count (r sq)))]
                                   (r sq)))]
                     (if (and (>= (count ds) assignments)
                              (>= (count (set ds)) 8))
                       (apply str (map #(let [v (r %)]
                                          (if (= 1 (count v)) v ".")) squares))
                       (recur r (rest sqs))))))))]
       (if (nil? result) (recur) result)))))

(defn solve-all
  "Solve each grid in GRIDS; returns {:solved n :total m}."
  [grids]
  {:solved (count (filter #(solved? (solve %)) grids)) :total (count grids)})

(def grid1
  (str "003020600900305001001806400008102900700000008006708200002609500"
       "800203009005010300"))

(def grid2
  (str "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2....."
       "1.4......"))

(def hard1
  (str ".....6....59.....82....8....45........3........6..3.54...325..6...."
       ".............."))
