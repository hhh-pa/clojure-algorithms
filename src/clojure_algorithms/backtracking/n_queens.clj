(ns clojure-algorithms.backtracking.n-queens
  "Classic N-Queens backtracking on a 2D board (1 marks a queen).
  Queens are placed row by row; only the rows above need safety checks.")

(defn safe?
  "Returns true when no queen above (`row`, `column`) attacks the cell:
  same column, left-upper or right-upper diagonal must be queen-free."
  [board row column]
  (let [n (count board)]
    (and (every? #(not= 1 (get-in board [% column])) (range row))
         (every? (fn [[i j]] (not= 1 (get-in board [i j])))
                 (map vector (range (dec row) -1 -1) (range (dec column) -1 -1)))
         (every? (fn [[i j]] (not= 1 (get-in board [i j])))
                 (map vector (range (dec row) -1 -1) (range (inc column) n))))))

(defn solve
  "Backtracking search from `row` downward. Returns the vector of solved
  boards (each a solution), without printing."
  ([board] (solve board 0))
  ([board row]
   (if (>= row (count board))
     [board]
     (vec (mapcat (fn [i]
                    (if (safe? board row i)
                      (solve (assoc-in board [row i] 1) (inc row))
                      []))
                  (range (count board)))))))

(defn printboard
  "Prints `board` with Q for queens and . for empty cells."
  [board]
  (doseq [row board]
    (println (apply str (map #(if (= 1 %) "Q " ". ") row)))))

(defn n-queens
  "Solves the n-queens problem, printing each solution. Returns the vector
  of solution boards."
  [n]
  (let [solutions (solve (vec (repeat n (vec (repeat n 0)))) 0)]
    (doseq [board solutions]
      (printboard board)
      (println ""))
    (println "The total number of solutions are:" (count solutions))
    solutions))
