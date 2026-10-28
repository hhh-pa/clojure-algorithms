(ns clojure-algorithms.backtracking.n-queens-math
  "N-Queens solved with slope math: queens share a right diagonal (/) when
  row - col matches, a left diagonal (\\) when row + col matches, and a
  column when the column repeats. A board is stored as a vector whose
  i-th entry is the queen's column in row i.")

(defn- render-board
  [possible-board n]
  (mapv (fn [i] (str (apply str (repeat i ". ")) "Q " (apply str (repeat (- n 1 i) ". "))))
        possible-board))

(defn depth-first-search
  "DFS over rows, threading column/diagonal collision sets. Returns the
  vector of rendered solution boards extending `possible-board`."
  [possible-board diagonal-right-collisions diagonal-left-collisions n]
  (let [row (count possible-board)]
    (if (= row n)
      [(render-board possible-board n)]
      (vec (mapcat (fn [col]
                     (if (or (contains? (set possible-board) col)
                             (contains? diagonal-right-collisions (- row col))
                             (contains? diagonal-left-collisions (+ row col)))
                       []
                       (depth-first-search (conj possible-board col)
                                           (conj diagonal-right-collisions (- row col))
                                           (conj diagonal-left-collisions (+ row col))
                                           n)))
                   (range n))))))

(defn n-queens-solutions
  "Returns all rendered n-queens solution boards for an n x n chessboard."
  [n]
  (depth-first-search [] #{} #{} n))

(defn n-queens-solution
  "Prints every solution board for an n x n chessboard, then the solution
  count. Returns the solution boards."
  [n]
  (let [boards (n-queens-solutions n)]
    (doseq [board boards]
      (doseq [column board]
        (println column))
      (println ""))
    (println (count boards) "solutions were found.")
    boards))
