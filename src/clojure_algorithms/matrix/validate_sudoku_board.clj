(ns clojure-algorithms.matrix.validate-sudoku-board
  "Validates 9x9 Sudoku boards (LeetCode 36).
  https://leetcode.com/problems/valid-sudoku/")

(def NUM-SQUARES 9)

(def EMPTY-CELL ".")

(defn is-valid-sudoku-board
  "True when the filled cells of the board respect the row, column and
  3x3 box rules. The board may be valid but unsolvable."
  [sudoku-board]
  (when (or (not= NUM-SQUARES (count sudoku-board))
            (not (every? #(= NUM-SQUARES (count %)) sudoku-board)))
    (throw (ex-info (str "Sudoku boards must be " NUM-SQUARES "x"
                         NUM-SQUARES " squares.")
                    {:type :ValueError})))
  (let [cells (for [r (range NUM-SQUARES)
                    c (range NUM-SQUARES)
                    :let [v (get-in sudoku-board [r c])]
                    :when (not= EMPTY-CELL v)]
                [r c v])]
    (loop [cells cells rows #{} cols #{} boxes #{}]
      (if (empty? cells)
        true
        (let [[[r c v] & rest-cells] cells
              row-key [r v]
              col-key [c v]
              box-key [(quot r 3) (quot c 3) v]]
          (if (or (contains? rows row-key)
                  (contains? cols col-key)
                  (contains? boxes box-key))
            false
            (recur rest-cells
                   (conj rows row-key)
                   (conj cols col-key)
                   (conj boxes box-key))))))))
