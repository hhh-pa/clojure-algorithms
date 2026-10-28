(ns clojure-algorithms.backtracking.word-search
  "Word Search (LeetCode 86-ish grid search): does `word` appear in the
  character grid via horizontal/vertical steps that never reuse a cell?
  See https://leetcode.com/problems/word-search/")

(defn get-point-key
  "Hash key identifying cell (`row`, `column`) of a grid."
  [len-board len-board-column row column]
  (+ (* len-board len-board-column row) column))

(defn validate-board-and-word
  "Throws unless `board` is a non-empty matrix of non-empty strings and
  `word` is a non-empty string."
  [board word]
  (let [board-msg "The board should be a non-empty matrix of single-character strings."]
    (when (or (not (sequential? board)) (empty? board))
      (throw (ex-info board-msg {})))
    (doseq [row board]
      (when (or (not (sequential? row)) (empty? row))
        (throw (ex-info board-msg {})))
      (doseq [item row]
        (when (or (not (string? item)) (empty? item))
          (throw (ex-info board-msg {})))))
    (when (or (not (string? word)) (empty? word))
      (throw (ex-info "The word parameter should be a string of length greater than 0." {})))))

(defn exits-word
  "Returns true when the suffix of `word` from `word-index` can be spelled
  starting at (`row`, `column`), avoiding `visited-points-set`."
  [board word row column word-index visited-points-set]
  (if (not= (get-in board [row column]) (str (nth word word-index)))
    false
    (if (= word-index (dec (count word)))
      true
      (let [len-board (count board)
            len-board-column (count (first board))]
        (boolean
         (some (fn [[dr dc]]
                 (let [next-i (+ row dr)
                       next-j (+ column dc)]
                   (when (and (<= 0 next-i (dec len-board))
                              (<= 0 next-j (dec len-board-column)))
                     (let [key (get-point-key len-board len-board-column next-i next-j)]
                       (when (not (contains? visited-points-set key))
                         (exits-word board word next-i next-j (inc word-index)
                                     (conj visited-points-set key)))))))
               [[0 1] [0 -1] [-1 0] [1 0]]))))))

(defn- find-path-from
  [board word rows cols r c index path visited]
  (when (= (get-in board [r c]) (str (nth word index)))
    (let [path (conj path [r c])
          visited (conj visited [r c])]
      (if (= index (dec (count word)))
        path
        (some (fn [[dr dc]]
                (let [nr (+ r dr), nc (+ c dc)]
                  (when (and (<= 0 nr (dec rows)) (<= 0 nc (dec cols))
                             (not (contains? visited [nr nc])))
                    (find-path-from board word rows cols nr nc (inc index)
                                    path visited))))
              [[0 1] [0 -1] [1 0] [-1 0]])))))

(defn get-word-path
  "Returns the cell path spelling `word` in `board`, or nil when absent."
  [board word]
  (validate-board-and-word board word)
  (let [rows (count board)
        cols (count (first board))]
    (some (fn [[i j]] (find-path-from board word rows cols i j 0 [] #{}))
          (for [i (range rows) j (range cols)] [i j]))))

(defn word-exists
  "Returns true when `word` can be spelled in `board`."
  [board word]
  (validate-board-and-word board word)
  (let [len-board (count board)
        len-board-column (count (first board))]
    (boolean
     (some (fn [[i j]]
             (exits-word board word i j 0
                         #{(get-point-key len-board len-board-column i j)}))
           (for [i (range len-board) j (range len-board-column)] [i j])))))
