(ns clojure-algorithms.other.word-search
  "Random word-search puzzle generation in eight compass directions.

  Port of TheAlgorithms/Python `other/word_search.py`.
  Boards are immutable vectors of vectors of characters-or-nil;
  `insert-*` fns return the updated board.")

(def words ["cat" "dog" "snake" "fish"])
(def width 10)
(def height 10)

(defn make-board
  "Empty `height` x `width` board."
  [w h]
  (vec (repeat h (vec (repeat w nil)))))

(defn- try-place
  "Place `word` along direction `[dr dc]` at the first fitting
  `[row col]` candidate; return the updated board."
  [board word rows cols dr dc]
  (let [n (count word)
        h (count board)
        w (count (first board))]
    (or (some (fn [[r c]]
                (let [cells (mapv (fn [i] [(+ r (* dr i)) (+ c (* dc i))])
                                  (range n))]
                  (when (and (every? (fn [[rr cc]]
                                       (and (<= 0 rr (dec h))
                                            (<= 0 cc (dec w))
                                            (nil? (get-in board [rr cc]))))
                                     cells)
                             (seq cells))
                    (reduce (fn [b [i [rr cc]]]
                              (assoc-in b [rr cc] (nth word i)))
                            board (map-indexed vector cells)))))
              (for [r rows c cols] [r c]))
        board)))

(defn insert-north [board word rows cols] (try-place board word rows cols -1 0))
(defn insert-northeast [board word rows cols] (try-place board word rows cols -1 1))
(defn insert-east [board word rows cols] (try-place board word rows cols 0 1))
(defn insert-southeast [board word rows cols] (try-place board word rows cols 1 1))
(defn insert-south [board word rows cols] (try-place board word rows cols 1 0))
(defn insert-southwest [board word rows cols] (try-place board word rows cols 1 -1))
(defn insert-west [board word rows cols] (try-place board word rows cols 0 -1))
(defn insert-northwest [board word rows cols] (try-place board word rows cols -1 -1))

(def ^:private directions
  [insert-north insert-northeast insert-east insert-southeast
   insert-south insert-southwest insert-west insert-northwest])

(defn generate-board
  "Board with each of `word-list` placed in a random direction.
  Defaults mirror the Python module constants."
  ([word-list w h]
   (reduce (fn [board word]
             ((rand-nth directions) board word
                                    (shuffle (range h)) (shuffle (range w))))
           (make-board w h) word-list))
  ([] (generate-board words width height)))

(defn visualise
  "Render `board` as a string; `#` marks empty cells unless
  `add-fake-chars?` fills them with random letters."
  ([board] (visualise board true))
  ([board add-fake-chars?]
   (apply str
          (for [row board]
            (str (apply str
                        (for [cell row]
                          (str (cond
                                 (some? cell) cell
                                 add-fake-chars? (char (+ 97 (rand-int 26)))
                                 :else \#)
                               " ")))
                 "\n")))))
