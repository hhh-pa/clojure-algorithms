(ns clojure-algorithms.matrix.matrix-based-game
  "Matrix-based game: remove connected groups of equal elements, apply
  gravity, shift columns left and score each move."
  (:require [clojure.string :as str]))

(defn validate-matrix-size
  "Throws unless size is a positive integer."
  [size]
  (when (or (not (integer? size)) (<= size 0))
    (throw (ex-info "Matrix size must be a positive integer."
                    {:type :ValueError}))))

(defn validate-matrix-content
  "Throws unless matrix has size rows of size alphanumeric characters."
  [matrix size]
  (println matrix)
  (when (not= (count matrix) size)
    (throw (ex-info "The matrix dont match with size." {:type :ValueError})))
  (doseq [row matrix]
    (when (not= (count row) size)
      (throw (ex-info (str "Each row in the matrix must have exactly "
                           size " characters.")
                      {:type :ValueError})))
    (when-not (every? #(Character/isLetterOrDigit ^char %) row)
      (throw (ex-info "Matrix rows can only contain letters and numbers."
                      {:type :ValueError})))))

(defn validate-moves
  "Throws when any move falls outside the size x size board."
  [moves size]
  (doseq [[x y] moves]
    (when-not (and (<= 0 x (dec size)) (<= 0 y (dec size)))
      (throw (ex-info "Move is out of bounds for a matrix."
                      {:type :ValueError})))))

(defn parse-moves
  "Parses a comma-separated string of \"x y\" pairs into move vectors."
  [input-str]
  (mapv (fn [pair]
          (let [parts (str/split (str/trim pair) #"\s+")]
            (when (not= 2 (count parts))
              (throw (ex-info "Each move must have exactly two numbers."
                              {:type :ValueError})))
            (mapv (fn [part]
                    (try
                      (Long/parseLong part)
                      (catch NumberFormatException _
                        (throw (ex-info (str "invalid literal for int() with base 10: '"
                                             part "'")
                                        {:type :ValueError})))))
                  parts)))
        (str/split input-str #",")))

(defn find-repeat
  "Returns the set of [row col] positions connected to (row, column)
  holding the same element. The y coordinate counts up from the bottom."
  [matrix-g row column size]
  (let [column (- (dec size) column)
        color (get-in matrix-g [column row])]
    (if (= "-" color)
      #{}
      (letfn [(dfs [rn cn visited repeated]
                (if (or (neg? rn) (>= rn size)
                        (neg? cn) (>= cn size)
                        (contains? visited [rn cn]))
                  [visited repeated]
                  (let [visited (conj visited [rn cn])]
                    (if (= (get-in matrix-g [rn cn]) color)
                      (let [[v1 r1] (dfs (dec rn) cn visited (conj repeated [rn cn]))
                            [v2 r2] (dfs (inc rn) cn v1 r1)
                            [v3 r3] (dfs rn (dec cn) v2 r2)]
                        (dfs rn (inc cn) v3 r3))
                      [visited repeated]))))]
        (second (dfs column row #{} #{}))))))

(defn increment-score
  "Scores a move removing count elements: 1 + 2 + ... + count."
  [count-v]
  (quot (* count-v (inc count-v)) 2))

(defn move-x
  "Lets the elements of column fall down, keeping their order.
  Returns the updated matrix."
  [matrix-g column size]
  (let [m (mapv vec matrix-g)
        fallen (reduce (fn [acc r]
                         (let [v (get-in m [r column])]
                           (if (= "-" v)
                             (vec (cons v acc))
                             (conj acc v))))
                       []
                       (range size))]
    (reduce (fn [m r] (assoc-in m [r column] (nth fallen r)))
            m
            (range size))))

(defn move-y
  "Shifts columns left over entirely empty ones, padding with \"-\".
  Returns the updated matrix."
  [matrix-g size]
  (let [m (mapv vec matrix-g)
        kept (filterv (fn [c]
                        (not (every? #(= "-" (get-in m [% c]))
                                     (range size))))
                      (range size))
        padding (vec (repeat (- size (count kept)) "-"))]
    (mapv (fn [r row]
            (if (< r size)
              (vec (concat (mapv #(nth row %) kept) padding))
              row))
          (range)
          m)))

(defn play
  "Removes the group at (pos-x, pos-y), applies gravity and returns
  [updated-matrix score]."
  [matrix-g pos-x pos-y size]
  (let [m (mapv vec matrix-g)
        same (find-repeat m pos-x pos-y size)]
    (if (empty? same)
      [m (increment-score 0)]
      (let [cleared (reduce (fn [m [r c]] (assoc-in m [r c] "-")) m same)
            fallen (reduce (fn [m c] (move-x m c size)) cleared (range size))]
        [(move-y fallen size) (increment-score (count same))]))))

(defn process-game
  "Plays every move in moves on matrix and returns the total score."
  [size matrix moves]
  (second
   (reduce (fn [[game total] [pos-x pos-y]]
             (let [[game score] (play game pos-x pos-y size)]
               [game (+ total score)]))
           [(mapv #(mapv str %) matrix) 0]
           moves)))
