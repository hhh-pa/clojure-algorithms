(ns clojure-algorithms.project-euler.problem-054.sol1
  "Euler Problem 54: rank Texas Hold'em poker hands and count how many of the
  input hands Player 1 wins. A hand is represented as a map created by
  poker-hand; the dealing-file logic takes the lines as a parameter instead of
  reading poker_hands.txt.")

(def ^:private hand-names
  ["High card" "One pair" "Two pairs" "Three of a kind" "Straight" "Flush"
   "Full house" "Four of a kind" "Straight flush" "Royal flush"])

(def ^:private card-names
  ["" "One" "Two" "Three" "Four" "Five" "Six" "Seven" "Eight" "Nine" "Ten"
   "Jack" "Queen" "King" "Ace"])

(def ^:private value-of
  {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
   \T 10 \J 11 \Q 12 \K 13 \A 14})

(defn- five-high?
  "Returns true if the sorted-descending card values form a five-high
  (low ace) straight."
  [values]
  (= (sort values) [2 3 4 5 14]))

(defn- straight-values?
  "Returns true if the sorted-descending card values are consecutive."
  [values]
  (every? true? (map (fn [a b] (= (- a b) 1)) values (rest values))))

(defn- same-kind
  "Analyses equal-valued cards. Returns [kind first-pair second-pair] where
  kind is 7 (four of a kind), 6 (full house), 3 (three of a kind), 2 (two
  pairs), 1 (one pair) or 0."
  [values]
  (let [[kind val1 val2]
        (loop [i 0 kind 0 val1 0 val2 0]
          (if (>= i 4)
            [kind val1 val2]
            (let [a (values i) b (values (inc i))]
              (if (= a b)
                (cond
                  (zero? val1) (recur (inc i) (inc kind) a val2)
                  (= val1 a) (recur (inc i) (+ kind 2) val1 val2)
                  (zero? val2) (recur (inc i) (inc kind) val1 a)
                  (= val2 a) (recur (inc i) (+ kind 2) val1 val2)
                  :else (recur (inc i) kind val1 val2))
                (recur (inc i) kind val1 val2)))))
        kind (if (contains? #{4 5} kind) (+ kind 2) kind)
        first-pair (max val1 val2)
        second-pair (min val1 val2)
        [first-pair second-pair]
        (if (and (= kind 6) (not= 3 (count (filter #{first-pair} values))))
          [second-pair first-pair]
          [first-pair second-pair])]
    [kind first-pair second-pair]))

(defn poker-hand
  "Creates a poker hand map from a string of five cards such as
  \"6S 4C KC AS TH\". Card values are 2-9, T, J, Q, K, A followed by the suit
  S, H, D or C. Throws IllegalArgumentException for invalid input."
  [hand]
  (when (not (string? hand))
    (throw (IllegalArgumentException.
            (str "Hand should be of type 'str': " (pr-str hand)))))
  (when (not= 5 (count (.split hand " ")))
    (throw (IllegalArgumentException.
            (str "Hand should contain only 5 cards: " (pr-str hand)))))
  (let [cards (.split hand " ")
        values (vec (reverse (sort (map (fn [c] (value-of (first c))) cards))))
        suits (set (map last cards))
        flush? (= 1 (count suits))
        values (if (five-high? values)
                 (vec (concat (rest values) [(first values)]))
                 values)
        low-straight? (five-high? (sort values))
        straight? (or low-straight? (straight-values? values))
        hand-type (cond
                    (and flush? straight?)
                    (if (= 60 (reduce + values)) 23 22)
                    flush? 19
                    straight? 18
                    :else (let [[kind _ _] (same-kind values)] (+ 14 kind)))
        [kind first-pair second-pair]
        (if (#{23 22 19 18} hand-type) [0 0 0] (same-kind values))]
    {:hand hand
     :card-values values
     :card-suit suits
     :hand-type hand-type
     :first-pair first-pair
     :second-pair second-pair
     :high-card (first values)}))

(defn- compare-cards
  "Compares two hands card by card, highest first."
  [player opponent]
  (loop [pv (:card-values player) ov (:card-values opponent)]
    (cond
      (empty? pv) "Tie"
      (not= (first pv) (first ov))
      (if (> (first pv) (first ov)) "Win" "Loss")
      :else (recur (rest pv) (rest ov)))))

(defn compare-with
  "Determines the outcome of comparing the player's hand with the opponent's
  hand. Returns \"Win\", \"Loss\" or \"Tie\" according to Texas Hold'em rules."
  [player opponent]
  (cond
    (> (:hand-type player) (:hand-type opponent)) "Win"
    (< (:hand-type player) (:hand-type opponent)) "Loss"
    (= (:first-pair player) (:first-pair opponent))
    (if (= (:second-pair player) (:second-pair opponent))
      (compare-cards player opponent)
      (if (> (:second-pair player) (:second-pair opponent)) "Win" "Loss"))
    :else (if (> (:first-pair player) (:first-pair opponent)) "Win" "Loss")))

(defn hand-name
  "Returns the name of the hand in the format \"hand name, high card\"."
  [hand]
  (let [t (:hand-type hand)
        name (hand-names (- t 14))
        high (card-names (:high-card hand))
        pair1 (card-names (:first-pair hand))
        pair2 (card-names (:second-pair hand))]
    (cond
      (#{22 19 18} t) (str name ", " high "-high")
      (#{21 17 15} t) (str name ", " pair1 "s")
      (#{20 16} t) (str name ", " pair1 "s "
                        (if (= t 20) "over" "and") " " pair2 "s")
      (= t 23) name
      :else (str name ", " high))))

(defn solution
  "Returns how many hands Player 1 wins given lines, a sequence of strings
  each holding ten cards: the first five are Player 1's, the last five are
  Player 2's."
  [lines]
  (count (filter (fn [line]
                   (let [line (.trim line)
                         player (poker-hand (subs line 0 14))
                         opponent (poker-hand (subs line 15))]
                     (= "Win" (compare-with player opponent))))
                 lines)))
