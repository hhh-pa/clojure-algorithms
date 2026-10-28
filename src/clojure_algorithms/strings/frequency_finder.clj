(ns clojure-algorithms.strings.frequency-finder
  "Letter-frequency analysis for cryptanalysis.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(def english-letter-freq
  {"E" 12.70 "T" 9.06 "A" 8.17 "O" 7.51 "I" 6.97 "N" 6.75 "S" 6.33
   "H" 6.09 "R" 5.99 "D" 4.25 "L" 4.03 "C" 2.78 "U" 2.76 "M" 2.41
   "W" 2.36 "F" 2.23 "G" 2.02 "Y" 1.97 "P" 1.93 "B" 1.29 "V" 0.98
   "K" 0.77 "J" 0.15 "X" 0.15 "Q" 0.10 "Z" 0.07})

(def etaoin "ETAOINSHRDLCUMWFGYPBVKJXQZ")
(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn get-letter-count
  "Map of each A-Z letter to its count in `message` (case-insensitive)."
  [message]
  (merge (zipmap (seq letters) (repeat 0))
         (frequencies (filter #(>= (.indexOf ^String letters (int %)) 0)
                              (str/upper-case message)))))

(defn get-frequency-order
  "Letters ordered by descending frequency in `message`; ties broken by
  reverse ETAOIN order (matching the Python original)."
  [message]
  (let [counts (get-letter-count message)
        by-freq (group-by counts (seq letters))]
    (apply str (mapcat (fn [freq]
                         (->> (get by-freq freq)
                              (sort-by #(.indexOf ^String etaoin (int %)))
                              reverse))
                       (sort > (keys by-freq))))))

(defn english-freq-match-score
  "0-12 score of how closely `message` letter frequencies match English."
  [message]
  (let [freq-order (get-frequency-order message)]
    (+ (count (filter #(>= (.indexOf ^String (subs freq-order 0 6) (int %)) 0)
                      (subs etaoin 0 6)))
       (count (filter #(>= (.indexOf ^String
                            (subs freq-order (- (count freq-order) 6))
                                     (int %))
                           0)
                      (subs etaoin (- (count etaoin) 6)))))))
