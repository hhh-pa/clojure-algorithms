(ns clojure-algorithms.project-euler.problem-042.solution42
  "Euler Problem 42: by converting each letter in a word to its alphabetical
  position and summing, how many words of the input are triangle words? The
  pure logic takes the file content (or word list) as a parameter instead of
  reading words.txt.")

(def triangular-numbers
  "The first 100 triangular numbers."
  (vec (map (fn [n] (long (* 0.5 n (inc n)))) (range 1 101))))

(def ^:private triangular-set (set triangular-numbers))

(defn word-value
  "Returns the sum of the alphabetical positions (A=1..Z=26) of the letters in
  word."
  [word]
  (reduce + (map (fn [ch] (- (int ch) 64)) word)))

(defn triangle-word?
  "Returns true if the value of word is a triangular number."
  [word]
  (contains? triangular-set (word-value word)))

(defn solution
  "Returns the count of triangle words in content, the raw text of the words
  file: comma-separated double-quoted words."
  [content]
  (count (filter triangle-word?
                 (map (fn [w] (.replace w "\"" ""))
                      (.split (.trim content) ",")))))
