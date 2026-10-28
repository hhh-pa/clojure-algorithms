(ns clojure-algorithms.strings.palindrome
  "Palindrome checks in several flavours.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(def test-data
  {"MALAYALAM" true "String" false "rotor" true "level" true "A" true
   "BB" true "ABC" false "amanaplanacanalpanama" true "abcdba" false
   "AB" false})

(defn is-palindrome
  "Two-pointer palindrome check."
  [s]
  (loop [i 0 j (dec (count s))]
    (cond
      (>= i j) true
      (not= (nth s i) (nth s j)) false
      :else (recur (inc i) (dec j)))))

(defn is-palindrome-traversal
  "Palindrome check traversing half the string."
  [s]
  (let [n (count s)]
    (every? (fn [i] (= (nth s i) (nth s (- n i 1))))
            (range (quot n 2)))))

(defn is-palindrome-recursive
  "Recursive palindrome check."
  [s]
  (if (<= (count s) 1)
    true
    (and (= (first s) (peek (vec s)))
         (is-palindrome-recursive (subs s 1 (dec (count s)))))))

(defn is-palindrome-slice
  "Palindrome check via string reversal."
  [s]
  (= s (str/reverse s)))

(defn is-palindrome-ignore-case-and-spaces
  "Palindrome check ignoring case, spaces and punctuation."
  [s]
  (let [clean (apply str (map #(Character/toLowerCase ^char %)
                              (filter #(Character/isLetterOrDigit (int %))
                                      s)))]
    (= clean (str/reverse clean))))

(def test-data-ignore-case-and-spaces
  {"A man a plan a canal Panama" true
   "Was it a car or a cat I saw?" true
   "Hello World" false
   "Never Odd or Even" true
   "" true})

(defn benchmark-function
  "Times 500k runs of `f` over test-data and prints it."
  [f fname]
  (let [t0 (System/nanoTime)]
    (dotimes [_ 500000]
      (doseq [[k v] test-data] (= (f k) v)))
    (println (format "%-35s finished 500,000 runs in %.5f seconds"
                     fname (/ (- (System/nanoTime) t0) 1e9)))))
