(ns clojure-algorithms.strings.is-pangram
  "Pangram checks in three flavours. Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(def default-sentence "The quick brown fox jumps over the lazy dog")

(defn is-pangram
  "True when `s` contains every letter a-z at least once."
  ([] (is-pangram default-sentence))
  ([s]
   (= 26 (count (set (filter (fn [ch] (re-matches #"[a-z]" (str ch)))
                             (str/lower-case (str/replace s " " ""))))))))

(defn is-pangram-faster
  "Flag-array pangram check."
  ([] (is-pangram-faster default-sentence))
  ([s]
   (let [flags (boolean-array 26)]
     (doseq [ch s]
       (cond
         (Character/isLowerCase ^char ch)
         (aset flags (- (int ch) 97) true)
         (Character/isUpperCase ^char ch)
         (aset flags (- (int ch) 65) true)))
     (every? true? flags))))

(defn is-pangram-fastest
  "Set-comprehension pangram check."
  ([] (is-pangram-fastest default-sentence))
  ([s]
   (= 26 (count (set (filter #(Character/isAlphabetic (int %))
                             (str/lower-case s)))))))

(defn benchmark
  "Prints rough timings of the three variants."
  []
  (doseq [[name f] [["is-pangram" is-pangram]
                    ["is-pangram-faster" is-pangram-faster]
                    ["is-pangram-fastest" is-pangram-fastest]]]
    (let [t0 (System/nanoTime)]
      (dotimes [_ 100000] (f))
      (println name (/ (- (System/nanoTime) t0) 1e9) "s for 100k runs"))))
