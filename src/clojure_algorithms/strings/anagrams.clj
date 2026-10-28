(ns clojure-algorithms.strings.anagrams
  "Word-signature anagrams over a dictionary word list.
  Ported from TheAlgorithms/Python."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn signature
  "Frequency-based signature of `word`, e.g. \"test\" -> \"e1s1t2\"."
  [word]
  (apply str (mapcat (fn [[ch n]] [ch n])
                     (sort (frequencies word)))))

(defn load-word-list
  "Loads a sorted unique lowercased word list from the file at `path`."
  [path]
  (with-open [r (io/reader path)]
    (vec (sort (set (map #(str/lower-case (str/trim %))
                         (line-seq r)))))))

(defn build-index
  "Builds {signature [words]} from `word-list`."
  [word-list]
  (reduce (fn [m w] (update m (signature w) (fnil conj []) w))
          {} word-list))

(def word-by-signature
  "Lazily loaded {signature [words]} index over the bundled words.txt."
  (delay
    (if-let [url (io/resource "words.txt")]
      (build-index (load-word-list url))
      (throw (ex-info "words.txt not found on classpath" {})))))

(defn anagram
  "Returns every anagram of `word` found in the dictionary index."
  ([word] (anagram word @word-by-signature))
  ([word index]
   (vec (get index (signature word) []))))
