(ns clojure-algorithms.strings.detecting-english-programmatically
  "Heuristic English-language detection via dictionary word matches.
  Ported from TheAlgorithms/Python."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def letters-and-space
  "Characters kept by remove-non-letters."
  (set (str "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz \t\n")))

(defn load-dictionary
  "Loads the English word set from the bundled dictionary.txt."
  []
  (if-let [url (io/resource "dictionary.txt")]
    (with-open [r (io/reader url)]
      (set (line-seq r)))
    (throw (ex-info "dictionary.txt not found on classpath" {}))))

(def english-words
  "Lazily loaded dictionary word set."
  (delay (load-dictionary)))

(defn remove-non-letters
  "Strips every character of `s` that is not a letter or space."
  [s]
  (apply str (filter #(contains? letters-and-space %) s)))

(defn get-english-count
  "Fraction of words in `message` found in the dictionary."
  [message]
  (let [words (str/split (remove-non-letters (str/upper-case message))
                         #"\s+")]
    (/ (double (count (filter #(contains? @english-words %) words)))
       (count words))))

(defn is-english
  "True when `message` looks like English: at least `word-percentage` %
  dictionary words and at least `letter-percentage` % letters."
  ([message] (is-english message 20 85))
  ([message word-percentage letter-percentage]
   (let [words-match (>= (* (get-english-count message) 100) word-percentage)
         num-letters (count (remove-non-letters message))
         letters-match (>= (* (/ (double num-letters) (count message)) 100)
                           letter-percentage)]
     (and words-match letters-match))))
