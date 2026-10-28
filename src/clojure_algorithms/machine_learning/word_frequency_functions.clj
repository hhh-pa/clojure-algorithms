(ns clojure-algorithms.machine-learning.word-frequency-functions
  "Word frequency statistics for information retrieval: term frequency,
  document frequency and TF-IDF term weighting.

  Reference: https://en.wikipedia.org/wiki/Tf%E2%80%93idf"
  (:require [clojure.string :as str]))

(def ^:private punctuation
  "Matches Python's string.punctuation, stripped by the upstream code."
  (set "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

(defn- strip-punctuation
  [s]
  (str/escape s (zipmap punctuation (repeat ""))))

(defn- round-to
  "Round x to ndigits decimal places with round-half-even (matches Python round)."
  [x ndigits]
  (let [scale (Math/pow 10.0 (double ndigits))]
    (/ (Math/rint (* (double x) scale)) scale)))

(defn term-frequency
  "Count case-insensitive occurrences of term in document.
  Punctuation is removed and newlines are deleted before splitting on spaces.

  >>> (term-frequency \"to\" \"To be, or not to be\")
  2"
  [term document]
  (let [words (str/split (str/replace (strip-punctuation document) "\n" "") #" " -1)
        target (str/lower-case term)]
    (count (filter #(= (str/lower-case %) target) words))))

(defn document-frequency
  "Count how many newline-separated documents of corpus contain term
  (case-insensitive substring match). Returns [hits total].

  >>> (document-frequency \"first\" \"This is the first...\\n...\")
  [1 3]"
  [term corpus]
  (let [docs (str/split (strip-punctuation (str/lower-case corpus)) #"\n" -1)
        target (str/lower-case term)]
    [(count (filter #(str/includes? % target) docs)) (count docs)]))

(defn inverse-document-frequency
  "log10(N/df), or the smoothed 1+log10(N/(1+df)) when smoothing is true.

  Throws :ValueError when N is 0 (log10(0) is undefined) and
  :ZeroDivisionError when df is 0 without smoothing."
  ([df n] (inverse-document-frequency df n false))
  ([df n smoothing]
   (if smoothing
     (do (when (zero? n)
           (throw (ex-info "log10(0) is undefined." {:type :ValueError})))
         (round-to (+ 1 (Math/log10 (/ (double n) (+ 1 df)))) 3))
     (do (when (zero? df)
           (throw (ex-info "df must be > 0" {:type :ZeroDivisionError})))
         (when (zero? n)
           (throw (ex-info "log10(0) is undefined." {:type :ValueError})))
         (round-to (Math/log10 (/ (double n) (double df))) 3)))))

(defn tf-idf
  "Term originality: tf * idf, rounded to 3 places.

  >>> (tf-idf 2 0.477)
  0.954"
  [tf idf]
  (round-to (* (double tf) (double idf)) 3))
