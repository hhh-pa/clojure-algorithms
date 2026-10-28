(ns clojure-algorithms.strings.dna
  "DNA strand complement. Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(def ^:private complement-base {\A \T \T \A \C \G \G \C})

(defn dna
  "Returns the complementary DNA strand. Throws IllegalArgumentException
  for strands with characters outside ATCG."
  [strand]
  (when-not (re-matches #"[ATCG]*" strand)
    (throw (IllegalArgumentException. "Invalid Strand")))
  (str/escape strand complement-base))
