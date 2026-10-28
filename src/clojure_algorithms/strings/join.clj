(ns clojure-algorithms.strings.join
  "Join a list of strings with a separator.
  Ported from TheAlgorithms/Python."
  (:refer-clojure :exclude [join])
  (:require [clojure.string :as str]))

(defn join
  "Joins string collection `parts` with `separator`.
  Throws Exception when any element is not a string."
  [separator parts]
  (let [v (if (string? parts) [parts] (vec parts))]
    (doseq [p v]
      (when-not (string? p)
        (throw (Exception. "join() accepts only strings"))))
    (str/join separator v)))
