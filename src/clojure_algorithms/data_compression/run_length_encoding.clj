(ns clojure-algorithms.data-compression.run-length-encoding
  "Port of data_compression/run_length_encoding.py.
  Run-length encoding. Runs are [char count] pairs.")

(defn run-length-encode
  "Encode TEXT to a vector of [char run-length] pairs."
  [text]
  (mapv (fn [run] [(first run) (count run)]) (partition-by identity text)))

(defn run-length-decode
  "Decode a sequence of [char run-length] pairs back to a string."
  [encoded]
  (apply str (mapcat (fn [[ch n]] (repeat n ch)) encoded)))
