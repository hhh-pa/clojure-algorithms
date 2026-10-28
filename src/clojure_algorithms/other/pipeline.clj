(ns clojure-algorithms.other.pipeline
  "Unix-pipe-style function pipeline.

  Port of TheAlgorithms/Python `other/pipeline.py`.
  A pipeline is a vector of single-arg fns; `pipe` appends a stage and
  `run` threads a value through every stage.")

(defn make-pipeline
  "Create an empty pipeline."
  []
  [])

(defn pipe
  "Append stage `f` to `pipeline`, returning the extended pipeline."
  [pipeline f]
  (conj (vec pipeline) f))

(defn run
  "Thread `input` through each stage of `pipeline` left to right."
  [pipeline input]
  (reduce (fn [value f] (f value)) input pipeline))

(defn pipeline-fn
  "Return a function applying `pipeline` to its argument."
  [pipeline]
  (fn [input] (run pipeline input)))
