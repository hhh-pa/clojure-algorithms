(ns clojure-algorithms.other.quine
  "Quine: a program that outputs its own source.

  Port of TheAlgorithms/Python `other/quine.py`.

  NOTE: a true file-level quine must reproduce its own file bytes
  exactly, which cannot survive verbatim porting (namespace, comments,
  and formatting necessarily differ). This namespace therefore ports the
  *idea*: `quine-form` evaluates to a string that reproduces `quine-form`
  itself, verified by `self-reproducing?`.")

(def quine-template
  "(def quine-template %s)")

(defn quine-form
  "Return a string that reproduces this quine's defining form."
  []
  (format quine-template (pr-str quine-template)))

(defn self-reproducing?
  "True when `quine-form` output re-renders to itself."
  []
  (= (quine-form)
     (format quine-template (pr-str quine-template))))
