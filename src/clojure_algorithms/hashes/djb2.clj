(ns clojure-algorithms.hashes.djb2
  "Dan Bernstein's djb2 string hash (magic constants 33 and 5381).
  See http://www.cse.yorku.ca/~oz/hash.html")

(defn djb2
  "Returns the 32-bit djb2 hash of string s."
  [s]
  (bit-and (reduce (fn [h ch]
                     (unchecked-add (unchecked-add (bit-shift-left h 5) h)
                                    (long (int ch))))
                   (long 5381)
                   s)
           0xFFFFFFFF))
