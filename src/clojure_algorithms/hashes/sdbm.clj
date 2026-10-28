(ns clojure-algorithms.hashes.sdbm
  "The sdbm hashing function (hash(i) = hash(i-1) * 65599 + str[i]).
  See http://www.cse.yorku.ca/~oz/hash.html")

(defn sdbm
  "Returns the sdbm hash of plain-text (arbitrary precision)."
  [plain-text]
  (reduce (fn [h ch]
            (+' (int ch) (*' 65599 h)))
          0
          plain-text))
