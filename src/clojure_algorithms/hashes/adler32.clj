(ns clojure-algorithms.hashes.adler32
  "Adler-32 checksum (Mark Adler, 1995).
  See https://en.wikipedia.org/wiki/Adler-32")

(def mod-adler 65521)

(defn adler32
  "Returns the Adler-32 checksum of plain-text as an integer."
  [plain-text]
  (let [{:keys [a b]} (reduce (fn [{:keys [a b]} ch]
                                (let [a (mod (+ a (int ch)) mod-adler)]
                                  {:a a :b (mod (+ b a) mod-adler)}))
                              {:a 1 :b 0}
                              plain-text)]
    (bit-or (bit-shift-left b 16) a)))
