(ns clojure-algorithms.maths.entropy
  "Implementation of entropy of information
  https://en.wikipedia.org/wiki/Entropy_(information_theory)")

(def ^:private log2
  "2-based logarithm."
  (let [ln2 (Math/log 2)]
    (fn [x] (/ (Math/log x) ln2))))

(defn- round-half-even
  "Rounds `x` to an integer with round-half-even, matching Python's round()."
  [x]
  (long (.setScale (BigDecimal. ^double (double x)) 0 java.math.RoundingMode/HALF_EVEN)))

(def ^:private alphabet
  "Characters whose frequencies contribute to the entropy: space plus a-z."
  (map str (cons \space (map char (range (int \a) (inc (int \z)))))))

(defn analyze-text
  "Converts `text` into a pair [single-char-counts two-char-counts] of
  frequency maps. The first maps each character to its count; the second
  maps each two-character string (plus a leading space paired with the
  first character) to its count."
  [text]
  (when (empty? text)
    (throw (ex-info "string index out of range" {:type :IndexError})))
  (let [chars (mapv str text)
        singles (frequencies chars)
        pairs (mapv str (map str (butlast chars) (rest chars)))
        twos (update (frequencies pairs) (str " " (first chars)) (fnil inc 0))]
    [singles twos]))

(defn- shannon-entropy
  "Shannon entropy (in bits) of the frequency map `freqs` over `keys-seq`."
  [freqs keys-seq]
  (let [total (reduce + (vals freqs))]
    (- (reduce + 0.0
               (for [k keys-seq :when (contains? freqs k)]
                 (let [p (/ (double (get freqs k)) total)]
                   (* p (log2 p))))))))

(defn calculate-prob
  "Prints three lines: the entropy of information based on single
  characters, based on two-character sequences, and the difference
  between them."
  [text]
  (let [[singles twos] (analyze-text text)
        h1 (shannon-entropy singles alphabet)
        pairs (for [a alphabet b alphabet] (str a b))
        h2 (shannon-entropy twos pairs)]
    (println (format "%.1f" (double (round-half-even h1))))
    (println (format "%.1f" (double (round-half-even h2))))
    (println (format "%.1f" (double (round-half-even (- h2 h1)))))))

(defn main
  "Upstream main only runs the doctest suite, which has no Clojure
  equivalent; kept as a stub so the public var still exists."
  [& _args]
  (throw (ex-info "main only runs the Python doctest suite" {:type :UnsupportedOperation})))
