(ns clojure-algorithms.data-compression.lempel-ziv
  "Port of data_compression/lempel_ziv.py.
  Lempel-Ziv-Welch compression over bit strings, plus file helpers.")

(defn- power-of-two? [n]
  (and (pos? n) (zero? (bit-and n (dec n)))))

(defn- to-bin [n]
  (Long/toBinaryString (long n)))

(defn add-key-to-lexicon
  "Add CURR-STRING extensions to LEXICON; returns [new-lexicon]."
  [lexicon curr-string index last-match-id]
  (let [lex (-> lexicon
                (dissoc curr-string)
                (assoc (str curr-string "0") last-match-id))
        lex (if (power-of-two? index)
              (into {} (map (fn [[k v]] [k (str "0" v)])) lex)
              lex)]
    (assoc lex (str curr-string "1") (to-bin index))))

(defn compress-data
  "Compress bit string DATA-BITS with LZW; returns a bit string."
  [data-bits]
  (loop [i 0 lexicon {"0" "0" "1" "1"} result "" curr "" index 2]
    (if (< i (count data-bits))
      (let [curr2 (str curr (nth data-bits i))]
        (if (contains? lexicon curr2)
          (let [match-id (lexicon curr2)]
            (recur (inc i) (add-key-to-lexicon lexicon curr2 index match-id)
                   (str result match-id) "" (inc index)))
          (recur (inc i) lexicon result curr2 index)))
      (let [tail (loop [c curr]
                   (if (or (empty? c) (contains? lexicon c)) c (recur (str c "0"))))]
        (if (empty? tail) result (str result (lexicon tail)))))))

(defn read-file-binary
  "Read file at FILE-PATH as a bit string."
  [file-path]
  (with-open [in (java.io.FileInputStream. ^String file-path)]
    (let [data (.readAllBytes in)]
      (apply str (map #(let [s (Integer/toBinaryString (bit-and % 0xFF))]
                         (str (apply str (repeat (- 8 (count s)) \0)) s))
                      data)))))

(defn add-file-length
  "Prefix COMPRESSED with the Elias-gamma-coded size of SOURCE-PATH."
  [source-path compressed]
  (let [file-length (.length (java.io.File. ^String source-path))
        len-bin (to-bin file-length)
        len-len (count len-bin)]
    (str (apply str (repeat (dec len-len) \0)) len-bin compressed)))

(defn write-file-binary
  "Write bit string TO-WRITE to FILE-PATH, padding the tail with 1 then 0s."
  [file-path to-write]
  (let [groups (mapv #(apply str %) (partition-all 8 to-write))
        groups (if (empty? groups)
                 ["10000000"]
                 (if (zero? (mod (count (peek groups)) 8))
                   (conj groups "10000000")
                   (let [last-g (peek groups)]
                     (conj (pop groups)
                           (str last-g "1"
                                (apply str (repeat (- 8 (count last-g) 1) \0)))))))]
    (with-open [out (java.io.FileOutputStream. ^String file-path)]
      (doseq [g groups]
        (.write out (int (Long/parseLong g 2)))))))

(defn compress
  "Compress SOURCE-PATH into DESTINATION-PATH."
  [source-path destination-path]
  (let [data-bits (read-file-binary source-path)
        compressed (compress-data data-bits)]
    (write-file-binary destination-path
                       (add-file-length source-path compressed))))
