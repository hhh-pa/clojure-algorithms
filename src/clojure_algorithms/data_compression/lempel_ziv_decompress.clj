(ns clojure-algorithms.data-compression.lempel-ziv-decompress
  "Port of data_compression/lempel_ziv_decompress.py.
  Lempel-Ziv-Welch decompression over bit strings, plus file helpers.")

(defn- power-of-two? [n]
  (and (pos? n) (zero? (bit-and n (dec n)))))

(defn- to-bin [n]
  (Long/toBinaryString (long n)))

(defn decompress-data
  "Decompress bit string DATA-BITS with LZW; returns a bit string."
  [data-bits]
  (loop [i 0 lexicon {"0" "0" "1" "1"} result "" curr "" index 2]
    (if (< i (count data-bits))
      (let [curr2 (str curr (nth data-bits i))]
        (if (contains? lexicon curr2)
          (let [match-id (lexicon curr2)
                lex (assoc lexicon curr2 (str match-id "0"))
                lex (if (power-of-two? index)
                      (into {} (map (fn [[k v]] [(str "0" k) v])) lex)
                      lex)
                lex (assoc lex (to-bin index) (str match-id "1"))]
            (recur (inc i) lex (str result match-id) "" (inc index)))
          (recur (inc i) lexicon result curr2 index)))
      result)))

(defn read-file-binary
  "Read file at FILE-PATH as a bit string."
  [file-path]
  (with-open [in (java.io.FileInputStream. ^String file-path)]
    (let [data (.readAllBytes in)]
      (apply str (map #(let [s (Integer/toBinaryString (bit-and % 0xFF))]
                         (str (apply str (repeat (- 8 (count s)) \0)) s))
                      data)))))

(defn write-file-binary
  "Write bit string TO-WRITE to FILE-PATH (drops the padding tail group)."
  [file-path to-write]
  (let [groups (mapv #(apply str %) (partition-all 8 to-write))
        groups (if (empty? groups)
                 []
                 (let [last-g (peek groups)]
                   (if (zero? (mod (count last-g) 8))
                     (conj groups "10000000")
                     (conj (pop groups)
                           (str last-g "1"
                                (apply str (repeat (- 8 (count last-g) 1)
                                                   \0)))))))
        groups (if (empty? groups) groups (pop groups))]
    (with-open [out (java.io.FileOutputStream. ^String file-path)]
      (doseq [g groups]
        (.write out (int (Long/parseLong g 2)))))))

(defn remove-prefix
  "Strip the Elias-gamma size prefix from bit string DATA-BITS."
  [data-bits]
  (let [counter (count (take-while #(= \0 %) data-bits))]
    (subs (subs data-bits counter) (inc counter))))

(defn decompress
  "Decompress SOURCE-PATH into DESTINATION-PATH."
  [source-path destination-path]
  (->> (read-file-binary source-path)
       remove-prefix
       decompress-data
       (write-file-binary destination-path)))
