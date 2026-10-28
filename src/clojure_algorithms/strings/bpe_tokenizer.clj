(ns clojure-algorithms.strings.bpe-tokenizer
  "Byte-Pair Encoding subword tokenizer.
  Ported from TheAlgorithms/Python.")

(defn get-byte-pair-counts
  "Counts of consecutive id pairs in `ids`."
  [ids]
  (frequencies (partition 2 1 ids)))

(defn merge-pair
  "Replaces every occurrence of pair `p` in `ids` with new id `idx`."
  [ids p idx]
  (loop [xs (seq ids) acc []]
    (cond
      (nil? xs) acc
      (and (next xs)
           (= (first xs) (first p))
           (= (second xs) (second p)))
      (recur (nnext xs) (conj acc idx))
      :else
      (recur (next xs) (conj acc (first xs))))))

(defn make-tokenizer
  "Creates a tokenizer state map with `num-merges` merge steps."
  ([] (make-tokenizer 20))
  ([num-merges] {:num-merges num-merges :merges (array-map)}))

(defn encode
  "Encodes `text` to token ids, learning merges. Returns
  {:ids [...] :merges {pair idx} :tokenizer state}."
  ([text] (encode (make-tokenizer) text))
  ([tokenizer text]
   (let [tokens (vec (.getBytes ^String text "UTF-8"))
         tokens (mapv #(bit-and % 0xFF) tokens)
         max-merges (dec (count tokens))
         num-merges (min (:num-merges tokenizer) (max max-merges 0))]
     (loop [i 0 ids tokens merges (array-map)]
       (if (>= i num-merges)
         {:ids ids :merges merges
          :tokenizer (assoc tokenizer :merges merges)}
         (let [counts (get-byte-pair-counts ids)
               pair (apply max-key counts (keys counts))]
           (if (= 1 (get counts pair))
             (recur (inc i) ids merges)
             (let [idx (+ 256 i)]
               (recur (inc i) (merge-pair ids pair idx)
                      (assoc merges pair idx))))))))))

(defn decode
  "Decodes token `ids` with `merges` back to a string."
  ([ids] (decode ids {}))
  ([ids merges]
   (let [vocab (into {} (map (fn [i] [i (byte-array [(unchecked-byte i)])])
                             (range 256)))
         vocab2 (reduce (fn [v [[p0 p1] idx]]
                          (assoc v idx (byte-array (concat (seq (get v p0))
                                                           (seq (get v p1))))))
                        vocab merges)
         bytes (byte-array (mapcat #(seq (get vocab2 %)) ids))]
     (String. ^bytes bytes "UTF-8"))))
