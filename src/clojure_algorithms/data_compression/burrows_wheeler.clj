(ns clojure-algorithms.data-compression.burrows-wheeler
  "Port of data_compression/burrows_wheeler.py.
  Burrows-Wheeler transform and its inverse.")

(defn all-rotations
  "Return the vector of all len(S) rotations of string S."
  [s]
  (when-not (string? s)
    (throw (ex-info "The parameter s type must be str." {:type :type-error})))
  (mapv (fn [i] (str (subs s i) (subs s 0 i))) (range (count s))))

(defn bwt-transform
  "Burrows-Wheeler transform of S.
  Returns {:bwt-string ... :idx-original-string ...}."
  [s]
  (when-not (string? s)
    (throw (ex-info "The parameter s type must be str." {:type :type-error})))
  (when (empty? s)
    (throw (ex-info "The parameter s must not be empty."
                    {:type :value-error})))
  (let [rotations (vec (sort (all-rotations s)))]
    {:bwt-string (apply str (map #(nth % (dec (count %))) rotations))
     :idx-original-string (.indexOf rotations s)}))

(defn- coerce-idx [idx]
  (cond
    (integer? idx) idx
    (float? idx) (int idx)
    (string? idx) (try (Long/parseLong idx)
                       (catch NumberFormatException _
                         (throw (ex-info (str "The parameter idx_original_string "
                                              "type must be int or passive of "
                                              "cast to int.")
                                         {:type :type-error}))))
    :else (throw (ex-info (str "The parameter idx_original_string type must "
                               "be int or passive of cast to int.")
                          {:type :type-error}))))

(defn reverse-bwt
  "Inverse Burrows-Wheeler transform of BWT-STRING with original index IDX."
  [bwt-string idx-original-string]
  (when-not (string? bwt-string)
    (throw (ex-info "The parameter bwt_string type must be str."
                    {:type :type-error})))
  (when (empty? bwt-string)
    (throw (ex-info "The parameter bwt_string must not be empty."
                    {:type :value-error})))
  (let [idx (coerce-idx idx-original-string)]
    (when (neg? idx)
      (throw (ex-info "The parameter idx_original_string must not be lower than 0."
                      {:type :value-error})))
    (when (>= idx (count bwt-string))
      (throw (ex-info (str "The parameter idx_original_string must be lower "
                           "than len(bwt_string).")
                      {:type :value-error})))
    (loop [rows (vec (repeat (count bwt-string) "")) n (count bwt-string)]
      (if (zero? n)
        (nth rows idx)
        (recur (vec (sort (mapv str bwt-string rows))) (dec n))))))
