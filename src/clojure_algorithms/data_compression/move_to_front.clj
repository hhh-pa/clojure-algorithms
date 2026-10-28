(ns clojure-algorithms.data-compression.move-to-front
  "Port of data_compression/move_to_front.py.
  Move-to-front transform encode/decode.")

(defn- validated-alphabet
  "Alphabet string as a vector after checking char uniqueness."
  [alphabet]
  (when-not (string? alphabet)
    (throw (ex-info "alphabet must be a string" {:type :type-error})))
  (when-not (= (count (set alphabet)) (count alphabet))
    (throw (ex-info "alphabet must contain unique characters"
                    {:type :value-error})))
  (vec alphabet))

(defn move-to-front-encode
  "Encode TEXT with the move-to-front transform over ALPHABET."
  [text alphabet]
  (when-not (string? text)
    (throw (ex-info "text must be a string" {:type :type-error})))
  (let [symbols (validated-alphabet alphabet)]
    (loop [chars (seq text) syms symbols out []]
      (if (empty? chars)
        out
        (let [ch (first chars)
              idx (.indexOf ^java.util.List (into [] syms) ch)]
          (when (neg? idx)
            (throw (ex-info (str "character '" ch "' is not in the alphabet")
                            {:type :value-error})))
          (recur (rest chars)
                 (vec (cons ch (concat (subvec syms 0 idx)
                                       (subvec syms (inc idx)))))
                 (conj out idx)))))))

(defn move-to-front-decode
  "Decode index vector ENCODED-TEXT with the move-to-front transform."
  [encoded-text alphabet]
  (let [symbols (validated-alphabet alphabet)]
    (loop [idxs (seq encoded-text) syms symbols out []]
      (if (empty? idxs)
        (apply str out)
        (let [i (first idxs)]
          (when-not (<= 0 i (dec (count syms)))
            (throw (ex-info (str "index " i " is not valid for alphabet size "
                                 (count syms))
                            {:type :value-error})))
          (recur (rest idxs)
                 (vec (cons (nth syms i) (concat (subvec syms 0 i)
                                                 (subvec syms (inc i)))))
                 (conj out (nth syms i))))))))
