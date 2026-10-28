(ns clojure-algorithms.data-compression.lz77
  "Port of data_compression/lz77.py.
  LZ77 sliding-window compression.
  Tokens are {:offset o :length l :indicator ch} maps.")

(defrecord Token [offset length indicator])

(defn make-compressor
  "Build an LZ77 compressor config map."
  ([] (make-compressor 13 6))
  ([window-size lookahead-buffer-size]
   {:window-size window-size
    :lookahead-buffer-size lookahead-buffer-size
    :search-buffer-size (- window-size lookahead-buffer-size)}))

(defn token->str
  "Render TOKEN as \"(offset, length, indicator)\"."
  [token]
  (str "(" (:offset token) ", " (:length token) ", "
       (:indicator token) ")"))

(defn match-length-from-index
  "Longest match of TEXT from TEXT-INDEX against WINDOW from WINDOW-INDEX."
  [_compressor text window text-index window-index]
  (if (or (empty? text) (not= (nth text text-index) (nth window window-index)))
    0
    (inc (match-length-from-index _compressor text
                                  (str window (nth text text-index))
                                  (inc text-index) (inc window-index)))))

(defn find-encoding-token
  "Find the encoding token for TEXT given SEARCH-BUFFER."
  [compressor text search-buffer]
  (when (empty? text)
    (throw (ex-info "We need some text to work with." {:type :value-error})))
  (if (empty? search-buffer)
    (->Token 0 0 (nth text 0))
    (let [[offset length]
          (reduce (fn [[offset length] [i ch]]
                    (if (= ch (nth text 0))
                      (let [found-offset (- (count search-buffer) i)
                            found-length (match-length-from-index
                                          compressor text search-buffer 0 i)]
                        (if (>= found-length length)
                          [found-offset found-length]
                          [offset length]))
                      [offset length]))
                  [0 0] (map-indexed vector search-buffer))]
      (->Token offset length (nth text length)))))

(defn compress
  "Compress TEXT to a vector of tokens."
  [compressor text]
  (loop [text text search-buffer "" output []]
    (if (empty? text)
      output
      (let [token (find-encoding-token compressor text search-buffer)
            grown (str search-buffer (subs text 0 (inc (:length token))))
            sbsize (:search-buffer-size compressor)
            search-buffer (if (> (count grown) sbsize)
                            (subs grown (- (count grown) sbsize))
                            grown)]
        (recur (subs text (inc (:length token))) search-buffer
               (conj output token))))))

(defn decompress
  "Decompress a sequence of TOKENS back to a string."
  [_compressor tokens]
  (reduce (fn [output token]
            (let [output (reduce (fn [o _]
                                   (str o (nth o (- (count o)
                                                    (:offset token)))))
                                 output (range (:length token)))]
              (str output (:indicator token))))
          "" tokens))
