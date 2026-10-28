(ns clojure-algorithms.data-compression.huffman
  "Port of data_compression/huffman.py.
  Huffman coding. The Python version reads files; this port operates on
  strings: parse-text builds letter frequencies, build-tree builds the
  Huffman tree, traverse-tree assigns bitstrings, encode-text encodes.
  Letters are {:letter ch :freq n} maps; internal nodes are
  {:freq n :left l :right r} maps.")

(defn parse-text
  "Build frequency-sorted letter maps for the chars of TEXT."
  [text]
  (->> (frequencies text)
       (map (fn [[c f]] {:letter c :freq f}))
       (sort-by :freq)
       vec))

(defn build-tree
  "Build the Huffman tree from frequency-sorted LETTERS."
  [letters]
  (loop [response (vec letters)]
    (if (<= (count response) 1)
      (first response)
      (let [[left right & rest-nodes] response
            node {:freq (+ (:freq left) (:freq right))
                  :left left :right right}]
        (recur (vec (sort-by :freq (conj (vec rest-nodes) node))))))))

(defn traverse-tree
  "Walk ROOT assigning bitstrings; returns map of char to bitstring."
  ([root] (traverse-tree root ""))
  ([root bitstring]
   (if (contains? root :letter)
     {(:letter root) bitstring}
     (merge (traverse-tree (:left root) (str bitstring "0"))
            (traverse-tree (:right root) (str bitstring "1"))))))

(defn huffman-codes
  "Map of each char in TEXT to its Huffman bitstring."
  [text]
  (traverse-tree (build-tree (parse-text text))))

(defn encode-text
  "Encode TEXT to its Huffman bitstring."
  [text]
  (let [codes (huffman-codes text)]
    (apply str (map codes text))))
