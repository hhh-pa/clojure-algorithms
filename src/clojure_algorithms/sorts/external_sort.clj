(ns clojure-algorithms.sorts.external-sort
  "External sort: sorts a large text file with bounded memory by splitting
  it into sorted block files and k-way merging them.
  Ported from TheAlgorithms/Python."
  (:require [clojure.java.io :as io])
  (:import [java.io File]))

(defn parse-memory
  "Parses a memory size string like \"100M\", \"64K\" or \"1G\" into bytes."
  [s]
  (let [suffix (Character/toLowerCase (last s))]
    (case suffix
      \k (* (Long/parseLong (subs s 0 (dec (count s)))) 1024)
      \m (* (Long/parseLong (subs s 0 (dec (count s)))) 1024 1024)
      \g (* (Long/parseLong (subs s 0 (dec (count s)))) 1024 1024 1024)
      (Long/parseLong s))))

(defn- write-block
  "Sorts `lines` (optionally by `sort-key`) into a temp block file.
  Returns the File."
  [lines sort-key]
  (let [ordered (if sort-key (sort-by sort-key lines) (sort lines))
        f (File/createTempFile "block_" ".dat")]
    (spit f (apply str (map #(str % "\n") ordered)))
    f))

(defn- split-into-blocks
  "Splits `filename` into sorted block files of ~`block-size` bytes.
  Returns the seq of block File objects."
  [filename block-size sort-key]
  (with-open [r (io/reader filename)]
    (loop [blocks [] current [] size 0]
      (let [line (.readLine ^java.io.BufferedReader r)]
        (if (nil? line)
          (if (seq current)
            (conj blocks (write-block current sort-key))
            blocks)
          (let [current2 (conj current line)
                size2 (+ size (count line))]
            (if (>= size2 block-size)
              (recur (conj blocks (write-block current2 sort-key)) [] 0)
              (recur blocks current2 size2))))))))

(defn- k-way-merge
  "Merges sorted block `files` into `out-filename` with a k-way merge,
  comparing by `sort-key` (or natural order when nil)."
  [files out-filename sort-key]
  (let [readers (mapv #(io/reader %) files)
        kf (or sort-key identity)]
    (try
      (with-open [w (io/writer out-filename)]
        (loop [heads (mapv (fn [r] (.readLine ^java.io.BufferedReader r))
                           readers)]
          (let [live (keep-indexed (fn [i h] (when h [i h])) heads)]
            (when (seq live)
              (let [[mi mh] (reduce (fn [best [i x]]
                                      (if (neg? (compare (kf x)
                                                         (kf (second best))))
                                        [i x] best))
                                    (first live) (rest live))]
                (.write ^java.io.BufferedWriter w (str mh "\n"))
                (recur (assoc heads mi
                              (.readLine ^java.io.BufferedReader
                               (nth readers mi)))))))))
      (finally
        (doseq [r readers] (.close ^java.io.Reader r))
        (doseq [f files] (io/delete-file f true))))))

(defn external-sort
  "Sorts the lines of text file `filename` using at most ~`block-size`
  bytes of memory. Writes the sorted output to `out-filename`
  (defaults to `filename`.out). Returns the output path."
  ([filename block-size] (external-sort filename block-size nil nil))
  ([filename block-size sort-key] (external-sort filename block-size sort-key nil))
  ([filename block-size sort-key out-filename]
   (let [out (or out-filename (str filename ".out"))
         blocks (split-into-blocks filename block-size sort-key)]
     (if (empty? blocks)
       (do (spit out "") out)
       (do (k-way-merge blocks out sort-key) out)))))
