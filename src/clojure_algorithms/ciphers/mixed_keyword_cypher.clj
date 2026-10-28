(ns clojure-algorithms.ciphers.mixed-keyword-cypher
  "Mixed keyword substitution cipher."
  (:require [clojure.string :as str]))

(defn mixed-keyword
  "Encrypts plaintext with a columnar keyword alphabet."
  ([keyword plaintext] (mixed-keyword keyword plaintext false))
  ([keyword plaintext verbose] (mixed-keyword keyword plaintext verbose "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  ([keyword plaintext verbose alphabet]
   (let [kw (str/upper-case keyword)
         plain (str/upper-case plaintext)
         alpha-set (set alphabet)
         unique-chars (vec (distinct (filter alpha-set kw)))
         n (count unique-chars)
         shifted (vec (concat unique-chars (remove (set unique-chars) alphabet)))
         rows (mapv vec (partition n n [] shifted))
         mapping (loop [col 0 idx 0 m {}]
                   (if (>= col n)
                     m
                     (recur (inc col)
                            (+ idx (count (filter #(> (count %) col) rows)))
                            (into m (comp (filter #(> (count %) col))
                                          (map-indexed (fn [k row] [(nth alphabet (+ idx k))
                                                                    (nth row col)])))
                                  rows))))]
     (when verbose (println mapping))
     (apply str (map #(get mapping % %) plain)))))
