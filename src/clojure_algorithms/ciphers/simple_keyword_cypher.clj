(ns clojure-algorithms.ciphers.simple-keyword-cypher
  "Simple keyword substitution cipher."
  (:require [clojure.string :as str]))

(defn remove-duplicates
  "Drops repeated letters from a keyword (keeps spaces)."
  [key]
  (apply str (:out (reduce (fn [{:keys [seen out]} ch]
                             (if (or (= ch \space)
                                     (and (Character/isLetter ^char ch)
                                          (not (contains? seen ch))))
                               {:seen (conj seen ch) :out (conj out ch)}
                               {:seen seen :out out}))
                           {:seen #{} :out []} key))))

(defn create-cipher-map
  "Builds the substitution map for a keyword."
  [key]
  (let [alphabet (mapv #(char (+ (int \A) %)) (range 26))
        k (remove-duplicates (str/upper-case key))]
    (loop [i 0 offset (count k) m {}]
      (if (>= i 26)
        m
        (if (< i (count k))
          (recur (inc i) offset (assoc m (nth alphabet i) (nth k i)))
          (let [[ch off] (loop [off offset]
                           (let [ch (nth alphabet (- i off))]
                             (if (contains? (set k) ch)
                               (recur (dec off)) [ch off])))]
            (recur (inc i) off (assoc m (nth alphabet i) ch))))))))

(defn encipher
  "Enciphers a message with a cipher map."
  [message cipher-map]
  (apply str (map #(get cipher-map % %) (str/upper-case message))))

(defn decipher
  "Deciphers a message with a cipher map."
  [message cipher-map]
  (let [rev (zipmap (vals cipher-map) (keys cipher-map))]
    (apply str (map #(get rev % %) (str/upper-case message)))))
