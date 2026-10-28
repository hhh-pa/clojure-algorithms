(ns clojure-algorithms.ciphers.base85
  "Base85 (Ascii85) encoding over UTF-8 strings."
  (:require [clojure.string :as str]))

(defn- base10-to-85
  [d]
  (loop [d d acc ""]
    (if (zero? d) acc (recur (quot d 85) (str acc (char (+ (mod d 85) 33)))))))

(defn- base85-to-10
  [digits]
  (reduce (fn [acc d] (+ (* acc 85) d)) 0 digits))

(defn- str-to-bits
  [s]
  (apply str (map #(let [b (format "%8s" (Integer/toBinaryString (int %)))]
                     (str/replace b #" " "0"))
                  (.getBytes ^String s "UTF-8"))))

(defn ascii85-encode
  "Encodes a string to Ascii85."
  [data]
  (if (empty? data)
    ""
    (let [binary (str-to-bits data)
          width (* 32 (inc (quot (count binary) 32)))
          null-values (quot (- width (count binary)) 8)
          padded (apply str (take width (concat binary (repeat \0))))
          chunks (partition 32 padded)
          result (apply str (map #(apply str (reverse (base10-to-85
                                                       (Long/parseLong (apply str %) 2))))
                                 chunks))]
      (if (zero? (mod null-values 4))
        result
        (subs result 0 (- (count result) null-values))))))

(defn ascii85-decode
  "Decodes an Ascii85 string."
  [data]
  (if (empty? data)
    ""
    (let [null-values (- (* 5 (inc (quot (count data) 5))) (count data))
          padded (str data (apply str (repeat null-values \u)))
          chunks (partition 5 padded)
          results (map (fn [chunk]
                         (let [v (base85-to-10 (map #(- (int %) 33) chunk))
                               b (format "%32s" (Long/toBinaryString v))]
                           (str/replace b #" " "0")))
                       chunks)
          result (apply str (mapcat (fn [r]
                                      (map #(char (Integer/parseInt (apply str %) 2))
                                           (partition 8 r)))
                                    results))
          offset (if (zero? (mod null-values 5)) 1 0)]
      (subs result 0 (max 0 (+ (count result) offset (- null-values)))))))
