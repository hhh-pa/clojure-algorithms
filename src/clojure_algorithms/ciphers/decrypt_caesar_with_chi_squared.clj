(ns clojure-algorithms.ciphers.decrypt-caesar-with-chi-squared
  "Caesar decryption by chi-squared frequency analysis."
  (:require [clojure.string :as str]))

(def ^:private default-frequencies
  {\a 0.08497 \b 0.01492 \c 0.02202 \d 0.04253 \e 0.11162 \f 0.02228
   \g 0.02015 \h 0.06094 \i 0.07546 \j 0.00153 \k 0.01292 \l 0.04025
   \m 0.02406 \n 0.06749 \o 0.07507 \p 0.01929 \q 0.00095 \r 0.07587
   \s 0.06327 \t 0.09356 \u 0.02758 \v 0.00978 \w 0.02560 \x 0.00150
   \y 0.01994 \z 0.00077})

(defn decrypt-caesar-with-chi-squared
  "Returns [shift chi-squared plaintext] for the most likely Caesar shift."
  [ciphertext & {:keys [cipher-alphabet frequencies-dict case-sensitive]
                 :or {case-sensitive false}}]
  (let [alphabet (vec (or cipher-alphabet (map char (range 97 123))))
        freqs (or (when (seq frequencies-dict)
                    (into {} (map (fn [[k v]] [(first (str k)) v])
                                  frequencies-dict)))
                  default-frequencies)
        text (if case-sensitive ciphertext (str/lower-case ciphertext))
        n (count alphabet)
        score (fn [shift]
                (let [decoded
                      (apply str
                             (map (fn [ch]
                                    (let [i (.indexOf ^java.util.List alphabet
                                                      (first (str/lower-case (str ch))))]
                                      (if (neg? i)
                                        ch
                                        (let [c (nth alphabet (mod (- i shift) n))]
                                          (if (and case-sensitive
                                                   (Character/isUpperCase ^char ch))
                                            (Character/toUpperCase ^char c) c)))))
                                  text))
                      chi (reduce
                           (fn [acc ch]
                             (let [l (if case-sensitive
                                       (Character/toLowerCase ^char ch) ch)]
                               (if-let [f (get freqs (if case-sensitive
                                                       l
                                                       (Character/toLowerCase ^char ch)))]
                                 (let [hay (if case-sensitive
                                             (str/lower-case decoded) decoded)
                                       occurrences (count (filter #(= % l) hay))
                                       expected (* f occurrences)]
                                   (+ acc (/ (Math/pow (- occurrences expected) 2)
                                             expected)))
                                 acc)))
                           0.0 decoded)]
                  [shift chi decoded]))
        results (map score (range n))]
    (first (sort-by (fn [[_ chi txt]] [chi txt]) results))))
