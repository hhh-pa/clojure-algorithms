(ns clojure-algorithms.strings.camel-case-to-snake-case
  "camelCase/PascalCase -> snake_case conversion.
  Ported from TheAlgorithms/Python.")

(defn- upper-case? [ch] (Character/isUpperCase ^char ch))
(defn- lower-case? [ch] (Character/isLowerCase ^char ch))
(defn- digit? [ch] (Character/isDigit ^char ch))
(defn- alpha? [ch] (Character/isAlphabetic (int ch)))
(defn- alnum? [ch] (or (alpha? ch) (digit? ch)))

(defn camel-to-snake-case
  "Converts camelCase/PascalCase string `s` to snake_case.
  Throws IllegalArgumentException for non-string input."
  [s]
  (when-not (string? s)
    (throw (IllegalArgumentException.
            (str "Expected string as input, found " (type s)))))
  (if (empty? s)
    ""
    (let [chars (vec s)
          n (count chars)
          out (apply str
                     (mapcat (fn [i]
                               (let [ch (nth chars i)
                                     prev (nth chars (mod (dec i) n))]
                                 (cond
                                   (upper-case? ch)
                                   [\_ (Character/toLowerCase ^char ch)]

                                   (and (digit? prev) (lower-case? ch))
                                   [\_ ch]

                                   (and (alpha? prev) (digit? ch))
                                   [\_ ch]

                                   (not (alnum? ch))
                                   [\_]

                                   :else [ch])))
                             (range n)))]
      (if (= \_ (first out)) (subs out 1) out))))
