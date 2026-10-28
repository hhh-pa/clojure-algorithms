(ns clojure-algorithms.strings.string-switch-case
  "Convert phrases between pascal/camel/snake/kebab case.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn split-input
  "Splits `s` on special characters, then on whitespace."
  [s]
  (mapv (fn [part]
          (filterv (complement str/blank?) (str/split part #"\s+")))
        (str/split s #"[^ a-zA-Z0-9\s]")))

(defn- capitalize-word
  [w]
  (if (empty? w) w
      (str (Character/toUpperCase ^char (first w))
           (str/lower-case (subs w 1)))))

(defn to-simple-case
  "Capitalizes each word of `s` and merges them (PascalCase)."
  [s]
  (apply str (mapcat #(map capitalize-word %) (split-input s))))

(defn to-complex-case
  "Joins the words of `text` with `separator`, upper- or lower-cased."
  [text upper separator]
  (apply str (map #(str/join separator
                             (map (if upper str/upper-case str/lower-case) %))
                  (split-input text))))

(defn to-pascal-case
  [text]
  (to-simple-case text))

(defn to-camel-case
  [text]
  (let [pascal (to-simple-case text)]
    (if (empty? pascal)
      "not valid string"
      (str (Character/toLowerCase ^char (first pascal)) (subs pascal 1)))))

(defn to-snake-case
  [text upper]
  (to-complex-case text upper "_"))

(defn to-kebab-case
  [text upper]
  (to-complex-case text upper "-"))
