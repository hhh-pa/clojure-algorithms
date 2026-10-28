(ns clojure-algorithms.strings.snake-case-to-camel-pascal-case
  "snake_case -> camelCase/PascalCase conversion.
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(defn snake-to-camel-case
  "Converts snake_case `s` to camelCase, or PascalCase when
  `use-pascal` is true. Throws IllegalArgumentException on bad types."
  ([s] (snake-to-camel-case s false))
  ([s use-pascal]
   (when-not (string? s)
     (throw (IllegalArgumentException.
             (str "Expected string as input, found " (type s)))))
   (when-not (boolean? use-pascal)
     (throw (IllegalArgumentException.
             (str "Expected boolean as use_pascal parameter, found "
                  (type use-pascal)))))
   (let [words (str/split s #"_")
         cap (fn [w] (if (empty? w) w
                         (str (Character/toUpperCase ^char (first w))
                              (subs w 1))))]
     (if use-pascal
       (apply str (map cap words))
       (str (first words) (apply str (map cap (rest words))))))))
