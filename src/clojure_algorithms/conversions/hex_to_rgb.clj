(ns clojure-algorithms.conversions.hex-to-rgb
  "Port of conversions/hex_to_rgb.py.
  Converts a hex color code to an \"rgb(r, g, b)\" string."
  (:require [clojure.string :as str]))

(defn hex-to-rgb
  "Convert hex color HEX-COLOR (\"#RGB\" or \"#RRGGBB\") to \"rgb(r, g, b)\"."
  [hex-color]
  (let [s (str/replace hex-color #"^#+" "")]
    (when-not (#{3 6} (count s))
      (throw (ex-info "Invalid hex color code" {:type :value-error})))
    (let [full (if (= 3 (count s)) (apply str (mapcat #(list % %) s)) s)]
      (when-not (re-matches #"[0-9a-fA-F]{6}" full)
        (throw (ex-info "Invalid hex color code" {:type :value-error})))
      (let [r (Integer/parseInt (subs full 0 2) 16)
            g (Integer/parseInt (subs full 2 4) 16)
            b (Integer/parseInt (subs full 4 6) 16)]
        (format "rgb(%d, %d, %d)" r g b)))))
