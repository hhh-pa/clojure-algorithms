(ns clojure-algorithms.conversions.rgb-cmyk-conversion
  "Port of conversions/rgb_cmyk_conversion.py.
  Converts RGB colors to CMYK percentages.")

(defn rgb-to-cmyk
  "Convert RGB (0-255 each) to [c m y k] percentages."
  [r-input g-input b-input]
  (when-not (and (integer? r-input) (integer? g-input) (integer? b-input)
                 (not-any? boolean? [r-input g-input b-input]))
    (throw (ex-info (str "Expected int, found ("
                         (type r-input) ", " (type g-input) ", "
                         (type b-input) ")")
                    {:type :value-error})))
  (when-not (and (<= 0 r-input 255) (<= 0 g-input 255) (<= 0 b-input 255))
    (throw (ex-info "Expected int of the range 0..255" {:type :value-error})))
  (let [r (/ r-input 255.0) g (/ g-input 255.0) b (/ b-input 255.0)
        k (- 1.0 (max r g b))]
    (if (== k 1)
      [0 0 0 100]
      [(long (Math/rint (* 100 (/ (- 1 r k) (- 1 k)))))
       (long (Math/rint (* 100 (/ (- 1 g k) (- 1 k)))))
       (long (Math/rint (* 100 (/ (- 1 b k) (- 1 k)))))
       (long (Math/rint (* 100 k)))])))
