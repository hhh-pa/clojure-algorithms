(ns clojure-algorithms.hashes.fletcher16
  "Fletcher-16 position-dependent checksum (John G. Fletcher).
  See https://en.wikipedia.org/wiki/Fletcher%27s_checksum")

(defn fletcher16
  "Returns the Fletcher-16 checksum of ASCII string text."
  [text]
  (let [{:keys [sum1 sum2]}
        (reduce (fn [{:keys [sum1 sum2]} b]
                  (let [sum1 (mod (+ sum1 (bit-and b 0xFF)) 255)]
                    {:sum1 sum1 :sum2 (mod (+ sum1 sum2) 255)}))
                {:sum1 0 :sum2 0}
                (.getBytes ^String text "US-ASCII"))]
    (bit-or (bit-shift-left sum2 8) sum1)))
