(ns clojure-algorithms.data-compression.coordinate-compression
  "Port of data_compression/coordinate_compression.py.
  Compresses comparable values to dense 0-based coordinates.
  A compressor is an immutable map; use make-compressor, compress, decompress.")

(defn make-compressor
  "Build a coordinate compressor over the values in ARR."
  [arr]
  (let [sorted-arr (vec (sort arr))
        [cmap rmap _]
        (reduce (fn [[cmap rmap k] v]
                  (if (contains? cmap v)
                    [cmap rmap k]
                    [(assoc cmap v k) (assoc rmap k v) (inc k)]))
                [{} (vec (repeat (count arr) -1)) 0]
                sorted-arr)]
    {:coordinate-map cmap :reverse-map rmap}))

(defn compress
  "Compress single value ORIGINAL to its coordinate."
  [cc original]
  (if-let [[_ k] (find (:coordinate-map cc) original)]
    k
    (throw (ex-info (str "KeyError: " original) {:type :key-error}))))

(defn decompress
  "Decompress coordinate NUM back to its original value."
  [cc num]
  (let [rmap (:reverse-map cc)]
    (if (<= 0 num (dec (count rmap)))
      (nth rmap num)
      (throw (ex-info (str "compressed coordinate " num " is out of range")
                      {:type :index-error})))))
