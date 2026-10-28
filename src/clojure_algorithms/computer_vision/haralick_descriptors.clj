(ns clojure-algorithms.computer-vision.haralick-descriptors
  "Haralick texture descriptors over nested vectors.")

(defn- dmap [f x] (if (sequential? x) (mapv #(dmap f %) x) (f x)))
(defn- dmin [x] (apply min (flatten x)))
(defn- dmax [x] (apply max (flatten x)))

(defn root-mean-square-error
  "RMSE between two nested numeric structures."
  [original reference]
  (let [a (flatten original) b (flatten reference)]
    (Math/sqrt (double (/ (reduce + (map (fn [x y]
                                           (let [d (- x y)] (* d d)))
                                         a b))
                          (count a))))))

(defn normalize-image
  "Scales an image to [0, cap]."
  ([image] (normalize-image image 255.0))
  ([image cap]
   (let [lo (dmin image) diff (- (dmax image) lo)]
     (if (zero? diff)
       (dmap (constantly 0) image)
       (dmap #(* (/ (double (- % lo)) (double diff)) (double cap)) image)))))

(defn normalize-array
  "Scales values to [0, cap]."
  ([array] (normalize-array array 1))
  ([array cap]
   (let [lo (dmin array) diff (- (dmax array) lo)]
     (dmap #(* (/ (double (- % lo)) (double (if (zero? diff) 1 diff))) (double cap)) array))))

(defn grayscale
  "Converts RGB pixels to luminance bytes."
  [image]
  (mapv (fn [row]
          (mapv (fn [[r g b]]
                  (int (+ (* r 0.299) (* g 0.587) (* b 0.114))))
                row))
        image))

(defn binarize
  "Thresholds a grayscale image to 0/1."
  ([image] (binarize image 127.0))
  ([image threshold]
   (dmap #(if (> % threshold) 1 0) image)))

(defn- pad-image
  [image constant]
  (let [w (count (first image))
        border (vec (repeat (+ w 2) constant))]
    (vec (concat [border]
                 (map #(vec (concat [constant] % [constant])) image)
                 [border]))))

(defn transform
  "Applies an erosion (max) or dilation (min) filter."
  [image kind & [kernel]]
  (let [kernel (or kernel [[1 1 1] [1 1 1] [1 1 1]])
        constant (if (= kind "erosion") 1 0)
        apply-fn (if (= kind "erosion") max min)
        h (count image) w (count (first image))
        padded (pad-image image constant)]
    (mapv (fn [x]
            (mapv (fn [y]
                    (apply apply-fn
                           (for [dx (range 3) dy (range 3)
                                 :when (= 1 (get-in kernel [dx dy]))]
                             (get-in padded [(+ x dx) (+ y dy)]))))
                  (range w)))
          (range h))))

(defn opening-filter
  "Dilation followed by erosion."
  [image & [kernel]]
  (transform (transform image "dilation" kernel) "erosion" kernel))

(defn closing-filter
  "Erosion followed by dilation."
  [image & [kernel]]
  (transform (transform image "erosion" kernel) "dilation" kernel))

(defn binary-mask
  "Splits gray values into [true-mask false-mask] by a binary map."
  [image-gray image-map]
  [(mapv (fn [r m] (mapv (fn [g v] (if (= 1 v) 1 g)) r m)) image-gray image-map)
   (mapv (fn [r m] (mapv (fn [g v] (if (= 0 v) 0 g)) r m)) image-gray image-map)])

(defn matrix-concurrency
  "Computes the normalized co-occurrence matrix at an offset."
  [image [offset-x offset-y]]
  (let [size (inc (apply max (map int (flatten image))))
        h (count image) w (count (first image))
        counts (reduce (fn [acc [x y]]
                         (update-in acc [(int (get-in image [x y]))
                                         (int (get-in image [(+ x offset-x)
                                                             (+ y offset-y)]))]
                                    inc))
                       (vec (repeat size (vec (repeat size 0))))
                       (for [x (range 1 (dec h)) y (range 1 (dec w))] [x y]))
        total (reduce + (apply concat counts))]
    (mapv (fn [row] (mapv #(/ (double %) (double (if (zero? total) 1 total))) row)) counts)))

(defn haralick-descriptors
  "Computes the 8 Haralick descriptors of a co-occurrence matrix."
  [matrix]
  (let [h (count matrix) w (count (first matrix))
        cells (for [i (range h) j (range w)] [i j (get-in matrix [i j])])
        max-p (apply max (map #(nth % 2) cells))]
    [max-p
     (reduce + (map (fn [[i j v]] (* i j v)) cells))
     (reduce + (map (fn [[_ _ v]] (* v v)) cells))
     (reduce + (map (fn [[i j v]] (let [d (- i j)] (* v d d))) cells))
     (reduce + (map (fn [[i j v]] (* v (Math/abs (long (- i j))))) cells))
     (reduce + (map (fn [[i j v]] (/ (double v)
                                     (+ 1 (Math/abs (long (- i j)))))) cells))
     (reduce + (map (fn [[i j v]] (let [d (- i j)] (/ (double v) (+ 1 (* d d))))) cells))
     (- (reduce + (map (fn [[_ _ v]]
                         (if (pos? v) (* v (Math/log (double v))) 0.0))
                       cells)))]))

(defn get-descriptors
  "Concatenates Haralick descriptors for each mask."
  [masks coordinate]
  (vec (mapcat #(haralick-descriptors (matrix-concurrency % coordinate)) masks)))

(defn euclidean
  "Euclidean distance between two points."
  [point-1 point-2]
  (Math/sqrt (double (reduce + (map (fn [a b]
                                      (let [d (- a b)] (* d d)))
                                    point-1 point-2)))))

(defn get-distances
  "Ranks descriptors by normalized distance to the base index."
  [descriptors base]
  (let [dists (mapv #(euclidean [%] [(nth descriptors base)]) descriptors)
        normed (normalize-array dists 1)]
    (vec (sort-by second > (map-indexed vector normed)))))
