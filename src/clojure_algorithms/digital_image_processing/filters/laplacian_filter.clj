(ns clojure-algorithms.digital-image-processing.filters.laplacian-filter
  "Port of digital_image_processing/filters/laplacian_filter.py.
  Laplacian via correlation with reflect_101 border and a (0, 0)
  anchor, matching the cv2.filter2D call.")

(def kernels
  {1 [[0 -1 0] [-1 4 -1] [0 -1 0]]
   3 [[0 1 0] [1 -4 1] [0 1 0]]
   5 [[0 0 -1 0 0]
      [0 -1 -2 -1 0]
      [-1 -2 16 -2 -1]
      [0 -1 -2 -1 0]
      [0 0 -1 0 0]]
   7 [[0 0 0 -1 0 0 0]
      [0 0 -2 -3 -2 0 0]
      [0 -2 -7 -10 -7 -2 0]
      [-1 -3 -10 68 -10 -3 -1]
      [0 -2 -7 -10 -7 -2 0]
      [0 0 -2 -3 -2 0 0]
      [0 0 0 -1 0 0 0]]})

(defn- reflect-101 [i n]
  (loop [i i]
    (cond
      (< i 0) (recur (- i))
      (>= i n) (recur (- (* 2 n) 2 i))
      :else i)))

(defn my-laplacian
  "Laplacian of SRC with kernel size KSIZE (1, 3, 5 or 7)."
  [src ksize]
  (when-not (contains? kernels ksize)
    (throw (ex-info (str "ksize must be in " (pr-str (sort (keys kernels))))
                    {:type :value-error})))
  (let [kernel (kernels ksize)
        kh (count kernel)
        kw (count (first kernel))
        h (count src)
        w (count (first src))
        getv (fn [i j]
               (nth (nth src (reflect-101 i h)) (reflect-101 j w)))]
    (mapv (fn [i]
            (mapv (fn [j]
                    (reduce + (for [ki (range kh) kj (range kw)]
                                (* (nth (nth kernel ki) kj)
                                   (getv (+ i ki) (+ j kj))))))
                  (range w)))
          (range h))))
