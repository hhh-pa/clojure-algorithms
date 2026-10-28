(ns clojure-algorithms.digital-image-processing.filters.bilateral-filter
  "Port of digital_image_processing/filters/bilateral_filter.py.
  Bilateral filter over nested vectors (values in [0, 1]).")

(defn vec-gaussian
  "Gaussian function applied element-wise (scalar or nested)."
  [img variance]
  (let [sigma (Math/sqrt variance)
        cons (/ 1 (* sigma (Math/sqrt (* 2 Math/PI))))]
    (if (sequential? img)
      (mapv #(vec-gaussian % variance) img)
      (* cons (Math/exp (* -0.5 (Math/pow (/ (double img) sigma) 2)))))))

(defn get-slice
  "KERNEL-SIZE x KERNEL-SIZE window of IMG centred at (X, Y)."
  [img x y kernel-size]
  (let [half (quot kernel-size 2)]
    (mapv (fn [i] (subvec (nth img i) (- y half) (+ y half 1)))
          (range (- x half) (+ x half 1)))))

(defn get-gauss-kernel
  "Spatial Gaussian kernel of KERNEL-SIZE with SPATIAL-VARIANCE."
  [kernel-size spatial-variance]
  (let [half (quot kernel-size 2)]
    (vec-gaussian
     (mapv (fn [i]
             (mapv (fn [j]
                     (Math/sqrt (+ (* (- i half) (- i half))
                                   (* (- j half) (- j half)))))
                   (range kernel-size)))
           (range kernel-size))
     spatial-variance)))

(defn bilateral-filter
  "Bilateral-filtered image (zero borders); returns nested vectors."
  [img spatial-variance intensity-variance kernel-size]
  (let [h (count img)
        w (count (first img))
        half (quot kernel-size 2)
        gauss-ker (get-gauss-kernel kernel-size spatial-variance)]
    (mapv (fn [i]
            (mapv (fn [j]
                    (if (or (< i half) (< j half)
                            (>= i (- h half)) (>= j (- w half)))
                      0.0
                      (let [slice (get-slice img i j kernel-size)
                            center (nth (nth slice half) half)
                            img-i (mapv (fn [row] (mapv #(- % center) row))
                                        slice)
                            img-ig (vec-gaussian img-i intensity-variance)
                            weights (mapv (fn [r1 r2] (mapv * r1 r2))
                                          gauss-ker img-ig)
                            vals (mapv (fn [r1 r2] (mapv * r1 r2))
                                       slice weights)]
                        (/ (reduce + (mapcat identity vals))
                           (reduce + (mapcat identity weights))))))
                  (range w)))
          (range h))))

(defn parse-args
  "Parse CLI-style ARGS; returns [filename spatial intensity kernel]."
  [args]
  (let [filename (if (> (count args) 1) (nth args 1) "../image_data/lena.jpg")
        spatial-variance (if (> (count args) 2)
                           (Double/parseDouble (nth args 2)) 1.0)
        intensity-variance (if (> (count args) 3)
                             (Double/parseDouble (nth args 3)) 1.0)
        kernel-size (if (> (count args) 4)
                      (let [k (Integer/parseInt (nth args 4))]
                        (+ k (Math/abs (- (mod k 2) 1))))
                      5)]
    [filename spatial-variance intensity-variance kernel-size]))
