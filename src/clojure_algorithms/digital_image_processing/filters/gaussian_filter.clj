(ns clojure-algorithms.digital-image-processing.filters.gaussian-filter
  "Port of digital_image_processing/filters/gaussian_filter.py.
  Valid (shrinking) convolution with a Gaussian kernel over
  nested vectors; outputs are truncated to int (uint8 range inputs).")

(defn gen-gaussian-kernel
  "K-SIZE x K-SIZE Gaussian kernel with SIGMA."
  [k-size sigma]
  (let [center (quot k-size 2)]
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [px (- x center)
                          py (- y center)]
                      (* (/ 1 (* 2 Math/PI sigma))
                         (Math/exp (/ (- (+ (* px px) (* py py)))
                                      (* 2 sigma sigma))))))
                  (range k-size)))
          (range k-size))))

(defn gaussian-filter
  "Gaussian-blurred image (valid convolution, int-truncated)."
  [image k-size sigma]
  (let [h (count image)
        w (count (first image))
        dh (- h k-size -1)
        dw (- w k-size -1)
        kernel (gen-gaussian-kernel k-size sigma)]
    (mapv (fn [i]
            (mapv (fn [j]
                    (int (reduce + (for [ki (range k-size) kj (range k-size)]
                                     (* (nth (nth image (+ i ki)) (+ j kj))
                                        (nth (nth kernel ki) kj))))))
                  (range dw)))
          (range dh))))
