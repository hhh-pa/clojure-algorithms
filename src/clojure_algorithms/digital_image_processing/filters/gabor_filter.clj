(ns clojure-algorithms.digital-image-processing.filters.gabor-filter
  "Port of the pure gabor_filter_kernel from
  digital_image_processing/filters/gabor_filter.py.")

(defn gabor-filter-kernel
  "KSIZE x KSIZE Gabor kernel (even KSIZE is bumped to odd)."
  [ksize sigma theta lambd gamma psi]
  (let [ksize (if (even? ksize) (inc ksize) ksize)
        half (quot ksize 2)
        rad (* theta (/ Math/PI 180))
        cos-t (Math/cos rad)
        sin-t (Math/sin rad)]
    (mapv (fn [y]
            (mapv (fn [x]
                    (let [px (- x half)
                          py (- y half)
                          gx (+ (* cos-t px) (* sin-t py))
                          gy (+ (* (- sin-t) px) (* cos-t py))]
                      (* (Math/exp (/ (- (+ (* gx gx)
                                            (* gamma gamma gy gy)))
                                      (* 2 sigma sigma)))
                         (Math/cos (+ (* 2 Math/PI (/ gx lambd)) psi)))))
                  (range ksize)))
          (range ksize))))
