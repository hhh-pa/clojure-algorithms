(ns clojure-algorithms.data-compression.peak-signal-to-noise-ratio
  "Port of data_compression/peak_signal_to_noise_ratio.py.
  The Python version uses numpy/cv2 over image files; this port implements
  the pure PSNR math over sequential pixel collections (nested or flat).
  The image-loading main() is omitted (requires cv2 + image files).")

(def pixel-max 255.0)

(defn peak-signal-to-noise-ratio
  "PSNR in dB between ORIGINAL and CONTRAST pixel collections."
  [original contrast]
  (let [xs (flatten original)
        ys (flatten contrast)
        n (count xs)
        mse (/ (reduce + (map (fn [a b]
                                (let [d (- (double a) (double b))] (* d d)))
                              xs ys))
               n)]
    (if (zero? mse)
      100
      (* 20 (Math/log10 (/ pixel-max (Math/sqrt mse)))))))
