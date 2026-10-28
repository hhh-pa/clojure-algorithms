(ns clojure-algorithms.fractals.barnsley-fern)

(def transformations
  "Each [a b c d e f] defines x' = a*x + b*y + e, y' = c*x + d*y + f."
  [[0.00 0.00 0.00 0.16 0.00 0.00]
   [0.85 0.04 -0.04 0.85 0.00 1.60]
   [0.20 -0.26 0.23 0.22 0.00 1.60]
   [-0.15 0.28 0.26 0.24 0.00 0.44]])

(def cumulative-probabilities [0.01 0.86 0.93 1.00])

(defn transform
  "Apply affine transformation index to point [x y]."
  [[x y] index]
  (when (not (<= 0 index (dec (count transformations))))
    (throw (ex-info (str "index must be in range 0..3, got " index) {:index index})))
  (let [[a b c d e f] (nth transformations index)
        round12 #(/ (Math/round (* 1e12 (double %))) 1e12)]
    [(round12 (+ (* a x) (* b y) e))
     (round12 (+ (* c x) (* d y) f))]))

(defn choose-transformation
  "Map a sample from [0, 1) to a transformation index."
  [sample]
  (or (first (keep-indexed (fn [i threshold] (when (< sample threshold) i))
                           cumulative-probabilities))
      (dec (count cumulative-probabilities))))

(defn generate-fern
  "Generate iterations points of the Barnsley fern from (0, 0).
  An optional seed makes the output reproducible."
  ([iterations] (generate-fern iterations nil))
  ([iterations seed]
   (when (<= iterations 0)
     (throw (ex-info (str "iterations must be positive, got " iterations) {})))
   (let [rng (if (some? seed) (java.util.Random. (long seed)) (java.util.Random.))]
     (loop [n (dec iterations) point [0.0 0.0] points [[0.0 0.0]]]
       (if (zero? n)
         points
         (let [point (transform point (choose-transformation (.nextDouble rng)))]
           (recur (dec n) point (conj points point))))))))

;; Note: the Python module's matplotlib plotting (run only under __main__)
;; is intentionally not ported; generate-fern returns the point cloud data.
