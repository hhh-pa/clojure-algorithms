(ns clojure-algorithms.computer-vision.gramian
  "Gram matrix style features over nested channel vectors.")

(defn gram-matrix
  "Returns the CxC Gram matrix of a CxHxW feature map."
  [mat]
  (let [c (count mat)
        h (count (first mat))
        w (count (ffirst mat))
        flat (mapv #(vec (apply concat %)) mat)
        n (* c h w)]
    (mapv (fn [a]
            (mapv (fn [b] (/ (double (reduce + (map * a b))) n)) flat))
          flat)))

(defn gram-loss
  "Squared Frobenius norm between two feature maps' Gram matrices."
  [input-features reference-features]
  (let [a (gram-matrix input-features)
        b (gram-matrix reference-features)]
    (reduce + (map (fn [ra rb]
                     (reduce + (map (fn [x y]
                                      (let [d (- x y)] (* d d)))
                                    ra rb)))
                   a b))))
