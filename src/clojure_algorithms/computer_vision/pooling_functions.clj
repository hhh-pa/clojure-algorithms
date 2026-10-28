(ns clojure-algorithms.computer-vision.pooling-functions
  "Max and average pooling over square matrices.")

(defn- check-square
  [arr]
  (let [a (vec (map vec arr))]
    (when-not (= (count a) (count (first a)))
      (throw (IllegalArgumentException. "The input array is not a square matrix")))
    a))

(defn- pool
  [arr size stride f]
  (let [a (check-square arr)
        n (count a)
        out-n (inc (quot (- n size) stride))]
    (mapv (fn [mi]
            (mapv (fn [mj]
                    (f (for [di (range size) dj (range size)]
                         (get-in a [(+ (* mi stride) di) (+ (* mj stride) dj)]))))
                  (range out-n)))
          (range out-n))))

(defn maxpooling
  "Max-pools a square matrix."
  [arr size stride]
  (pool arr size stride (partial apply max)))

(defn avgpooling
  "Average-pools a square matrix (truncated to whole numbers)."
  [arr size stride]
  (pool arr size stride (fn [vs] (double (long (/ (reduce + vs) (count vs)))))))
