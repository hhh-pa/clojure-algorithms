(ns clojure-algorithms.computer-vision.horn-schunck
  "Horn-Schunck optical flow over nested vectors.")

(defn- emap2 [f a b] (mapv (fn [ra rb] (mapv f ra rb)) a b))
(defn- emap1 [f a] (mapv #(mapv f %) a))
(defn- esum [& ms] (reduce (partial emap2 +) ms))
(defn- ediv [a b] (emap2 / a b))
(defn- emul [a b] (emap2 * a b))

(defn- reflect-index
  [i n]
  (cond (< i 0) (- (- i) 1)
        (>= i n) (- (+ (* 2 n) 1) i 2)
        :else i))

(defn convolve-reflect
  "Convolves image with kernel using reflected borders."
  [image kernel]
  (let [h (count image) w (count (first image))
        kh (count kernel) kw (count (first kernel))
        oh (quot kh 2) ow (quot kw 2)]
    (mapv (fn [r]
            (mapv (fn [c]
                    (reduce + (for [kr (range kh) kc (range kw)]
                                (* (get-in kernel [kr kc])
                                   (get-in image [(reflect-index (+ r kr (- oh)) h)
                                                  (reflect-index (+ c kc (- ow)) w)])))))
                  (range w)))
          (range h))))

(defn warp
  "Warps image pixels by horizontal/vertical flows; invalid pixels are 0."
  [image horizontal-flow vertical-flow]
  (let [h (count image) w (count (first image))]
    (mapv (fn [r]
            (mapv (fn [c]
                    (let [sr (Math/round (double (- r (get-in vertical-flow [r c]))))
                          sc (Math/round (double (- c (get-in horizontal-flow [r c]))))]
                      (if (or (< sr 0) (< sc 0) (>= sr h) (>= sc w))
                        0
                        (get-in image [sr sc]))))
                  (range w)))
          (range h))))

(def ^:private kernel-x [[-0.25 0.25] [-0.25 0.25]])
(def ^:private kernel-y [[-0.25 -0.25] [0.25 0.25]])
(def ^:private kernel-t [[0.25 0.25] [0.25 0.25]])
(def ^:private kernel-laplacian
  [[(/ 1 12) (/ 1 6) (/ 1 12)] [(/ 1 6) 0 (/ 1 6)] [(/ 1 12) (/ 1 6) (/ 1 12)]])

(defn horn-schunck
  "Estimates [horizontal-flow vertical-flow] between two frames."
  [image0 image1 num-iter & [alpha]]
  (let [alpha (or alpha 0.1)
        h (count image0) w (count (first image0))
        zeros (vec (repeat h (vec (repeat w 0.0))))]
    (loop [hf zeros vf zeros iter 0]
      (if (>= iter num-iter)
        [hf vf]
        (let [warped (warp image0 hf vf)
              dx (esum (convolve-reflect warped kernel-x)
                       (convolve-reflect image1 kernel-x))
              dy (esum (convolve-reflect warped kernel-y)
                       (convolve-reflect image1 kernel-y))
              dt (emap2 + (convolve-reflect warped kernel-t)
                        (emap1 - (convolve-reflect image1 kernel-t)))
              ahf (convolve-reflect hf kernel-laplacian)
              avf (convolve-reflect vf kernel-laplacian)
              num (esum (emul dx ahf) (emul dy avf) dt)
              den (esum (emap1 (constantly (* alpha alpha)) dx)
                        (emul dx dx) (emul dy dy))
              update (ediv num den)]
          (recur (emap2 - ahf (emul dx update))
                 (emap2 - avf (emul dy update))
                 (inc iter)))))))
