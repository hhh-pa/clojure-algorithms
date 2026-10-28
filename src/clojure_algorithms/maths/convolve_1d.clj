(ns clojure-algorithms.maths.convolve-1d
  "Calculate the discrete convolution of two linear discrete sets.
  https://en.wikipedia.org/wiki/Convolution")

(defrecord Signal [signal n])

(defrecord DiscreteConvolve1D [kern sig])

(defn make-Signal
  "Constructs a Signal, a discrete representation of a signal as an
  n-dimensional vector. Every element must be numeric."
  ([] (make-Signal [] 0))
  ([signal] (make-Signal signal 0))
  ([signal n]
   (doseq [v signal]
     (when-not (number? v)
       (throw (ex-info "vector must be a list of numeric values."
                       {:type :TypeError}))))
   (->Signal (vec signal) (+ n (count signal)))))

(defn make-DiscreteConvolve1D
  "Constructs a 1D discrete convolution between two linear signals."
  ([] (make-DiscreteConvolve1D (make-Signal) (make-Signal)))
  ([kern] (make-DiscreteConvolve1D kern (make-Signal)))
  ([kern sig] (->DiscreteConvolve1D kern sig)))

(defn DiscreteConvolve1D-convolve-1d
  "Returns the 1D discrete convolution of the kernel with the signal
  as a new Signal."
  [self]
  (let [kern (:kern self)
        sig (:sig self)
        half (quot (:n kern) 2)
        sig-vec (:signal sig)
        conv (mapv (fn [i]
                     (reduce + 0.0
                             (map-indexed
                              (fn [j k]
                                (let [idx (+ i j (- half))]
                                  (if (or (neg? idx) (>= idx (:n sig)))
                                    0.0
                                    (* k (double (nth sig-vec idx))))))
                              (:signal kern))))
                   (range (:n sig)))]
    (->Signal conv (:n sig))))
