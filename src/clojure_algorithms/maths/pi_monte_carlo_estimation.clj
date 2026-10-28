(ns clojure-algorithms.maths.pi-monte-carlo-estimation
  "Port of maths/pi_monte_carlo_estimation.py")

;; Mersenne Twister (MT19937) compatible with CPython's random module:
;; integer seeds use init_by_array, matching random.Random(seed) streams.

(def ^:private mt-n 624)
(def ^:private mt-m 397)
(def ^:private mt-matrix-a 0x9908B0DF)
(def ^:private mt-upper-mask 0x80000000)
(def ^:private mt-lower-mask 0x7FFFFFFF)
(def ^:private mt-mask32 0xFFFFFFFF)

(defn- mt-init-genrand
  "Seed the state array from a single 32-bit value (init_genrand)."
  [seed]
  (loop [i 1
         mt (assoc (vec (repeat mt-n 0)) 0 (bit-and seed mt-mask32))]
    (if (>= i mt-n)
      mt
      (let [prev (nth mt (dec i))]
        (recur (inc i)
               (assoc mt i (bit-and (+ (* 1812433253
                                          (bit-xor prev
                                                   (unsigned-bit-shift-right prev 30)))
                                       i)
                                    mt-mask32)))))))

(defn- mt-init-by-array
  "Seed the state array from a vector of 32-bit words (init_by_array)."
  [init-key]
  (let [key-length (count init-key)]
    (loop [i 1
           j 0
           k (max mt-n key-length)
           mt (mt-init-genrand 19650218)
           first-phase true]
      (if (zero? k)
        (if first-phase
          (recur i j (dec mt-n) mt false)
          (assoc mt 0 0x80000000))
        (let [prev (nth mt (dec i))
              mixed (bit-xor prev (unsigned-bit-shift-right prev 30))
              v (if first-phase
                  (+ (bit-xor (nth mt i) (* mixed 1664525))
                     (nth init-key j)
                     j)
                  (- (bit-xor (nth mt i) (* mixed 1566083941)) i))
              mt-next (assoc mt i (bit-and v mt-mask32))
              [i-next mt-next] (if (>= (inc i) mt-n)
                                 [1 (assoc mt-next 0 (nth mt-next (dec mt-n)))]
                                 [(inc i) mt-next])
              j-next (if first-phase
                       (let [jj (inc j)]
                         (if (>= jj key-length) 0 jj))
                       j)]
          (recur i-next j-next (dec k) mt-next first-phase))))))

(defn- seed-key
  "Split a seed into little-endian 32-bit words (CPython version-2 seeding:
  strings seed from their UTF-8 bytes mixed with their SHA-512 digest)."
  [seed]
  (let [n (if (string? seed)
            (let [bs (.getBytes ^String seed "UTF-8")
                  md (java.security.MessageDigest/getInstance "SHA-512")]
              (bigint (BigInteger. 1 (byte-array (concat (seq bs)
                                                         (seq (.digest md bs)))))))
            (bigint seed))
        n (if (neg? n) (+ n (bigint (.shiftLeft BigInteger/ONE 64))) n)]
    (if (zero? n)
      [0]
      (loop [s n
             words []]
        (if (zero? s)
          words
          (recur (quot s 0x100000000)
                 (conj words (long (rem s 0x100000000)))))))))

(defn- mt-twist
  "Generate the next 624 words of state."
  [mt]
  (loop [i 0
         mt mt]
    (if (>= i mt-n)
      mt
      (let [y (bit-or (bit-and (nth mt i) mt-upper-mask)
                      (bit-and (nth mt (mod (inc i) mt-n)) mt-lower-mask))
            v (bit-xor (nth mt (mod (+ i mt-m) mt-n))
                       (unsigned-bit-shift-right y 1)
                       (if (odd? y) mt-matrix-a 0))]
        (recur (inc i) (assoc mt i (bit-and v mt-mask32)))))))

(defn- mt-next
  "Return [uint32 output, next state] for the generator state map."
  [{:keys [mt idx]}]
  (let [[mt idx] (if (>= idx mt-n) [(mt-twist mt) 0] [mt idx])
        y0 (nth mt idx)
        y1 (bit-xor y0 (unsigned-bit-shift-right y0 11))
        y2 (bit-xor y1 (bit-and (bit-shift-left y1 7) 0x9D2C5680))
        y3 (bit-xor y2 (bit-and (bit-shift-left y2 15) 0xEFC60000))
        y4 (bit-xor y3 (unsigned-bit-shift-right y3 18))]
    [(bit-and y4 mt-mask32) {:mt mt :idx (inc idx)}]))

(defn- mt-random-double
  "Return [uniform double in [0, 1), next state], like random.random()."
  [state]
  (let [[a s1] (mt-next state)
        [b s2] (mt-next s1)]
    [(/ (+ (* (double (unsigned-bit-shift-right a 5)) 67108864.0)
           (double (unsigned-bit-shift-right b 6)))
        9007199254740992.0)
     s2]))

(defn rng
  "Create a random number generator handle (an atom of generator state).
  Integer and string seeds reproduce CPython random.Random(seed) streams;
  a nil seed picks a non-deterministic starting point."
  ([] (rng nil))
  ([seed]
   (when-not (or (nil? seed) (integer? seed) (string? seed))
     (throw (ex-info "The only supported seed types are: nil, int, and str"
                     {:type :TypeError})))
   (atom {:mt (mt-init-by-array (seed-key (if (nil? seed)
                                            (System/nanoTime)
                                            seed)))
          :idx mt-n})))

(defrecord Point [x y])

(defn make-Point
  "Create a point with coordinates x and y."
  [x y]
  (->Point x y))

(defn Point-is-in-unit-circle
  "True when the point lies in the unit circle, false otherwise."
  [point]
  (<= (+ (* (:x point) (:x point)) (* (:y point) (:y point))) 1))

(defn Point-random-unit-square
  "Generate a point randomly drawn from the unit square [0, 1) x [0, 1),
  advancing the given rng handle's stream."
  [rng-handle]
  (let [[x s1] (mt-random-double @rng-handle)
        [y s2] (mt-random-double s1)]
    (reset! rng-handle s2)
    (->Point x y)))

(defn estimate-pi
  "Estimate pi with number-of-simulations Monte Carlo simulations: draw
  points uniformly from the unit square and return 4 * m / n where m is
  the number of points landing in the unit circle. The optional seed is
  an integer or string for a reproducible stream, or an rng handle whose
  stream continues across calls."
  ([number-of-simulations] (estimate-pi number-of-simulations nil))
  ([number-of-simulations seed]
   (when (< number-of-simulations 1)
     (throw (ex-info "At least one simulation is necessary to estimate PI."
                     {:type :ValueError})))
   (let [rng-handle (cond (or (integer? seed) (string? seed)) (rng seed)
                          (nil? seed) (rng nil)
                          (instance? clojure.lang.Atom seed) seed
                          :else (throw (ex-info (str "seed must be an integer, "
                                                     "string, nil, or rng handle")
                                                {:type :TypeError})))
         in-circle (loop [i number-of-simulations
                          in-circle 0
                          state @rng-handle]
                     (if (zero? i)
                       (do (reset! rng-handle state)
                           in-circle)
                       (let [[x s1] (mt-random-double state)
                             [y s2] (mt-random-double s1)]
                         (recur (dec i)
                                (if (<= (+ (* x x) (* y y)) 1)
                                  (inc in-circle)
                                  in-circle)
                                s2))))]
     (/ (* 4.0 in-circle) number-of-simulations))))
