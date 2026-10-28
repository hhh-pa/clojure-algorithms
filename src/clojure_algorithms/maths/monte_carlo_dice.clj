(ns clojure-algorithms.maths.monte-carlo-dice
  "Port of maths/monte_carlo_dice.py")

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

(defn- mt-randbelow
  "Return [uniform int in [0, n), next state], like random._randbelow."
  [state n]
  (let [k (.bitLength (biginteger n))]
    (loop [state state]
      (let [[r next-state] (mt-next state)
            v (unsigned-bit-shift-right r (- 32 k))]
        (if (< v n)
          [v next-state]
          (recur next-state))))))

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

(def Dice-NUM-SIDES 6)

(defrecord Dice [sides])

(defn make-Dice
  "Create a six-sided die."
  []
  (->Dice (vec (range 1 (inc Dice-NUM-SIDES)))))

(defn Dice-roll
  "Roll the die, returning one of its sides. With an rng handle the shared
  stream advances; without one a fresh unseeded generator is used."
  ([dice] (Dice-roll dice (rng nil)))
  ([dice rng-handle]
   (let [[v state] (mt-randbelow @rng-handle (count (:sides dice)))]
     (reset! rng-handle state)
     (nth (:sides dice) v))))

(defn- round-to
  "Round x to ndigits decimal places, half to even, exactly like
  Python's round() (decimal rounding of the true binary value)."
  [x ndigits]
  (.doubleValue (.setScale (BigDecimal. ^double (double x))
                           ndigits
                           java.math.RoundingMode/HALF_EVEN)))

(defn throw-dice
  "Return the probability list (percentages) of all possible sums when
  throwing num-dice dice num-throws times. The optional third argument is
  an integer or string seed for a reproducible stream, or an rng handle
  whose stream continues across calls."
  ([num-throws] (throw-dice num-throws 2 nil))
  ([num-throws num-dice] (throw-dice num-throws num-dice nil))
  ([num-throws num-dice seed-or-rng]
   (let [rng-handle (cond (or (integer? seed-or-rng) (string? seed-or-rng))
                          (rng seed-or-rng)
                          (nil? seed-or-rng) (rng nil)
                          (instance? clojure.lang.Atom seed-or-rng) seed-or-rng
                          :else (throw (ex-info (str "seed must be an integer, string, "
                                                     "nil, or rng handle")
                                                {:type :TypeError})))
         dice (vec (repeat num-dice (make-Dice)))
         counts (loop [t num-throws
                       counts (vec (repeat (inc (* num-dice Dice-NUM-SIDES)) 0))]
                  (if (zero? t)
                    counts
                    (recur (dec t)
                           (update counts
                                   (reduce + (map #(Dice-roll % rng-handle) dice))
                                   inc))))]
     (mapv (fn [c] (round-to (/ (* (double c) 100.0) (double num-throws)) 2))
           (subvec counts num-dice)))))
