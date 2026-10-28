(ns clojure-algorithms.audio-filters.butterworth-filter
  "2nd-order IIR (biquad) filters with Butterworth design, following the
  RBJ Audio EQ Cookbook
  (https://webaudio.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html).
  See https://en.wikipedia.org/wiki/Butterworth_filter

  Notation: w0 = normalized angular frequency (2 pi f / samplerate),
  alpha = bandwidth parameter sin(w0) / (2 q), b0..b2 feed-forward and
  a0..a2 feed-back coefficients.")

(defn- validate-frequency
  [frequency samplerate q-factor]
  (when (or (not (integer? frequency)) (<= frequency 0))
    (throw (ex-info "frequency must be a positive integer" {})))
  (when (or (not (integer? samplerate)) (<= samplerate 0))
    (throw (ex-info "samplerate must be a positive integer" {})))
  (when (>= frequency (/ samplerate 2))
    (throw (ex-info "frequency must be less than half the samplerate" {})))
  (when (<= q-factor 0)
    (throw (ex-info "q_factor must be positive" {}))))

(def ^:private default-q (/ 1 (Math/sqrt 2)))

(defn- biquad
  "Builds the shared w0/cos/alpha terms, then applies `design` to produce
  [a-coeffs b-coeffs]. Returned as a coefficient map (no IIR dependency)."
  [frequency samplerate q-factor design]
  (validate-frequency frequency samplerate q-factor)
  (let [w0 (* 2 Math/PI (/ frequency samplerate))
        s (Math/sin w0)
        c (Math/cos w0)
        alpha (/ s (* 2 q-factor))]
    (design s c alpha)))

(defn- coeffs
  [a b]
  {:a-coeffs (vec a) :b-coeffs (vec b)})

(defn make-lowpass
  "Creates a low-pass biquad coefficient map."
  ([frequency samplerate] (make-lowpass frequency samplerate default-q))
  ([frequency samplerate q-factor]
   (biquad frequency samplerate q-factor
           (fn [_s c alpha]
             (let [b0 (/ (- 1 c) 2)
                   b1 (- 1 c)
                   a0 (+ 1 alpha)
                   a1 (* -2 c)
                   a2 (- 1 alpha)]
               (coeffs [a0 a1 a2] [b0 b1 b0]))))))

(defn make-highpass
  "Creates a high-pass biquad coefficient map."
  ([frequency samplerate] (make-highpass frequency samplerate default-q))
  ([frequency samplerate q-factor]
   (biquad frequency samplerate q-factor
           (fn [_s c alpha]
             (let [b0 (/ (+ 1 c) 2)
                   b1 (- -1 c)
                   a0 (+ 1 alpha)
                   a1 (* -2 c)
                   a2 (- 1 alpha)]
               (coeffs [a0 a1 a2] [b0 b1 b0]))))))

(defn make-bandpass
  "Creates a band-pass biquad coefficient map (constant skirt gain)."
  ([frequency samplerate] (make-bandpass frequency samplerate default-q))
  ([frequency samplerate q-factor]
   (biquad frequency samplerate q-factor
           (fn [s c alpha]
             (let [b0 (/ s 2)
                   a0 (+ 1 alpha)
                   a1 (* -2 c)
                   a2 (- 1 alpha)]
               (coeffs [a0 a1 a2] [b0 0 (- b0)]))))))

(defn make-bandpass-peak
  "Creates a band-pass biquad with constant 0 dB peak gain."
  ([frequency samplerate] (make-bandpass-peak frequency samplerate default-q))
  ([frequency samplerate q-factor]
   (biquad frequency samplerate q-factor
           (fn [_s c alpha]
             (let [a0 (+ 1 alpha)
                   a1 (* -2 c)
                   a2 (- 1 alpha)]
               (coeffs [a0 a1 a2] [alpha 0 (- alpha)]))))))

(defn make-allpass
  "Creates an all-pass biquad coefficient map."
  ([frequency samplerate] (make-allpass frequency samplerate default-q))
  ([frequency samplerate q-factor]
   (biquad frequency samplerate q-factor
           (fn [_s c alpha]
             (let [b0 (- 1 alpha)
                   b1 (* -2 c)
                   b2 (+ 1 alpha)]
               (coeffs [b2 b1 b0] [b0 b1 b2]))))))

(defn make-peak
  "Creates a peaking-EQ biquad with `gain-db` decibels of gain."
  ([frequency samplerate gain-db]
   (make-peak frequency samplerate gain-db default-q))
  ([frequency samplerate gain-db q-factor]
   (biquad frequency samplerate q-factor
           (fn [s c alpha]
             (let [big-a (Math/pow 10 (/ gain-db 40))
                   b0 (+ 1 (* alpha big-a))
                   b1 (* -2 c)
                   b2 (- 1 (* alpha big-a))
                   a0 (+ 1 (/ alpha big-a))
                   a1 (* -2 c)
                   a2 (- 1 (/ alpha big-a))]
               (coeffs [a0 a1 a2] [b0 b1 b2]))))))

(defn make-lowshelf
  "Creates a low-shelf biquad with `gain-db` decibels of gain."
  ([frequency samplerate gain-db]
   (make-lowshelf frequency samplerate gain-db default-q))
  ([frequency samplerate gain-db q-factor]
   (biquad frequency samplerate q-factor
           (fn [s c alpha]
             (let [big-a (Math/pow 10 (/ gain-db 40))
                   pmc (- (+ big-a 1) (* (- big-a 1) c))
                   ppmc (+ (+ big-a 1) (* (- big-a 1) c))
                   mpc (- (- big-a 1) (* (+ big-a 1) c))
                   pmpc (+ (- big-a 1) (* (+ big-a 1) c))
                   aa2 (* 2 (Math/sqrt big-a) alpha)
                   b0 (* big-a (+ pmc aa2))
                   b1 (* 2 big-a mpc)
                   b2 (* big-a (- pmc aa2))
                   a0 (+ ppmc aa2)
                   a1 (* -2 pmpc)
                   a2 (- ppmc aa2)]
               (coeffs [a0 a1 a2] [b0 b1 b2]))))))

(defn make-highshelf
  "Creates a high-shelf biquad with `gain-db` decibels of gain."
  ([frequency samplerate gain-db]
   (make-highshelf frequency samplerate gain-db default-q))
  ([frequency samplerate gain-db q-factor]
   (biquad frequency samplerate q-factor
           (fn [s c alpha]
             (let [big-a (Math/pow 10 (/ gain-db 40))
                   pmc (- (+ big-a 1) (* (- big-a 1) c))
                   ppmc (+ (+ big-a 1) (* (- big-a 1) c))
                   mpc (- (- big-a 1) (* (+ big-a 1) c))
                   pmpc (+ (- big-a 1) (* (+ big-a 1) c))
                   aa2 (* 2 (Math/sqrt big-a) alpha)
                   b0 (* big-a (+ ppmc aa2))
                   b1 (* -2 big-a pmpc)
                   b2 (* big-a (- ppmc aa2))
                   a0 (+ pmc aa2)
                   a1 (* 2 mpc)
                   a2 (- pmc aa2)]
               (coeffs [a0 a1 a2] [b0 b1 b2]))))))

(defn make-notch
  "Creates a notch (band-reject) biquad attenuating `frequency`.
  See https://en.wikipedia.org/wiki/Band-stop_filter"
  ([frequency samplerate] (make-notch frequency samplerate default-q))
  ([frequency samplerate q-factor]
   (let [w0 (* 2 Math/PI (/ frequency samplerate))
         s (Math/sin w0)
         c (Math/cos w0)
         alpha (/ s (* 2 q-factor))
         b0 1.0
         b1 (* -2 c)
         a0 (+ 1 alpha)
         a1 (* -2 c)
         a2 (- 1 alpha)]
     (coeffs [a0 a1 a2] [b0 b1 1.0]))))
