(ns clojure-algorithms.machine-learning.mfcc
  "Mel Frequency Cepstral Coefficients from an audio signal.

  Signals are vectors of doubles; FFT results are rows of [real imag] pairs.
  The discrete Fourier transform is computed directly (O(n^2)), which is
  exact and dependency-free. Only the WAV-file demo needs scipy and is
  honestly stubbed.

  References:
  https://en.wikipedia.org/wiki/Mel-frequency_cepstrum
  https://web.stanford.edu/~jurafsky/slp3/")

(defn- unsupported!
  [needs]
  (throw (ex-info (str "requires " needs) {:type :UnsupportedOperation})))

(defn- linspace
  "num samples from start to stop, inclusive (like np.linspace)."
  [start stop num]
  (cond (<= num 0) []
        (= num 1) [(double start)]
        :else (let [step (/ (- (double stop) (double start)) (dec num))]
                (mapv #(+ (double start) (* step %)) (range num)))))

(defn normalize
  "Scale audio by its maximum absolute value.
  Throws :ValueError on empty input."
  [audio]
  (when (empty? audio)
    (throw (ex-info "audio must not be empty" {:type :ValueError})))
  (let [peak (apply max (map #(Math/abs (double %)) audio))]
    (mapv #(/ (double %) peak) audio)))

(defn audio-frames
  "Split audio into overlapping frames of ftt-size samples every hop-length
  milliseconds (reflect-padded by half a window at both ends)."
  ([audio sample-rate] (audio-frames audio sample-rate 20 1024))
  ([audio sample-rate hop-length] (audio-frames audio sample-rate hop-length 1024))
  ([audio sample-rate hop-length ftt-size]
   (let [hop-size (int (Math/rint (/ (* (double sample-rate) hop-length) 1000.0)))
         width (int (/ ftt-size 2))
         audio (vec audio)
         n (count audio)
         padded (vec (concat (map #(nth audio (- width %)) (range width))
                             audio
                             (map #(nth audio (- (- n 2) %)) (range width))))
         frame-count (inc (quot (- (count padded) ftt-size) hop-size))]
     (mapv #(subvec padded (* % hop-size) (+ (* % hop-size) ftt-size))
           (range frame-count)))))

(defn- dft-row
  "First (inc (quot ftt-size 2)) DFT bins of a real row, as [re im] pairs."
  [row ftt-size]
  (let [n (count row)
        xs (double-array (map double row))
        two-pi-over-n (/ (* 2.0 Math/PI) n)
        bins (min n (inc (quot ftt-size 2)))]
    (mapv (fn [k]
            (loop [i 0
                   re 0.0
                   im 0.0]
              (if (>= i n)
                [re im]
                (let [angle (* two-pi-over-n k i)]
                  (recur (inc i)
                         (+ re (* (aget xs i) (Math/cos angle)))
                         (- im (* (aget xs i) (Math/sin angle))))))))
          (range bins))))

(defn calculate-fft
  "DFT of every windowed row (FFT length = row length), keeping the first
  (inc (quot ftt-size 2)) bins."
  ([audio-windowed] (calculate-fft audio-windowed 1024))
  ([audio-windowed ftt-size]
   (mapv #(dft-row % ftt-size) audio-windowed)))

(defn- complex-pair?
  [x]
  (and (sequential? x) (= 2 (count x)) (every? number? x)))

(defn calculate-signal-power
  "Squared magnitudes of FFT coefficients (flat or nested rows)."
  [audio-fft]
  (letfn [(power [z]
            (if (complex-pair? z)
              (let [[re im] z] (+ (* re re) (* im im)))
              (mapv power z)))]
    (power audio-fft)))

(defn freq-to-mel
  "Hertz to mel scale: 2595 * log10(1 + freq/700)."
  [freq]
  (* 2595.0 (Math/log10 (+ 1.0 (/ (double freq) 700.0)))))

(defn mel-to-freq
  "Mel scale to Hertz: 700 * (10^(mels/2595) - 1)."
  [mels]
  (* 700.0 (- (Math/pow 10.0 (/ (double mels) 2595.0)) 1.0)))

(defn get-filter-points
  "Mel filter points: [integer-points frequencies]."
  ([sample-rate freq-min freq-high]
   (get-filter-points sample-rate freq-min freq-high 10 1024))
  ([sample-rate freq-min freq-high mel-filter-num]
   (get-filter-points sample-rate freq-min freq-high mel-filter-num 1024))
  ([sample-rate freq-min freq-high mel-filter-num ftt-size]
   (let [freqs (mapv mel-to-freq
                     (linspace (freq-to-mel freq-min) (freq-to-mel freq-high)
                               (+ mel-filter-num 2)))
         scale (/ (double (inc ftt-size)) sample-rate)]
     [(mapv #(int (Math/floor (* scale %))) freqs) freqs])))

(defn get-filters
  "Triangular mel filters over (inc (quot ftt-size 2)) FFT bins."
  [filter-points ftt-size]
  (let [width (inc (quot ftt-size 2))
        num-filters (- (count filter-points) 2)]
    (mapv (fn [n]
            (let [start (nth filter-points n)
                  mid (nth filter-points (inc n))
                  end (nth filter-points (+ n 2))
                  row (vec (repeat width 0.0))
                  row (reduce (fn [row [i v]] (assoc row i v))
                              row (map vector (range start mid)
                                       (linspace 0 1 (- mid start))))
                  row (reduce (fn [row [i v]] (assoc row i v))
                              row (map vector (range mid end)
                                       (linspace 1 0 (- end mid))))]
              row))
          (range num-filters))))

(defn mel-spaced-filterbank
  "Librosa-normalized mel filterbank for the sample rate."
  ([sample-rate] (mel-spaced-filterbank sample-rate 10 1024))
  ([sample-rate mel-filter-num] (mel-spaced-filterbank sample-rate mel-filter-num 1024))
  ([sample-rate mel-filter-num ftt-size]
   (let [[filter-points mel-freqs] (get-filter-points sample-rate 0
                                                      (quot sample-rate 2)
                                                      mel-filter-num ftt-size)
         enorm (mapv #(/ 2.0 (- (nth mel-freqs (+ % 2)) (nth mel-freqs %)))
                     (range mel-filter-num))]
     (mapv (fn [row e] (mapv #(* % e) row))
           (get-filters filter-points ftt-size) enorm))))

(defn discrete-cosine-transform
  "DCT-II basis matrix with dct-filter-num rows over filter-num inputs."
  [dct-filter-num filter-num]
  (let [n (double filter-num)
        row0 (vec (repeat filter-num (/ 1.0 (Math/sqrt n))))
        samples (mapv #(* % (/ Math/PI (* 2.0 n)))
                      (map #(+ 1.0 (* 2.0 %)) (range filter-num)))]
    (mapv (fn [i]
            (if (zero? i)
              row0
              (mapv #(* (Math/cos (* i %)) (Math/sqrt (/ 2.0 n))) samples)))
          (range dct-filter-num))))

(defn- matmul
  [a b]
  (let [bt (apply mapv vector b)]
    (mapv (fn [row] (mapv (fn [col] (reduce + (map * row col))) bt)) a)))

(defn mfcc
  "Mel Frequency Cepstral Coefficients of audio at sample-rate.
  Returns dct-filter-num rows over one column per frame.
  Throws :ValueError on empty input."
  ([audio sample-rate] (mfcc audio sample-rate 1024 20 10 40))
  ([audio sample-rate ftt-size] (mfcc audio sample-rate ftt-size 20 10 40))
  ([audio sample-rate ftt-size hop-length]
   (mfcc audio sample-rate ftt-size hop-length 10 40))
  ([audio sample-rate ftt-size hop-length mel-filter-num]
   (mfcc audio sample-rate ftt-size hop-length mel-filter-num 40))
  ([audio sample-rate ftt-size hop-length mel-filter-num dct-filter-num]
   (when (empty? audio)
     (throw (ex-info "audio must not be empty" {:type :ValueError})))
   (let [framed (audio-frames (normalize audio) sample-rate hop-length ftt-size)
         window (mapv #(- 0.5 (* 0.5 (Math/cos (/ (* 2.0 Math/PI %) ftt-size))))
                      (range ftt-size))
         windowed (mapv (fn [frame] (mapv * frame window)) framed)
         power (calculate-signal-power (calculate-fft windowed ftt-size))
         filters (mel-spaced-filterbank sample-rate mel-filter-num ftt-size)
         audio-log (mapv (fn [row] (mapv #(* 10.0 (Math/log10 %)) row))
                         (matmul filters (apply mapv vector power)))
         dct (discrete-cosine-transform dct-filter-num mel-filter-num)]
     (matmul dct audio-log))))

(defn example
  "STUB: reads a WAV file via scipy (needs wavfile plus the audio file)."
  [& _args]
  (unsupported! "wavfile"))
