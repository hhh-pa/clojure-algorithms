(ns clojure-algorithms.machine-learning.mfcc
  "Mel Frequency Cepstral Coefficients (MFCC) Calculation\n\nMFCC is an algorithm widely used in audio and speech processing to represent the\nshort-term power spectrum of a sound signal in a more compact and\ndiscriminative way. It is particularly popular in speech and audio processing\ntasks such as speech recognition and speaker identification.\n\nHow Mel Frequency Cepstral Coefficients are Calculated:\n1. Preprocessing:\n   - Load an audio signal and normalize it to ensure that the values fall\n     within a specific range (e.g., between -1 and 1).\n   - Frame the audio signal into overlapping, fixed-length segments, typically\n     using a technique like windowing to reduce spectral leakage.\n\n2. Fourier Transform:\n   - Apply a Fast Fourier Transform (FFT) to each audio frame to convert it\n     from the time domain to the frequency domain. This results in a\n     representation of the audio frame as a sequence of frequency components.\n\n3. Power Spectrum:\n   - Calculate the power spectrum by taking the squared magnitude of each\n     frequency component obtained from the FFT. This step measures the energy\n     distribution across different frequency bands.\n\n4. Mel Filterbank:\n   - Apply a set of triangular filterbanks spaced in the Mel frequency scale\n     to the power spectrum. These filters mimic the human auditory system's\n     frequency response. Each filterbank sums the power spectrum values within\n     its band.\n\n5. Logarithmic Compression:\n   - Take the logarithm (typically base 10) of the filterbank values to\n     compress the dynamic range. This step mimics the logarithmic response of\n     the human ear to sound intensity.\n\n6. Discrete Cosine Transform (DCT):\n   - Apply the Discrete Cosine Transform to the log filterbank energies to\n     obtain the MFCC coefficients. This transformation helps decorrelate the\n     filterbank energies and captures the most important features of the audio\n     signal.\n\n7. Feature Extraction:\n   - Select a subset of the DCT coefficients to form the feature vector.\n     Often, the first few coefficients (e.g., 12-13) are used for most\n     applications.\n\nReferences:\n- Mel-Frequency Cepstral Coefficients (MFCCs):\n  https://en.wikipedia.org/wiki/Mel-frequency_cepstrum\n- Speech and Language Processing by Daniel Jurafsky & James H. Martin:\n  https://web.stanford.edu/~jurafsky/slp3/\n- Mel Frequency Cepstral Coefficient (MFCC) tutorial\n  http://practicalcryptography.com/miscellaneous/machine-learning\n  /guide-mel-frequency-cepstral-coefficients-mfccs/\n\nAuthor: Amir Lavasani"
  (:require [clojure.string :as str] [clojure.set :as set]))

(declare mfcc normalize audio-frames calculate-fft calculate-signal-power freq-to-mel mel-to-freq mel-spaced-filterbank get-filters get-filter-points discrete-cosine-transform example)

(declare py-dunder py-no-dunder py-truthy py-num-bool py-cmp-safe py-lt py-sub py-repeat-vec py-mul py-div py-pow py-float-str py-quote py-repr py-str)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-sub [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (-' a b)
          (and (set? a) (set? b)) (clojure.set/difference a b)
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :sub a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :sub-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for -" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for -" {:type :TypeError})))))

(defn- py-repeat-vec [v n]
  (vec (apply concat (repeat n (seq v)))))

(defn- py-mul [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (*' a b)
          (and (string? a) (integer? b))
          (apply str (repeat (max 0 (long b)) a))
          (and (integer? a) (string? b))
          (apply str (repeat (max 0 (long a)) b))
          (and (sequential? a) (integer? b)) (py-repeat-vec a (max 0 (long b)))
          (and (integer? a) (sequential? b)) (py-repeat-vec b (max 0 (long a)))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :mul a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :mul-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for *" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for *" {:type :TypeError})))))

(defn- py-div [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :div a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :div-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for /" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (and (number? b) (zero? b))
                  (throw (ex-info "division by zero" {:type :ZeroDivisionError})))
                (if (and (number? a) (number? b))
                  (/ (double a) (double b))
                  (throw (ex-info "unsupported operand for /" {:type :TypeError}))))))

(defn- py-pow [a b]
  (cond (instance? clojure.lang.IRecord a)
        (let [r (py-dunder :pow a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        (instance? clojure.lang.IRecord b)
        (let [r (py-dunder :pow-r a b)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "unsupported operand for **" {:type :TypeError})) r))
        :else (let [a (py-num-bool a) b (py-num-bool b)]
                (when (or (not (number? a)) (not (number? b)))
                  (throw (ex-info "unsupported operand for **" {:type :TypeError})))
                (when (and (zero? a) (number? b) (neg? b))
                  (throw (ex-info "0.0 cannot be raised to a negative power" {:type :ZeroDivisionError})))
                (if (and (integer? a) (integer? b) (not (neg? b)))
                  (bigint (.pow (biginteger a) (int b)))
                  (Math/pow (double a) (double b))))))

(defn- py-float-str [v]
  (let [s (str (double v))]
    (if-let [[_ mant exp] (re-matches #"^(-?\d(?:\.\d+)?)E([+-]?\d+)$" s)]
      (let [e (Long/parseLong exp)]
        (str mant "e" (if (neg? e) "-" "+") (format "%02d" (Math/abs e))))
      s)))

(defn- py-quote [s]
  (let [esc (fn [q] (-> s
                        (clojure.string/replace "\\" "\\\\")
                        (clojure.string/replace "\n" "\\n")
                        (clojure.string/replace "\r" "\\r")
                        (clojure.string/replace "\t" "\\t")
                        (clojure.string/replace q (str "\\" q))))]
    (if (and (clojure.string/includes? s "'") (not (clojure.string/includes? s "\"")))
      (str "\"" (esc "\"") "\"")
      (str "'" (esc "'") "'"))))

(defn- py-repr [v]
  (cond (nil? v) "None"
        (true? v) "True"
        (false? v) "False"
        (string? v) (py-quote v)
        (float? v) (py-float-str v)
        (map? v) (str "{" (clojure.string/join ", "
                                               (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
        (set? v) (if (empty? v) "set()"
                     (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
        (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
        :else (str v)))

(defn- py-str
  ([] "")
  ([v]
   (cond (nil? v) "None"
         (true? v) "True"
         (false? v) "False"
         (string? v) v
         (float? v) (py-float-str v)
         (map? v) (str "{" (clojure.string/join ", "
                                                (map (fn [[k w]] (str (py-repr k) ": " (py-repr w))) v)) "}")
         (set? v) (if (empty? v) "set()"
                      (str "{" (clojure.string/join ", " (map py-repr v)) "}"))
         (sequential? v) (str "[" (clojure.string/join ", " (map py-repr v)) "]")
         :else (str v))))

(defn mfcc "Calculate Mel Frequency Cepstral Coefficients (MFCCs) from an audio signal.\n\nArgs:\n    audio: The input audio signal.\n    sample_rate: The sample rate of the audio signal (in Hz).\n    ftt_size: The size of the FFT window (default is 1024).\n    hop_length: The hop length for frame creation (default is 20ms).\n    mel_filter_num: The number of Mel filters (default is 10).\n    dct_filter_num: The number of DCT filters (default is 40).\n\nReturns:\n    A matrix of MFCCs for the input audio.\n\nRaises:\n    ValueError: If the input audio is empty.\n\nExample:\n>>> sample_rate = 44100  # Sample rate of 44.1 kHz\n>>> duration = 2.0  # Duration of 1 second\n>>> t = np.linspace(0, duration, int(sample_rate * duration), endpoint=False)\n>>> audio = 0.5 * np.sin(2 * np.pi * 440.0 * t)  # Generate a 440 Hz sine wave\n>>> mfccs = mfcc(audio, sample_rate)\n>>> mfccs.shape\n(40, 101)"
  [& _args]
  ;; stub: original requires get_window,np,numpy-attr:dot,numpy-attr:max,numpy-attr:min,numpy-attr:shape,numpy-attr:transpose; pure-Clojure port not applicable
  (throw (ex-info "requires get_window,np,numpy-attr:dot,numpy-attr:max,numpy-attr:min,numpy-attr:shape,numpy-attr:transpose" {:type :UnsupportedOperation})))

(defn normalize "Normalize an audio signal by scaling it to have values between -1 and 1.\n\nArgs:\n    audio: The input audio signal.\n\nReturns:\n    The normalized audio signal.\n\nExamples:\n>>> audio = np.array([1, 2, 3, 4, 5])\n>>> normalized_audio = normalize(audio)\n>>> float(np.max(normalized_audio))\n1.0\n>>> float(np.min(normalized_audio))\n0.2"
  [& _args]
  ;; stub: original requires np,numpy-attr:max; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:max" {:type :UnsupportedOperation})))

(defn audio-frames "Split an audio signal into overlapping frames.\n\nArgs:\n    audio: The input audio signal.\n    sample_rate: The sample rate of the audio signal.\n    hop_length: The length of the hopping (default is 20ms).\n    ftt_size: The size of the FFT window (default is 1024).\n\nReturns:\n    An array of overlapping frames.\n\nExamples:\n>>> audio = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]*1000)\n>>> sample_rate = 8000\n>>> frames = audio_frames(audio, sample_rate, hop_length=10, ftt_size=512)\n>>> frames.shape\n(126, 512)"
  [& _args]
  ;; stub: original requires np,numpy-attr:astype,numpy-attr:round; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:astype,numpy-attr:round" {:type :UnsupportedOperation})))

(defn calculate-fft "Calculate the Fast Fourier Transform (FFT) of windowed audio data.\n\nArgs:\n    audio_windowed: The windowed audio signal.\n    ftt_size: The size of the FFT (default is 1024).\n\nReturns:\n    The FFT of the audio data.\n\nExamples:\n>>> audio_windowed = np.array([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])\n>>> audio_fft = calculate_fft(audio_windowed, ftt_size=4)\n>>> bool(np.allclose(audio_fft[0], np.array([6.0+0.j, -1.5+0.8660254j,\n...     -1.5-0.8660254j])))\nTrue"
  [& _args]
  ;; stub: original requires fft,np,numpy-attr:shape,numpy-attr:transpose; pure-Clojure port not applicable
  (throw (ex-info "requires fft,np,numpy-attr:shape,numpy-attr:transpose" {:type :UnsupportedOperation})))

(defn calculate-signal-power "Calculate the power of the audio signal from its FFT.\n\nArgs:\n    audio_fft: The FFT of the audio signal.\n\nReturns:\n    The power of the audio signal.\n\nExamples:\n>>> audio_fft = np.array([1+2j, 2+3j, 3+4j, 4+5j])\n>>> power = calculate_signal_power(audio_fft)\n>>> np.allclose(power, np.array([5, 13, 25, 41]))\nTrue"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn freq-to-mel "Convert a frequency in Hertz to the mel scale.\n\nArgs:\n    freq: The frequency in Hertz.\n\nReturns:\n    The frequency in mel scale.\n\nExamples:\n>>> float(round(freq_to_mel(1000), 2))\n999.99"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn mel-to-freq "Convert a frequency in the mel scale to Hertz.\n\nArgs:\n    mels: The frequency in mel scale.\n\nReturns:\n    The frequency in Hertz.\n\nExamples:\n>>> round(mel_to_freq(999.99), 2)\n1000.01"
  [mels] (py-mul 700.0 (py-sub (py-pow 10.0 (py-div mels 2595.0)) 1.0)))

(defn mel-spaced-filterbank "Create a Mel-spaced filter bank for audio processing.\n\nArgs:\n    sample_rate: The sample rate of the audio.\n    mel_filter_num: The number of mel filters (default is 10).\n    ftt_size: The size of the FFT (default is 1024).\n\nReturns:\n    Mel-spaced filter bank.\n\nExamples:\n>>> float(round(mel_spaced_filterbank(8000, 10, 1024)[0][1], 10))\n0.0004603981"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn get-filters "Generate filters for audio processing.\n\nArgs:\n    filter_points: A list of filter points.\n    ftt_size: The size of the FFT.\n\nReturns:\n    A matrix of filters.\n\nExamples:\n>>> get_filters(np.array([0, 20, 51, 95, 161, 256], dtype=int), 512).shape\n(4, 257)"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn get-filter-points "Calculate the filter points and frequencies for mel frequency filters.\n\nArgs:\n    sample_rate: The sample rate of the audio.\n    freq_min: The minimum frequency in Hertz.\n    freq_high: The maximum frequency in Hertz.\n    mel_filter_num: The number of mel filters (default is 10).\n    ftt_size: The size of the FFT (default is 1024).\n\nReturns:\n    Filter points and corresponding frequencies.\n\nExamples:\n>>> filter_points = get_filter_points(8000, 0, 4000, mel_filter_num=4, ftt_size=512)\n>>> filter_points[0]\narray([  0,  20,  51,  95, 161, 256])\n>>> filter_points[1]\narray([   0.        ,  324.46707094,  799.33254207, 1494.30973963,\n       2511.42581671, 4000.        ])"
  [& _args]
  ;; stub: original requires np,numpy-attr:astype; pure-Clojure port not applicable
  (throw (ex-info "requires np,numpy-attr:astype" {:type :UnsupportedOperation})))

(defn discrete-cosine-transform "Compute the Discrete Cosine Transform (DCT) basis matrix.\n\nArgs:\n    dct_filter_num: The number of DCT filters to generate.\n    filter_num: The number of the fbank filters.\n\nReturns:\n    The DCT basis matrix.\n\nExamples:\n>>> float(round(discrete_cosine_transform(3, 5)[0][0], 5))\n0.44721"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn example "Example function to calculate Mel Frequency Cepstral Coefficients\n(MFCCs) from an audio file.\n\nArgs:\n    wav_file_path: The path to the WAV audio file.\n\nReturns:\n    np.ndarray: The computed MFCCs for the audio."
  [& _args]
  ;; stub: original requires np,wavfile; pure-Clojure port not applicable
  (throw (ex-info "requires np,wavfile" {:type :UnsupportedOperation})))

;; NOTE: mfcc stubbed (get_window,np,numpy-attr:dot,numpy-attr:max,numpy-attr:min,numpy-attr:shape,numpy-attr:transpose)
;; NOTE: normalize stubbed (np,numpy-attr:max)
;; NOTE: audio_frames stubbed (np,numpy-attr:astype,numpy-attr:round)
;; NOTE: calculate_fft stubbed (fft,np,numpy-attr:shape,numpy-attr:transpose)
;; NOTE: calculate_signal_power stubbed (np)
;; NOTE: freq_to_mel stubbed (np)
;; NOTE: mel_spaced_filterbank stubbed (np)
;; NOTE: get_filters stubbed (np)
;; NOTE: get_filter_points stubbed (np,numpy-attr:astype)
;; NOTE: discrete_cosine_transform stubbed (np)
;; NOTE: example stubbed (np,wavfile)
;; NOTE: top-level stmt stubbed (method .basicConfig)
