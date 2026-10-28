(ns clojure-algorithms.audio-filters.show-response
  "Frequency/phase response helpers.

  `get-bounds` is a pure port of the Python reference. The plotting entry
  points (`show-frequency-response`, `show-phase-response`) are STUBS: the
  reference computes them with NumPy FFTs over tens of thousands of samples
  and renders with matplotlib, neither of which has a stdlib-only Clojure
  equivalent. `impulse-response` exposes the pure impulse-response prefix
  (unit impulse through the filter's `process` fn) used by both plots.
  See https://en.wikipedia.org/wiki/Frequency_response")

(defn get-bounds
  "Returns [lowest highest] plot bounds for FFT magnitudes `fft-results`
  at `samplerate`: the data range over bins 1..samplerate/2-2, widened to
  at least [-20, 20]."
  [fft-results samplerate]
  ;; Slice mirrors Python's fft_results[1 : samplerate // 2 - 1] (end-exclusive).
  (let [slice (subvec (vec fft-results) 1 (dec (quot samplerate 2)))]
    [(min -20 (apply min slice))
     (max 20 (apply max slice))]))

(defn impulse-response
  "Feeds a 512-sample unit impulse through `process-fn` (a single-sample
  filter step, e.g. from clojure-algorithms.audio-filters.iir-filter) and
  returns the 512 outputs."
  [process-fn]
  (let [[outs _] (reduce (fn [[outs f] sample]
                           (let [[y f2] (process-fn f sample)]
                             [(conj outs y) f2]))
                         [[] nil]
                         (into [1] (repeat 511 0)))]
    outs))

(defn show-frequency-response
  "STUB: would plot gain (dB) vs log frequency via FFT + matplotlib.
  Always throws; use `get-bounds`/`impulse-response` for the pure parts."
  [_filter _samplerate]
  (throw (UnsupportedOperationException.
          (str "show-frequency-response needs NumPy FFT and matplotlib; "
               "see the show-response namespace docstring."))))

(defn show-phase-response
  "STUB: would plot unwrapped phase vs log frequency via FFT + matplotlib.
  Always throws; use `get-bounds`/`impulse-response` for the pure parts."
  [_filter _samplerate]
  (throw (UnsupportedOperationException.
          (str "show-phase-response needs NumPy FFT and matplotlib; "
               "see the show-response namespace docstring."))))
