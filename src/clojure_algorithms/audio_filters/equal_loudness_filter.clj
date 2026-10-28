(ns clojure-algorithms.audio-filters.equal-loudness-filter
  "STUB: equal-loudness filter design.

  The Python reference (`audio_filters/equal_loudness_filter.py`) is built on
  NumPy/SciPy throughout: FFTs (`np.fft`), polynomial root finding (`np.roots`,
  `np.poly`), least-squares solves (`np.linalg.lstsq`), Toeplitz systems
  (`scipy.linalg.toeplitz`), digital filtering (`scipy.signal.lfilter`) and the
  `loudness_curve.json` data file. Its core algorithms -- `_polystab`
  (polynomial stabilization via root reflection), `_numerator` (least-squares
  numerator fit) and `yulewalk` (modified Yule-Walker IIR design) -- all depend
  on those numerical primitives, which have no stdlib-only Clojure equivalents.

  A faithful port would need a from-scratch complex FFT, polynomial root
  finder and least-squares solver. This namespace therefore documents the
  intended API and throws `UnsupportedOperationException` until such a port
  exists. Pure-Clojure callers needing filtering should use
  clojure-algorithms.audio-filters.iir-filter and
  clojure-algorithms.audio-filters.butterworth-filter instead.")

(defn- unsupported
  [name]
  (throw (UnsupportedOperationException.
          (str name " requires NumPy/SciPy numerical primitives (FFT, polynomial "
               "roots, least-squares) with no stdlib-only Clojure equivalent; "
               "see the equal-loudness-filter namespace docstring."))))

(defn yulewalk
  "STUB for the modified Yule-Walker IIR design. Always throws."
  [order frequencies magnitudes & _args]
  (unsupported "yulewalk"))

(defn make-equal-loudness-filter
  "STUB for the EqualLoudnessFilter (cascaded Yule-Walker + Butterworth
  highpass). Always throws."
  [& _args]
  (unsupported "make-equal-loudness-filter"))
