(ns clojure-algorithms.maths.radix2-fft
  "Fast Polynomial Multiplication using radix-2 fast Fourier Transform.

STUB: the upstream implementation fundamentally requires numpy (log2/ceil),
mpmath (complex roots of unity), and complex-number arithmetic whose results
(complex coefficients such as (2+0j)) are the module's public API. The JVM
standard library has no complex-number type and no equivalent of mpmath, so a
pure-Clojure port cannot preserve the public behavior; all entry points throw
ex-info with :type :UnsupportedOperation.")

(defrecord FFT [polyA polyB len-A len-B c-max-length root product])

(defn make-FFT
  "Constructor for FFT. STUB: requires numpy/mpmath/complex support."
  [& _args]
  (throw (ex-info (str "FFT requires numpy, mpmath and complex-number arithmetic; "
                       "no pure-Clojure stdlib port preserves its behavior")
                  {:type :UnsupportedOperation})))

(defn FFT-dft
  "Discrete Fourier transform of polynomial A or B. STUB: requires complex arithmetic."
  [& _args]
  (throw (ex-info (str "FFT-dft requires complex-number arithmetic; "
                       "no pure-Clojure stdlib port preserves its behavior")
                  {:type :UnsupportedOperation})))

(defn FFT-multiply
  "Multiply the DFTs of A and B and invert to obtain A*B.
STUB: requires complex-number arithmetic."
  [& _args]
  (throw (ex-info (str "FFT-multiply requires complex-number arithmetic; "
                       "no pure-Clojure stdlib port preserves its behavior")
                  {:type :UnsupportedOperation})))

(defn FFT-str
  "Render A, B and A*B. STUB: requires a constructed FFT object."
  [& _args]
  (throw (ex-info (str "FFT-str requires a constructed FFT object, which needs "
                       "numpy/mpmath/complex support")
                  {:type :UnsupportedOperation})))
