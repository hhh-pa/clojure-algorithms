(ns clojure-algorithms.audio-filters.iir-filter
  "N-order IIR filter for float samples normalized to [-1, 1].
  Generalizes the 2nd-order biquad from
  https://en.wikipedia.org/wiki/Digital_biquad_filter using the transfer
  function H(z) = (b0 + b1 z^-1 + ... + bk z^-k)
                   / (a0 + a1 z^-1 + ... + ak z^-k).

  Unlike the stateful Python class, a filter is an immutable map; `process`
  returns [output updated-filter] so state is threaded explicitly.")

(defn make-iir-filter
  "Creates an `order`-th order identity IIR filter."
  [order]
  {:order order
   :a-coeffs (into [1.0] (repeat order 0.0))
   :b-coeffs (into [1.0] (repeat order 0.0))
   :input-history (vec (repeat order 0.0))
   :output-history (vec (repeat order 0.0))})

(defn set-coefficients
  "Returns `filt` with denominator `a-coeffs` and numerator `b-coeffs`
  installed. Both must hold order+1 entries; a leading a0 may be omitted
  (defaults to 1.0)."
  [filt a-coeffs b-coeffs]
  (let [order (:order filt)
        a-coeffs (if (= (count a-coeffs) order)
                   (into [1.0] a-coeffs)
                   (vec a-coeffs))]
    (when (not= (count a-coeffs) (inc order))
      (throw (ex-info (format "Expected a_coeffs to have %d elements for %d-order filter, got %d"
                              (inc order) order (count a-coeffs)) {})))
    (when (not= (count b-coeffs) (inc order))
      (throw (ex-info (format "Expected b_coeffs to have %d elements for %d-order filter, got %d"
                              (inc order) order (count b-coeffs)) {})))
    (assoc filt :a-coeffs a-coeffs :b-coeffs (vec b-coeffs))))

(defn process
  "Filters one `sample` through `filt`. Returns [y updated-filter]."
  [filt sample]
  (let [order (:order filt)
        {:keys [a-coeffs b-coeffs input-history output-history]} filt
        result (reduce (fn [acc i]
                         (+ acc
                            (* (nth b-coeffs i) (nth input-history (dec i)))
                            (- (* (nth a-coeffs i) (nth output-history (dec i))))))
                       0.0
                       (range 1 (inc order)))
        y (/ (+ result (* (nth b-coeffs 0) sample)) (nth a-coeffs 0))]
    [y (assoc filt
              :input-history (into [sample] (subvec input-history 0 (max 0 (dec order))))
              :output-history (into [y] (subvec output-history 0 (max 0 (dec order)))))]))

(defn process-seq
  "Filters a sequence of samples, threading filter state. Returns
  [outputs final-filter]."
  [filt samples]
  (reduce (fn [[outs f] sample]
            (let [[y f2] (process f sample)]
              [(conj outs y) f2]))
          [[] filt]
          samples))
