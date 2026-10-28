(ns clojure-algorithms.physics.doppler-frequency
  "Doppler effect: f = (f0 * (v + v0)) / (v - vs).

  https://en.wikipedia.org/wiki/Doppler_effect")

(defn doppler-effect
  "Returns the frequency perceived by the observer given the source frequency,
  wave velocity, observer velocity (+ve towards source) and source velocity
  (+ve towards observer). Throws ArithmeticException when vs = v and ex-info
  when the resulting frequency would be non-positive."
  [org-freq wave-vel obs-vel src-vel]
  (when (= wave-vel src-vel)
    (throw (ArithmeticException.
            "Division by zero implies vs=v and observer in front of the source")))
  (let [doppler-freq (/ (* org-freq (+ wave-vel obs-vel)) (- wave-vel src-vel))]
    (when (<= doppler-freq 0)
      (throw (ex-info
              "Non-positive frequency implies vs>v or v0>v (in the opposite direction)"
              {:freq doppler-freq})))
    doppler-freq))
