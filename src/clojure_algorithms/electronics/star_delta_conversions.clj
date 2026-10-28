(ns clojure-algorithms.electronics.star-delta-conversions
  "Star/delta (Y/impedance) network transformations.

  Impedances are [real imag] pairs, since Clojure has no complex literals.
  Ported from TheAlgorithms/Python electronics/star_delta_conversions.py.")

(defn- c-add [[a b] [c d]] [(+ a c) (+ b d)])
(defn- c-mul [[a b] [c d]] [(- (* a c) (* b d)) (+ (* a d) (* b c))])
(defn- c-div [[a b] [c d]]
  (let [den (+ (* c c) (* d d))]
    [(/ (+ (* a c) (* b d)) den) (/ (- (* b c) (* a d)) den)]))
(defn- c-abs [[a b]] (Math/sqrt (+ (* a a) (* b b))))

(defn- check-impedance
  "Throw unless every impedance in `zs` is non-zero with non-negative resistance."
  [zs]
  (when (some zero? (map c-abs zs))
    (throw (IllegalArgumentException. "entered impedance value is zero")))
  (when (some neg? (map first zs))
    (throw (IllegalArgumentException. "entered resistance value is negative"))))

(defn star-to-delta
  "Convert star impedances za, zb, zc to the delta equivalents."
  [za zb zc]
  (check-impedance [za zb zc])
  (let [chain (c-add (c-add (c-mul za zb) (c-mul zb zc)) (c-mul zc za))]
    [(c-div chain za) (c-div chain zb) (c-div chain zc)]))

(defn delta-to-star
  "Convert delta impedances za, zb, zc to the star equivalents."
  [za zb zc]
  (check-impedance [za zb zc])
  (let [total (c-add (c-add za zb) zc)]
    [(c-div (c-mul zb zc) total)
     (c-div (c-mul zc za) total)
     (c-div (c-mul za zb) total)]))
