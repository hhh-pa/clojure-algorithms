(ns clojure-algorithms.physics.grahams-law
  "Graham's law of effusion: r1/r2 = sqrt(m2/m1).

  https://en.wikipedia.org/wiki/Graham%27s_law")

(defn valid?
  "Returns true when at least one value is given and all are positive."
  [& values]
  (boolean (and (seq values) (every? #(> % 0.0) values))))

(defn- round6 [x] (/ (double (Math/round (* x 1e6))) 1e6))

(defn effusion-ratio
  "Returns r1/r2 = sqrt(m2/m1) for molar masses m1 and m2."
  [molar-mass-1 molar-mass-2]
  (when (not (valid? molar-mass-1 molar-mass-2))
    (throw (ex-info "Input Error: Molar mass values must greater than 0." {})))
  (round6 (Math/sqrt (/ molar-mass-2 molar-mass-1))))

(defn first-effusion-rate
  "Returns r1 = r2 * sqrt(m2/m1)."
  [effusion-rate molar-mass-1 molar-mass-2]
  (when (not (valid? effusion-rate molar-mass-1 molar-mass-2))
    (throw (ex-info
            "Input Error: Molar mass and effusion rate values must greater than 0."
            {})))
  (round6 (* effusion-rate (Math/sqrt (/ molar-mass-2 molar-mass-1)))))

(defn second-effusion-rate
  "Returns r2 = r1 / sqrt(m2/m1)."
  [effusion-rate molar-mass-1 molar-mass-2]
  (when (not (valid? effusion-rate molar-mass-1 molar-mass-2))
    (throw (ex-info
            "Input Error: Molar mass and effusion rate values must greater than 0."
            {})))
  (round6 (/ effusion-rate (Math/sqrt (/ molar-mass-2 molar-mass-1)))))

(defn first-molar-mass
  "Returns m1 = m2 / (r1/r2)^2."
  [molar-mass effusion-rate-1 effusion-rate-2]
  (when (not (valid? molar-mass effusion-rate-1 effusion-rate-2))
    (throw (ex-info
            "Input Error: Molar mass and effusion rate values must greater than 0."
            {})))
  (round6 (/ molar-mass (Math/pow (/ effusion-rate-1 effusion-rate-2) 2))))

(defn second-molar-mass
  "Returns m2 = (r1/r2)^2 / m1."
  [molar-mass effusion-rate-1 effusion-rate-2]
  (when (not (valid? molar-mass effusion-rate-1 effusion-rate-2))
    (throw (ex-info
            "Input Error: Molar mass and effusion rate values must greater than 0."
            {})))
  (round6 (/ (Math/pow (/ effusion-rate-1 effusion-rate-2) 2) molar-mass)))
