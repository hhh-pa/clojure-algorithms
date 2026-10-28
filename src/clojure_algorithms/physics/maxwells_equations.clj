(ns clojure-algorithms.physics.maxwells-equations
  "Maxwell's four equations plus derived electromagnetic wave quantities.

  Reference: https://en.wikipedia.org/wiki/Maxwell%27s_equations")

(def vacuum-permittivity
  "Vacuum permittivity eps0 in F/m."
  8.8541878128e-12)

(def vacuum-permeability
  "Vacuum permeability mu0 in H/m."
  (* 4 Math/PI 1e-7))

(def speed-of-light
  "Speed of light in m/s."
  299792458.0)

(defn- within-tolerance?
  "True when actual matches expected within 1% (or 1e-10 near zero)."
  [actual expected]
  (let [tolerance (if (zero? expected) 1e-10 (* 0.01 (Math/abs expected)))]
    (<= (Math/abs (- actual expected)) tolerance)))

(defn gauss-law-electric
  "Gauss's law for electricity: flux E*A = Q/eps. Returns true when the
  given field, area and enclosed charge satisfy it within tolerance."
  [electric-field-magnitude surface-area enclosed-charge
   & {:keys [permittivity] :or {permittivity vacuum-permittivity}}]
  (when (< surface-area 0)
    (throw (ex-info "Surface area must be non-negative" {})))
  (when (<= permittivity 0)
    (throw (ex-info "Permittivity must be positive" {})))
  (within-tolerance? (* electric-field-magnitude surface-area)
                     (/ enclosed-charge permittivity)))

(defn gauss-law-magnetic
  "Gauss's law for magnetism: flux through a closed surface is zero
  (no magnetic monopoles). Returns true for non-negative areas."
  [surface-area]
  (when (< surface-area 0)
    (throw (ex-info "Surface area must be non-negative" {})))
  (<= (Math/abs 0.0) 1e-10))

(defn faraday-law
  "Faraday's law: circulation of E = -d(PhiB)/dt. Returns true when the
  given circulation and flux change rate satisfy it within tolerance."
  [electric-field-circulation magnetic-flux-change-rate]
  (within-tolerance? electric-field-circulation (- magnetic-flux-change-rate)))

(defn ampere-maxwell-law
  "Ampere-Maxwell law: circulation of B = mu0*(I + eps0*d(PhiE)/dt).
  Returns true when the given values satisfy it within tolerance."
  [magnetic-field-circulation enclosed-current electric-flux-change-rate
   & {:keys [permeability permittivity]
      :or {permeability vacuum-permeability permittivity vacuum-permittivity}}]
  (when (<= permeability 0)
    (throw (ex-info "Permeability must be positive" {})))
  (when (<= permittivity 0)
    (throw (ex-info "Permittivity must be positive" {})))
  (let [total-current (+ enclosed-current (* permittivity electric-flux-change-rate))]
    (within-tolerance? magnetic-field-circulation (* permeability total-current))))

(defn electromagnetic-wave-speed
  "Returns the EM wave speed 1/sqrt(mu*eps) in the medium (m/s)."
  [& {:keys [permeability permittivity]
      :or {permeability vacuum-permeability permittivity vacuum-permittivity}}]
  (when (<= permeability 0)
    (throw (ex-info "Permeability must be positive" {})))
  (when (<= permittivity 0)
    (throw (ex-info "Permittivity must be positive" {})))
  (/ 1.0 (Math/sqrt (* permeability permittivity))))

(defn electromagnetic-wave-impedance
  "Returns the EM wave impedance sqrt(mu/eps) of the medium (ohms)."
  [& {:keys [permeability permittivity]
      :or {permeability vacuum-permeability permittivity vacuum-permittivity}}]
  (when (<= permeability 0)
    (throw (ex-info "Permeability must be positive" {})))
  (when (<= permittivity 0)
    (throw (ex-info "Permittivity must be positive" {})))
  (Math/sqrt (/ permeability permittivity)))

(defn poynting-vector-magnitude
  "Returns |S| = |E||B|/mu0, the EM power flow density (W/m^2)."
  [electric-field magnetic-field
   & {:keys [permeability] :or {permeability vacuum-permeability}}]
  (when (<= permeability 0)
    (throw (ex-info "Permeability must be positive" {})))
  (/ (* electric-field magnetic-field) permeability))

(defn energy-density-electromagnetic
  "Returns u = (eps*E^2 + B^2/mu)/2, the EM energy density (J/m^3)."
  [electric-field magnetic-field
   & {:keys [permittivity permeability]
      :or {permittivity vacuum-permittivity permeability vacuum-permeability}}]
  (when (<= permittivity 0)
    (throw (ex-info "Permittivity must be positive" {})))
  (when (<= permeability 0)
    (throw (ex-info "Permeability must be positive" {})))
  (+ (* 0.5 permittivity electric-field electric-field)
     (* 0.5 (/ (* magnetic-field magnetic-field) permeability))))
