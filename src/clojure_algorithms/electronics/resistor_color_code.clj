(ns clojure-algorithms.electronics.resistor-color-code
  "Resistance of an n-band resistor from its IEC 60062 color bands.
  Ported from TheAlgorithms/Python electronics/resistor_color_code.py.")

(def valid-colors
  "All recognized band colors."
  ["Black" "Brown" "Red" "Orange" "Yellow" "Green" "Blue" "Violet" "Grey"
   "White" "Gold" "Silver"])

(def significant-figures-color-values
  "Digit per significant-figure band color."
  {"Black" 0 "Brown" 1 "Red" 2 "Orange" 3 "Yellow" 4 "Green" 5 "Blue" 6
   "Violet" 7 "Grey" 8 "White" 9})

(def multiplier-color-values
  "Multiplier per multiplier band color."
  {"Black" 1 "Brown" 10 "Red" 100 "Orange" 1000 "Yellow" 10000
   "Green" 100000 "Blue" 1000000 "Violet" 10000000 "Grey" 100000000
   "White" 1000000000 "Gold" 0.1 "Silver" 0.01})

(def tolerance-color-values
  "Tolerance percent per tolerance band color."
  {"Brown" 1 "Red" 2 "Orange" 0.05 "Yellow" 0.02 "Green" 0.5 "Blue" 0.25
   "Violet" 0.1 "Grey" 0.01 "Gold" 5 "Silver" 10})

(def temperature-coefficient-color-values
  "Temperature coefficient (ppm/K) per band color."
  {"Black" 250 "Brown" 100 "Red" 50 "Orange" 15 "Yellow" 25 "Green" 20
   "Blue" 10 "Violet" 5 "Grey" 1})

(def band-types
  "Band-count per type for 3- to 6-band resistors."
  {3 {"significant" 2 "multiplier" 1}
   4 {"significant" 2 "multiplier" 1 "tolerance" 1}
   5 {"significant" 3 "multiplier" 1 "tolerance" 1}
   6 {"significant" 3 "multiplier" 1 "tolerance" 1 "temp_coeffecient" 1}})

(defn get-significant-digits
  "Digit string for significant-figure band `colors`."
  [colors]
  (apply str (map (fn [color]
                    (when-not (contains? significant-figures-color-values color)
                      (throw (IllegalArgumentException.
                              (str color " is not a valid color for significant figure bands"))))
                    (significant-figures-color-values color))
                  colors)))

(defn get-multiplier
  "Multiplier for multiplier band `color`."
  [color]
  (when-not (contains? multiplier-color-values color)
    (throw (IllegalArgumentException.
            (str color " is not a valid color for multiplier band"))))
  (multiplier-color-values color))

(defn get-tolerance
  "Tolerance percent for tolerance band `color`."
  [color]
  (when-not (contains? tolerance-color-values color)
    (throw (IllegalArgumentException.
            (str color " is not a valid color for tolerance band"))))
  (tolerance-color-values color))

(defn get-temperature-coefficient
  "Temperature coefficient (ppm/K) for band `color`."
  [color]
  (when-not (contains? temperature-coefficient-color-values color)
    (throw (IllegalArgumentException.
            (str color " is not a valid color for temperature coeffecient band"))))
  (temperature-coefficient-color-values color))

(defn get-band-type-count
  "Number of `type-of-band` bands on a `total-number-of-bands` resistor."
  [total-number-of-bands type-of-band]
  (when-not (contains? band-types total-number-of-bands)
    (throw (IllegalArgumentException.
            (str total-number-of-bands " is not a valid number of bands"))))
  (when-not (contains? (band-types total-number-of-bands) type-of-band)
    (throw (IllegalArgumentException.
            (str type-of-band " is not valid for a "
                 total-number-of-bands " band resistor"))))
  ((band-types total-number-of-bands) type-of-band))

(defn check-validity
  "True when `colors` is a valid `number-of-bands` code; throws otherwise."
  [number-of-bands colors]
  (when-not (<= 3 number-of-bands 6)
    (throw (IllegalArgumentException.
            "Invalid number of bands. Resistor bands must be 3 to 6")))
  (when-not (= number-of-bands (count colors))
    (throw (IllegalArgumentException.
            (str "Expecting " number-of-bands " colors, provided "
                 (count colors) " colors"))))
  (doseq [color colors]
    (when-not (some #{color} valid-colors)
      (throw (IllegalArgumentException. (str color " is not a valid color")))))
  true)

(defn calculate-resistance
  "Resistance map {:resistance \"...\"} for the given band color code."
  [number-of-bands color-code-list]
  (check-validity number-of-bands color-code-list)
  (let [n-significant (get-band-type-count number-of-bands "significant")
        significant-digits (Integer/parseInt
                            (get-significant-digits
                             (take n-significant color-code-list)))
        multiplier (get-multiplier (nth color-code-list n-significant))
        tolerance (if (= number-of-bands 3)
                    20
                    (get-tolerance (nth color-code-list (inc n-significant))))
        temp-coeff (if (= number-of-bands 6)
                     (get-temperature-coefficient
                      (nth color-code-list (+ n-significant 2)))
                     0)
        resistance (* significant-digits multiplier)]
    {:resistance (if (zero? temp-coeff)
                   (str resistance "\u03A9 \u00B1" tolerance "% ")
                   (str resistance "\u03A9 \u00B1" tolerance "% "
                        temp-coeff " ppm/K"))}))
