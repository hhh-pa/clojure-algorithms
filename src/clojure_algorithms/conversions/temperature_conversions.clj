(ns clojure-algorithms.conversions.temperature-conversions
  "Port of conversions/temperature_conversions.py.
  Converts between celsius, fahrenheit, kelvin, rankine and reaumur.")

(defn- round-to [x ndigits]
  (let [s (Math/pow 10.0 ndigits)]
    (/ (Math/rint (* (double x) s)) s)))

(defn- as-double [v]
  (if (number? v)
    (double v)
    (throw (ex-info (str "could not convert string to float: '" v "'")
                    {:type :value-error}))))

(defn celsius-to-fahrenheit
  ([celsius] (celsius-to-fahrenheit celsius 2))
  ([celsius ndigits] (round-to (+ (* (as-double celsius) 9.0 (/ 5.0)) 32) ndigits)))

(defn celsius-to-kelvin
  ([celsius] (celsius-to-kelvin celsius 2))
  ([celsius ndigits] (round-to (+ (as-double celsius) 273.15) ndigits)))

(defn celsius-to-rankine
  ([celsius] (celsius-to-rankine celsius 2))
  ([celsius ndigits] (round-to (+ (* (as-double celsius) 9.0 (/ 5.0)) 491.67) ndigits)))

(defn fahrenheit-to-celsius
  ([fahrenheit] (fahrenheit-to-celsius fahrenheit 2))
  ([fahrenheit ndigits] (round-to (* (- (as-double fahrenheit) 32) 5.0 (/ 9.0)) ndigits)))

(defn fahrenheit-to-kelvin
  ([fahrenheit] (fahrenheit-to-kelvin fahrenheit 2))
  ([fahrenheit ndigits]
   (round-to (+ (* (- (as-double fahrenheit) 32) 5.0 (/ 9.0)) 273.15) ndigits)))

(defn fahrenheit-to-rankine
  ([fahrenheit] (fahrenheit-to-rankine fahrenheit 2))
  ([fahrenheit ndigits] (round-to (+ (as-double fahrenheit) 459.67) ndigits)))

(defn kelvin-to-celsius
  ([kelvin] (kelvin-to-celsius kelvin 2))
  ([kelvin ndigits] (round-to (- (as-double kelvin) 273.15) ndigits)))

(defn kelvin-to-fahrenheit
  ([kelvin] (kelvin-to-fahrenheit kelvin 2))
  ([kelvin ndigits]
   (round-to (+ (* (- (as-double kelvin) 273.15) 9.0 (/ 5.0)) 32) ndigits)))

(defn kelvin-to-rankine
  ([kelvin] (kelvin-to-rankine kelvin 2))
  ([kelvin ndigits] (round-to (* (as-double kelvin) 9.0 (/ 5.0)) ndigits)))

(defn rankine-to-celsius
  ([rankine] (rankine-to-celsius rankine 2))
  ([rankine ndigits] (round-to (* (- (as-double rankine) 491.67) 5.0 (/ 9.0)) ndigits)))

(defn rankine-to-fahrenheit
  ([rankine] (rankine-to-fahrenheit rankine 2))
  ([rankine ndigits] (round-to (- (as-double rankine) 459.67) ndigits)))

(defn rankine-to-kelvin
  ([rankine] (rankine-to-kelvin rankine 2))
  ([rankine ndigits] (round-to (* (as-double rankine) 5.0 (/ 9.0)) ndigits)))

(defn reaumur-to-kelvin
  ([reaumur] (reaumur-to-kelvin reaumur 2))
  ([reaumur ndigits] (round-to (+ (* (as-double reaumur) 1.25) 273.15) ndigits)))

(defn reaumur-to-fahrenheit
  ([reaumur] (reaumur-to-fahrenheit reaumur 2))
  ([reaumur ndigits] (round-to (+ (* (as-double reaumur) 2.25) 32) ndigits)))

(defn reaumur-to-celsius
  ([reaumur] (reaumur-to-celsius reaumur 2))
  ([reaumur ndigits] (round-to (* (as-double reaumur) 1.25) ndigits)))

(defn reaumur-to-rankine
  ([reaumur] (reaumur-to-rankine reaumur 2))
  ([reaumur ndigits] (round-to (+ (* (as-double reaumur) 2.25) 491.67) ndigits)))
