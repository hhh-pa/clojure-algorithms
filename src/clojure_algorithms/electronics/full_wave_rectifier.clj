(ns clojure-algorithms.electronics.full-wave-rectifier
  "Full-wave rectifier circuit quantities.

  Each function returns {:name :value}, mirroring the Python Result tuple.
  Ported from TheAlgorithms/Python electronics/full_wave_rectifier.py.")

(defn max-load-current
  "Maximum load current Im = Vm / (rf + rs + rl)."
  [rf rs rl vm]
  (cond
    (= 3 (count (filter zero? [rf rs rl])))
    (throw (IllegalArgumentException. "At least one Resistance must be non-zero"))
    (or (neg? rf) (neg? rs) (neg? rl))
    (throw (IllegalArgumentException. "Resistance cannot be negative"))
    :else {:name "max_load_current" :value (double (/ vm (+ rf rs rl)))}))

(defn dc-current
  "Average DC current Idc = 2 * Im * 0.318."
  [im]
  {:name "Idc" :value (* 2 im 0.318)})

(defn dc-voltage
  "Average DC voltage Vdc = 2 * 0.318 * Vm."
  [vm]
  {:name "Vdc" :value (* 2 0.318 vm)})

(defn max-current
  "Maximum current Im = Vm / rl."
  [vm rl]
  (when (<= rl 0)
    (throw (IllegalArgumentException.
            "Resistance cannot be negative or equal to zero")))
  {:name "Max_current_Im" :value (double (/ vm rl))})

(defn rms-current
  "RMS current Irms = Im * 0.707."
  [im]
  {:name "Irms" :value (* im 0.707)})

(defn rms-voltage
  "RMS voltage Vrms = Vm * 0.707."
  [vm]
  {:name "Vrms" :value (* vm 0.707)})
