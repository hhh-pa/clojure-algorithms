(ns clojure-algorithms.electronics.coulombs-law
  "Coulomb's law: solve for the zero-valued quantity among force,
  charge1, charge2 and distance. Returns a {name value} map.
  Ported from TheAlgorithms/Python electronics/coulombs_law.py.")

(def ^:const coulombs-constant
  "Coulomb's constant in N m^2 C^-2."
  8.988e9)

(defn coulombs-law
  "Apply Coulomb's law; exactly one argument must be zero (the unknown)."
  [force charge1 charge2 distance]
  (when (not= 1 (count (filter zero? [force charge1 charge2 distance])))
    (throw (IllegalArgumentException. "One and only one argument must be 0")))
  (when (neg? distance)
    (throw (IllegalArgumentException. "Distance cannot be negative")))
  (let [charge-product (Math/abs (double (* charge1 charge2)))]
    (cond
      (zero? force)
      {"force" (/ (* coulombs-constant charge-product)
                  (* distance distance))}
      (zero? charge1)
      {"charge1" (/ (* (Math/abs (double force)) distance distance)
                    (* coulombs-constant charge2))}
      (zero? charge2)
      {"charge2" (/ (* (Math/abs (double force)) distance distance)
                    (* coulombs-constant charge1))}
      :else
      {"distance" (Math/sqrt (/ (* coulombs-constant charge-product)
                                (Math/abs (double force))))})))
