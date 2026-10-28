(ns clojure-algorithms.physics.center-of-mass
  "Center of mass of a discrete system of particles.

  R = sum(mi * ri) / sum(mi)
  https://en.wikipedia.org/wiki/Center_of_mass")

(defn make-particle
  "Creates a particle map with position :x :y :z and :mass."
  [x y z mass]
  {:x x :y y :z z :mass mass})

(defn center-of-mass
  "Returns the center of mass {:x :y :z} of a seq of particle maps,
  each coordinate rounded to two decimal places."
  [particles]
  (when (empty? particles)
    (throw (ex-info "No particles provided" {})))
  (when (some #(<= (:mass %) 0) particles)
    (throw (ex-info "Mass of all particles must be greater than 0" {})))
  (let [total-mass (reduce + (map :mass particles))
        avg (fn [k] (/ (double (Math/round (double (* (/ (reduce + (map #(* (k %) (:mass %))
                                                                        particles))
                                                         total-mass)
                                                      100))))
                       100))]
    {:x (avg :x) :y (avg :y) :z (avg :z)}))
