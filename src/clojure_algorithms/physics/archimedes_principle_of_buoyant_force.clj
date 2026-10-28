(ns clojure-algorithms.physics.archimedes-principle-of-buoyant-force
  "Buoyant force on a body submerged in a static fluid (Archimedes' principle).

  Fb = p * V * g
  https://en.wikipedia.org/wiki/Archimedes%27_principle")

(def g
  "Acceleration due to gravity on Earth in m/s^2."
  9.80665)

(defn archimedes-principle
  "Returns the buoyant force in Newtons given fluid density (kg/m^3),
  displaced volume (m^3) and optionally gravity (m/s^2, defaults to Earth's)."
  ([fluid-density volume] (archimedes-principle fluid-density volume g))
  ([fluid-density volume gravity]
   (cond
     (<= fluid-density 0)
     (throw (ex-info "Impossible fluid density" {:fluid-density fluid-density}))
     (<= volume 0)
     (throw (ex-info "Impossible object volume" {:volume volume}))
     (< gravity 0)
     (throw (ex-info "Impossible gravity" {:gravity gravity}))
     :else
     (* fluid-density gravity volume))))
