(ns clojure-algorithms.physics.n-body-simulation
  "Gravitational N-body simulation with Euler integration at each time step.

  Pure-math port of the reference: bodies and systems are immutable maps and
  every update returns a new value. The matplotlib animation/plotting helpers
  have no stdlib equivalent and are intentionally omitted.
  https://en.wikipedia.org/wiki/N-body_simulation")

(def interval
  "Frame rate of the animation in the reference implementation."
  20)

(def delta-time
  "Time between time steps in seconds."
  (/ interval 1000.0))

(defn make-body
  "Creates a body map with position, velocity, mass, size and color."
  [position-x position-y velocity-x velocity-y
   & {:keys [mass size color] :or {mass 1.0 size 1.0 color "blue"}}]
  {:position-x (double position-x) :position-y (double position-y)
   :velocity-x (double velocity-x) :velocity-y (double velocity-y)
   :mass (double mass) :size (double size) :color color})

(defn body-position
  "Returns the [x y] position of a body."
  [body]
  [(:position-x body) (:position-y body)])

(defn body-velocity
  "Returns the [vx vy] velocity of a body."
  [body]
  [(:velocity-x body) (:velocity-y body)])

(defn update-velocity
  "Returns the body with velocity advanced by force over delta-time (Euler)."
  [body force-x force-y delta-time]
  (-> body
      (update :velocity-x + (* force-x delta-time))
      (update :velocity-y + (* force-y delta-time))))

(defn update-position
  "Returns the body with position advanced by its velocity (Euler)."
  [body delta-time]
  (-> body
      (update :position-x + (* (:velocity-x body) delta-time))
      (update :position-y + (* (:velocity-y body) delta-time))))

(defn make-system
  "Creates a body-system map holding bodies plus gravitation, time-factor
  and softening-factor parameters."
  [bodies & {:keys [gravitation-constant time-factor softening-factor]
             :or {gravitation-constant 1.0 time-factor 1.0
                  softening-factor 0.0}}]
  {:bodies (vec bodies) :gravitation-constant (double gravitation-constant)
   :time-factor (double time-factor)
   :softening-factor (double softening-factor)})

(defn update-system
  "Returns the system advanced by delta-time: pairwise Newtonian gravity
  updates every velocity first, then every position."
  [system delta-time]
  (let [{:keys [bodies gravitation-constant time-factor
                softening-factor]} system
        dt (* delta-time time-factor)
        stepped (mapv (fn [body1]
                        (let [[fx fy]
                              (reduce (fn [[ax ay] body2]
                                        (if (identical? body1 body2)
                                          [ax ay]
                                          (let [dx (- (:position-x body2)
                                                      (:position-x body1))
                                                dy (- (:position-y body2)
                                                      (:position-y body1))
                                                dist (Math/pow (+ (* dx dx)
                                                                  (* dy dy)
                                                                  softening-factor)
                                                               (/ 1 2))]
                                            [(+ ax (/ (* gravitation-constant
                                                         (:mass body2) dx)
                                                      (Math/pow dist 3)))
                                             (+ ay (/ (* gravitation-constant
                                                         (:mass body2) dy)
                                                      (Math/pow dist 3)))])))
                                      [0.0 0.0] bodies)]
                          (update-velocity body1 fx fy dt)))
                      bodies)]
    (assoc system :bodies (mapv #(update-position % dt) stepped))))
