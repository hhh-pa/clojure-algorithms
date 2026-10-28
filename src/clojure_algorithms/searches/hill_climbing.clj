(ns clojure-algorithms.searches.hill-climbing
  "Hill climbing optimization over a 2D search state.

  Port of TheAlgorithms/Python searches/hill_climbing.py. Search states
  are immutable SearchProblem records and `hill-climbing` takes an
  options map. The matplotlib visualization flag is accepted and ignored
  (documented here instead of plotting).")

(defrecord SearchProblem [x y step-size function])

(defn make-search-problem
  "Build a SearchProblem at (x, y) with `step-size` optimizing `f`."
  [x y step-size f]
  (->SearchProblem x y step-size f))

(defn score
  "Value of the state's function at its current (x, y)."
  [state]
  ((:function state) (:x state) (:y state)))

(defn get-neighbors
  "The eight states adjacent to `state` at step-size distance."
  [state]
  (let [{:keys [x y step-size function]} state]
    (mapv (fn [[nx ny]] (->SearchProblem nx ny step-size function))
          [[(- x step-size) (- y step-size)]
           [(- x step-size) y]
           [(- x step-size) (+ y step-size)]
           [x (- y step-size)]
           [x (+ y step-size)]
           [(+ x step-size) (- y step-size)]
           [(+ x step-size) y]
           [(+ x step-size) (+ y step-size)]])))

(defn state-str
  "Python-compatible string form: \"x: <x> y: <y>\"."
  [state]
  (str "x: " (:x state) " y: " (:y state)))

(defn hill-climbing
  "Climb from `search-prob` to a local optimum; return the final state.

  Options map keys (all optional): :find-max (default true),
  :max-x/:min-x/:max-y/:min-y bounds (default unbounded),
  :max-iter (default 10000), :visualization (accepted, ignored: there
  is no matplotlib in this port)."
  ([search-prob] (hill-climbing search-prob {}))
  ([search-prob {:keys [find-max max-x min-x max-y min-y max-iter]
                 :or {find-max true
                      max-x Double/POSITIVE_INFINITY
                      min-x Double/NEGATIVE_INFINITY
                      max-y Double/POSITIVE_INFINITY
                      min-y Double/NEGATIVE_INFINITY
                      max-iter 10000}}]
   (loop [current search-prob
          visited #{search-prob}
          iterations 0]
     (if (>= iterations max-iter)
       current
       (let [current-score (score current)
             next-state
             (loop [neighbors (get-neighbors current)
                    best nil
                    best-change (if find-max
                                  Double/NEGATIVE_INFINITY
                                  Double/POSITIVE_INFINITY)]
               (if (empty? neighbors)
                 best
                 (let [neighbor (first neighbors)
                       rest-neighbors (rest neighbors)]
                   (if (or (contains? visited neighbor)
                           (> (:x neighbor) max-x)
                           (< (:x neighbor) min-x)
                           (> (:y neighbor) max-y)
                           (< (:y neighbor) min-y))
                     (recur rest-neighbors best best-change)
                     (let [change (- (score neighbor) current-score)]
                       (if (if find-max
                             (and (> change best-change) (pos? change))
                             (and (< change best-change) (neg? change)))
                         (recur rest-neighbors neighbor change)
                         (recur rest-neighbors best best-change)))))))]
         (if (nil? next-state)
           current
           (recur next-state (conj visited next-state) (inc iterations))))))))
