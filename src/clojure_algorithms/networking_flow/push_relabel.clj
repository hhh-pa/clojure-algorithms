(ns clojure-algorithms.networking-flow.push-relabel
  "Push-relabel (Goldberg-Tarjan) algorithm for the maximum-flow problem.

  Unlike augmenting-path methods that build up a valid flow one path at
  a time, push-relabel works with a preflow in which a vertex may
  temporarily receive more flow than it sends out. Each active vertex
  either pushes its excess towards a neighbour one level lower, or is
  relabeled to a higher level so a push becomes possible. When no vertex
  other than source and sink has excess, the preflow is a maximum flow.

  Uses the highest-label selection rule (always discharge an active
  vertex with the largest label), which runs in O(V^2 * sqrt(E)) time.

  Graphs are plain immutable maps of the form
  `{:size n :adj [...] :edges [...]}` (see
  clojure-algorithms.networking-flow.dinic for the layout). Build one
  with `make-graph`/`add-edge`, then call `max-flow`. The input graph is
  never mutated.

  Reference: https://en.wikipedia.org/wiki/Push%E2%80%93relabel_maximum_flow_algorithm")

(defn make-graph
  "Creates an empty directed flow network with `n` vertices (0..n-1)."
  [n]
  (when (or (not (integer? n)) (<= n 0))
    (throw (IllegalArgumentException. "number of vertices must be positive")))
  {:size n
   :adj (vec (repeat n []))
   :edges []})

(defn- valid-vertex?
  [graph v]
  (and (integer? v) (<= 0 v (dec (:size graph)))))

(defn add-edge
  "Adds a directed edge `source -> destination` with `capacity`.

  Returns a new graph; parallel edges are supported and their capacities
  add up. Throws IllegalArgumentException on negative capacity or on a
  vertex out of range."
  [graph source destination capacity]
  (when (neg? capacity)
    (throw (IllegalArgumentException. "capacity must be non-negative")))
  (when-not (and (valid-vertex? graph source)
                 (valid-vertex? graph destination))
    (throw (IllegalArgumentException. "vertex out of range")))
  (let [fwd-idx (count (:edges graph))]
    (-> graph
        (update :edges conj [destination capacity] [source 0])
        (update-in [:adj source] conj fwd-idx)
        (update-in [:adj destination] conj (inc fwd-idx)))))

(defn max-flow
  "Returns the maximum flow from `source` to `sink` in `graph`.

  The input graph is left unchanged. Throws IllegalArgumentException
  when a vertex is out of range or when source equals sink."
  [graph source sink]
  (let [size (:size graph)
        adj (:adj graph)]
    (when-not (and (valid-vertex? graph source) (valid-vertex? graph sink))
      (throw (IllegalArgumentException. "vertex out of range")))
    (when (= source sink)
      (throw (IllegalArgumentException. "source and sink must be different")))
    (let [edges (atom (:edges graph))
          height (atom (assoc (vec (repeat size 0)) source size))
          excess (atom (vec (repeat size 0)))]
      ;; Saturate every edge leaving the source to create the preflow.
      (doseq [ei (adj source)]
        (let [[to res] (@edges ei)]
          (when (pos? res)
            (swap! edges
                   (fn [es]
                     (let [ri (bit-xor ei 1)
                           [rt rc] (es ri)]
                       (-> es
                           (assoc ei [to 0])
                           (assoc ri [rt (+ rc res)])))))
            (swap! excess update to + res)
            (swap! excess update source - res))))
      (letfn [(admissible? [u]
                (let [h @height
                      es @edges]
                  (boolean (some (fn [ei]
                                   (let [[to res] (es ei)]
                                     (and (pos? res)
                                          (= (h to) (dec (h u))))))
                                 (adj u)))))
              (apply-pushes [u]
                (loop [eis (adj u)]
                  (when (and (seq eis) (pos? (@excess u)))
                    (let [ei (first eis)
                          [to res] (@edges ei)
                          hu (@height u)
                          ht (@height to)]
                      (when (and (pos? res) (= hu (inc ht)))
                        (let [delta (min (@excess u) res)
                              ri (bit-xor ei 1)]
                          (swap! edges
                                 (fn [es]
                                   (let [[_ c1] (es ei)
                                         [rt c2] (es ri)]
                                     (-> es
                                         (assoc ei [to (- c1 delta)])
                                         (assoc ri [rt (+ c2 delta)])))))
                          (swap! excess update u - delta)
                          (swap! excess update to + delta)))
                      (recur (rest eis))))))
              (active-vertices []
                (let [ex @excess]
                  (vec (remove #(or (= % source)
                                    (= % sink)
                                    (not (pos? (ex %))))
                               (range size)))))]
        (loop []
          (let [active (active-vertices)]
            (when (seq active)
              (let [h @height
                    u (apply max-key h active)]
                (when-not (admissible? u)
                  ;; Relabel: lift u just above its lowest usable neighbour.
                  ;; (Guarded against an empty residual neighbourhood, which
                  ;; cannot happen for an active vertex in a valid preflow
                  ;; but would crash Python's min() on an empty sequence.)
                  (let [es @edges
                        hh @height
                        hs (keep (fn [ei]
                                   (let [[to res] (es ei)]
                                     (when (pos? res)
                                       (hh to))))
                                 (adj u))]
                    (when (seq hs)
                      (swap! height assoc u (inc (apply min hs))))))
                (apply-pushes u)
                (recur))))))
      (@excess sink))))
