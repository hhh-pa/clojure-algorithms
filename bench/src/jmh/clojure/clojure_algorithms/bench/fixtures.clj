(ns clojure-algorithms.bench.fixtures
  "Deterministic inputs for the JMH benchmarks in bench/src/jmh/java.
  Every generator uses a fixed seed so runs are comparable over time."
  (:import (java.util Random)))

(defn random-ints
  "Vector of `n` pseudo-random longs in [0, bound), seeded with `seed`."
  ([n] (random-ints n 1000000 42))
  ([n bound seed]
   (let [rng (Random. seed)]
     (vec (repeatedly n #(long (.nextInt rng (int bound))))))))

(defn sorted-ints
  "Sorted vector of `n` distinct longs (0, 2, 4, ...)."
  [n]
  (vec (range 0 (* 2 n) 2)))

(defn random-text
  "Pseudo-random lowercase string of length `n` over the first `alphabet`
  letters, seeded with `seed`."
  ([n] (random-text n 26 7))
  ([n alphabet seed]
   (let [rng (Random. seed)
         sb (StringBuilder. (int n))]
     (dotimes [_ n]
       (.append sb (char (+ 97 (.nextInt rng (int alphabet))))))
     (str sb))))

(def ^:private lorem
  (str "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do "
       "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim "
       "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
       "aliquip ex ea commodo consequat. "))

(defn prose
  "English-like text of at least `n` characters, truncated to `n`."
  [n]
  (let [s (apply str (repeat (inc (quot n (count lorem))) lorem))]
    (subs s 0 n)))

(defn weighted-digraph
  "String-keyed adjacency map {node [[neighbour cost] ...]} with `n` nodes
  and `degree` random outgoing edges per node, plus a chain n0 -> n1 -> ...
  so every node is reachable from \"n0\"."
  [n degree seed]
  (let [rng (Random. seed)
        node (fn [i] (str "n" i))]
    (into {}
          (for [i (range n)]
            [(node i)
             (vec (concat
                   (when (< (inc i) n) [[(node (inc i)) 10]])
                   (repeatedly degree
                               (fn [] [(node (.nextInt rng (int n)))
                                       (inc (.nextInt rng 9))]))))]))))

(defn weighted-edges
  "Vector of `m` random [u v weight] edges over `n` nodes, plus a chain so
  the graph is connected."
  [n m seed]
  (let [rng (Random. seed)]
    (vec (concat
          (for [i (range (dec n))] [i (inc i) 1000])
          (repeatedly m (fn [] [(.nextInt rng (int n))
                                (.nextInt rng (int n))
                                (inc (.nextInt rng 999))]))))))

(defn random-edges
  "Vector of `m` random directed [u v] edges over `n` nodes."
  [n m seed]
  (let [rng (Random. seed)]
    (vec (repeatedly m (fn [] [(.nextInt rng (int n)) (.nextInt rng (int n))])))))

(defn bfs-graph
  "Directed graph {vertex #{neighbours}} with `n` vertices and `degree`
  random edges per vertex, plus a chain for reachability."
  [n degree seed]
  (let [rng (Random. seed)]
    (into {}
          (for [i (range n)]
            [i (into (if (< (inc i) n) #{(inc i)} #{})
                     (repeatedly degree #(.nextInt rng (int n))))]))))

(defn random-signed-ints
  "Vector of `n` pseudo-random longs in (-bound, bound), seeded with `seed`."
  [n bound seed]
  (let [rng (Random. seed)]
    (vec (repeatedly n #(long (- (.nextInt rng (int (* 2 bound))) bound))))))

(defn inc-all
  "Increments every element of `coll`; returns a vector."
  [coll]
  (mapv inc coll))

(defn adjacency-map
  "Directed graph {u [v ...]} over `n` nodes from `m` random edges."
  [n m seed]
  (reduce (fn [g [u v]] (update g u conj v))
          (into {} (map #(vector % []) (range n)))
          (random-edges n m seed)))

(defn binary-grid
  "`size` x `size` grid of 1 (walkable) / 0 (obstacle) cells with roughly
  `obstacle-pct` percent obstacles. Corners are always walkable."
  [size obstacle-pct seed]
  (let [rng (Random. seed)
        last-cell (dec size)]
    (vec (for [r (range size)]
           (vec (for [c (range size)]
                  (if (or (and (zero? r) (zero? c))
                          (and (= r last-cell) (= c last-cell))
                          (>= (.nextInt rng 100) obstacle-pct))
                    1
                    0)))))))
