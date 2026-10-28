(ns clojure-algorithms.searches.tabu-search
  "Tabu search for the symmetric Travelling Salesman Problem.

  Port of TheAlgorithms/Python searches/tabu_search.py. Instead of file
  paths, `generate-neighbours` takes the graph content directly: either a
  string of \"node1 node2 distance\" lines or a collection of such lines.
  Distances are parsed to longs. The argparse CLI is not ported."
  (:require [clojure.string :as str]))

(defn generate-neighbours
  "Parse graph `content` into {node [[neighbor distance] ...]}.

  `content` is a newline-separated string or a collection of lines, each
  \"node1 node2 distance\". Edges are mirrored (symmetric TSP)."
  [content]
  (let [lines (if (string? content) (str/split-lines content) content)]
    (reduce (fn [d line]
              (let [[n1 n2 w] (str/split (str/trim line) #"\s+")
                    w (Long/parseLong w)]
                (-> d
                    (update n1 (fnil conj []) [n2 w])
                    (update n2 (fnil conj []) [n1 w]))))
            {}
            (remove str/blank? lines))))

(defn- edge-weight
  "Distance between adjacent nodes `a` and `b` in the neighbour map."
  [dict-of-neighbours a b]
  (some (fn [[n w]] (when (= n b) w)) (dict-of-neighbours a)))

(defn generate-first-solution
  "Greedy first tour from `start-node`; return [tour total-distance].

  Repeatedly visits the nearest unvisited neighbour (the redundant
  resolution strategy), finally returning to the start node."
  [start-node dict-of-neighbours]
  (loop [solution [] visiting start-node dist 0]
    (if (some #{visiting} solution)
      [(conj solution start-node)
       (+ dist
          (edge-weight dict-of-neighbours (peek solution) start-node)
          -10000)]
      (let [cands (filter (fn [[n _]] (not (some #{n} solution)))
                          (dict-of-neighbours visiting))
            [best-node best-w]
            (when (seq cands) (apply min-key second cands))]
        (if best-node
          (recur (conj solution visiting) best-node (+ dist best-w))
          (recur (conj solution visiting) visiting (+ dist 10000)))))))

(defn find-neighborhood
  "All 1-1 swaps of the inner tour nodes, each with appended distance,
  sorted ascending by distance."
  [solution dict-of-neighbours]
  (let [tour (vec solution)
        middle (subvec tour 1 (dec (count tour)))
        index-of (fn [n]
                   (first (keep-indexed #(when (= %2 n) %1) tour)))
        swaps (for [n middle kn middle :when (not= n kn)]
                (let [swapped (assoc tour (index-of n) kn (index-of kn) n)
                      dist (reduce + (map #(edge-weight dict-of-neighbours %1 %2)
                                          swapped
                                          (rest swapped)))]
                  (conj swapped dist)))]
    (vec (sort-by peek (distinct swaps)))))

(defn tabu-search
  "Run tabu search for `iters` iterations with tabu list `size`;
  return [best-tour best-distance]."
  [first-solution distance-of-first-solution dict-of-neighbours iters size]
  (loop [iter 1
         solution (vec first-solution)
         tabu []
         best-cost distance-of-first-solution
         best (vec first-solution)]
    (if (> iter iters)
      [best best-cost]
      (let [neighborhood (find-neighborhood solution dict-of-neighbours)
            [solution cost tabu]
            (loop [idx 0]
              (let [cand (nth neighborhood idx)
                    route (vec (butlast cand))
                    i (first (keep-indexed
                              #(when (not= %2 (nth solution %1)) %1)
                              route))
                    e1 (nth route i)
                    e2 (nth solution i)]
                (if (and (not (some #{[e1 e2]} tabu))
                         (not (some #{[e2 e1]} tabu)))
                  [route (peek cand) (conj tabu [e1 e2])]
                  (recur (inc idx)))))
            [best best-cost]
            (if (< cost best-cost) [solution cost] [best best-cost])
            tabu (if (>= (count tabu) size) (vec (rest tabu)) tabu)]
        (recur (inc iter) solution tabu best-cost best)))))
