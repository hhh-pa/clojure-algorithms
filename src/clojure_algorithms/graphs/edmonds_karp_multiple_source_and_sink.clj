(ns clojure-algorithms.graphs.edmonds-karp-multiple-source-and-sink
  "Maximum flow with multiple sources/sinks via push-relabel.")

(defn- normalize
  "Adds a super source/sink when several are given."
  [graph sources sinks]
  (let [sources (vec sources) sinks (vec sinks)]
    (if (or (empty? sources) (empty? sinks))
      {:graph graph :source nil :sink nil}
      (if (and (= 1 (count sources)) (= 1 (count sinks)))
        {:graph graph :source (first sources) :sink (first sinks)}
        (let [cap (reduce + (map #(reduce + (nth graph %)) sources))
              n (count graph)
              g1 (vec (cons (vec (repeat (inc n) 0))
                            (mapv #(vec (cons 0 %)) graph)))
              g1 (reduce (fn [g s] (assoc-in g [0 (inc s)] cap)) g1 sources)
              n2 (count g1)
              g2 (vec (conj (mapv #(conj % 0) g1)
                            (vec (repeat (inc n2) 0))))]
          {:graph (reduce (fn [g t] (assoc-in g [(inc t) n2] cap)) g2 sinks)
           :source 0 :sink n2})))))

(defn- push-relabel [graph source sink]
  (let [n (count graph)
        flow (atom (vec (repeat n (vec (repeat n 0)))))
        height (atom (assoc (vec (repeat n 0)) source n))
        excess (atom (vec (repeat n 0)))]
    (doseq [v (range n)]
      (let [cap (get-in graph [source v])]
        (swap! flow assoc-in [source v] (+ (get-in @flow [source v]) cap))
        (swap! flow assoc-in [v source] (- (get-in @flow [v source]) cap))
        (swap! excess assoc v (+ (nth @excess v) cap))))
    (letfn [(push [u v]
              (let [delta (min (nth @excess u)
                               (- (get-in graph [u v])
                                  (get-in @flow [u v])))]
                (swap! flow assoc-in [u v] (+ (get-in @flow [u v]) delta))
                (swap! flow assoc-in [v u] (- (get-in @flow [v u]) delta))
                (swap! excess assoc u (- (nth @excess u) delta))
                (swap! excess assoc v (+ (nth @excess v) delta))))
            (relabel [u]
              (let [hs (keep (fn [v]
                               (when (pos? (- (get-in graph [u v])
                                              (get-in @flow [u v])))
                                 (nth @height v)))
                             (range n))]
                (when (seq hs)
                  (swap! height assoc u (inc (apply min hs))))))
            (discharge [u]
              (while (pos? (nth @excess u))
                (doseq [v (range n)]
                  (when (and (pos? (- (get-in graph [u v])
                                      (get-in @flow [u v])))
                             (> (nth @height u) (nth @height v)))
                    (push u v)))
                (relabel u)))]
      (loop [order (vec (remove #{source sink} (range n)))]
        (when (seq order)
          (let [u (first order)
                h (nth @height u)]
            (discharge u)
            (recur (if (> (nth @height u) h)
                     (vec (cons u (rest order)))
                     (vec (rest order)))))))
      (reduce + (nth @flow source)))))

(defn find-maximum-flow
  "Returns the maximum flow from sources to sinks."
  [graph sources sinks]
  (let [{:keys [graph source sink]} (normalize graph sources sinks)]
    (if (or (nil? source) (nil? sink))
      0
      (push-relabel graph source sink))))
