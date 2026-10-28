(ns clojure-algorithms.graphs.tarjans-scc
  "Strongly connected components via Tarjan's algorithm.")

(defn create-graph
  "Builds an n-vertex adjacency vector from [u v] edges."
  [n edges]
  (reduce (fn [g [u v]] (update g u conj v))
          (vec (repeat n [])) edges))

(defn tarjan
  "Returns the strongly connected components of adjacency-vector g."
  [g]
  (let [n (count g)
        index-of (atom (vec (repeat n -1)))
        lowlink-of (atom (vec (repeat n -1)))
        on-stack (atom (vec (repeat n false)))
        stack (atom [])
        next-index (atom 0)
        components (atom [])]
    (letfn [(strong-connect [v]
              (swap! index-of assoc v @next-index)
              (swap! lowlink-of assoc v @next-index)
              (swap! next-index inc)
              (swap! stack conj v)
              (swap! on-stack assoc v true)
              (doseq [w (nth g v)]
                (if (= -1 (nth @index-of w))
                  (do (strong-connect w)
                      (swap! lowlink-of assoc v
                             (min (nth @lowlink-of v) (nth @lowlink-of w))))
                  (when (nth @on-stack w)
                    (swap! lowlink-of assoc v
                           (min (nth @lowlink-of v) (nth @lowlink-of w))))))
              (when (= (nth @lowlink-of v) (nth @index-of v))
                (let [comp (atom [])]
                  (loop []
                    (let [w (peek @stack)]
                      (swap! stack pop)
                      (swap! on-stack assoc w false)
                      (swap! comp conj w)
                      (when (not= w v)
                        (recur))))
                  (swap! components conj @comp))))]
      (doseq [v (range n)]
        (when (= -1 (nth @index-of v))
          (strong-connect v))))
    @components))
