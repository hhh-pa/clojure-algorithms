(ns clojure-algorithms.graphs.dinic
  "Maximum flow via Dinic's algorithm with capacity scaling.")

(defn make-network
  "Builds an empty flow network with n vertices."
  [n]
  {:n n :adj (atom (vec (repeat n [])))})

(defn add-edge
  "Adds edge a->b with capacity c (and reverse capacity rcap)."
  ([net a b c] (add-edge net a b c 0))
  ([{:keys [adj] :as net} a b c rcap]
   (let [fwd-idx (count (nth @adj a))
         bwd-idx (count (nth @adj b))]
     (swap! adj update a conj {:to b :rev bwd-idx :cap c :flow 0})
     (swap! adj update b conj {:to a :rev fwd-idx :cap rcap :flow 0}))
   net))

(defn- dfs-flow [adj lvl ptr v sink flow]
  (if (or (= v sink) (zero? flow))
    flow
    (loop [i (nth @ptr v)]
      (if (>= i (count (nth @adj v)))
        0
        (let [e (nth (nth @adj v) i)]
          (swap! ptr assoc v (inc i))
          (if (= (nth @lvl (:to e)) (inc (nth @lvl v)))
            (let [p (dfs-flow adj lvl ptr (:to e) sink
                              (min flow (- (:cap e) (:flow e))))]
              (if (pos? p)
                (do
                  (swap! adj update-in [v i :flow] + p)
                  (swap! adj update-in [(:to e) (:rev e) :flow] - p)
                  p)
                (recur (inc i))))
            (recur (inc i))))))))

(defn max-flow
  "Returns the maximum source->sink flow."
  [{:keys [n adj]} source sink]
  (let [flow (atom 0)
        bfs (fn [lvl q l]
              (loop [qi 0 qe 1]
                (when (and (< qi qe) (zero? (nth @lvl sink)))
                  (let [v (nth @q qi)
                        qe (reduce
                            (fn [qe e]
                              (if (and (zero? (nth @lvl (:to e)))
                                       (pos? (bit-shift-right
                                              (long (- (:cap e) (:flow e)))
                                              (- 30 l))))
                                (do (swap! q assoc qe (:to e))
                                    (swap! lvl assoc (:to e)
                                           (inc (nth @lvl v)))
                                    (inc qe))
                                qe))
                            qe (nth @adj v))]
                    (recur (inc qi) qe)))))]
    (loop [l 0]
      (when (< l 31)
        (loop []
          (let [lvl (atom (assoc (vec (repeat n 0)) source 1))
                ptr (atom (vec (repeat n 0)))
                q (atom (assoc (vec (repeat n 0)) 0 source))]
            (bfs lvl q l)
            (loop [p (dfs-flow adj lvl ptr source sink Long/MAX_VALUE)]
              (when (pos? p)
                (swap! flow + p)
                (recur (dfs-flow adj lvl ptr source sink
                                 Long/MAX_VALUE))))
            (when (pos? (nth @lvl sink))
              (recur))))
        (recur (inc l))))
    @flow))
