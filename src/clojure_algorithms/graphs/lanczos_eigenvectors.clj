(ns clojure-algorithms.graphs.lanczos-eigenvectors
  "Largest eigenvalues/vectors of a graph adjacency list via Lanczos.")

(defn validate-adjacency-list
  "Throws when the graph is not a vector of neighbour-index vectors."
  [graph]
  (when-not (vector? graph)
    (throw (IllegalArgumentException. "Graph should be a list of lists.")))
  (doseq [[i neighbours] (map-indexed vector graph)]
    (when-not (vector? neighbours)
      (throw (IllegalArgumentException.
              (str "Node " i " should have a list of neighbors."))))
    (doseq [nb neighbours]
      (when-not (and (integer? nb) (<= 0 nb (dec (count graph))))
        (throw (IllegalArgumentException.
                (str "Invalid neighbor " nb " in node " i
                     " adjacency list.")))))))

(defn multiply-matrix-vector
  "Multiplies the adjacency-list graph by vector."
  [graph v]
  (when-not (= (count v) (count graph))
    (throw (IllegalArgumentException.
            "Vector length must match the number of nodes in the graph.")))
  (mapv (fn [neighbours] (reduce + 0 (map v neighbours))) graph))

(defn- dot [a b] (reduce + (map * a b)))
(defn- norm [a] (Math/sqrt (dot a a)))

(defn lanczos-iteration
  "Returns [tridiagonal orthonormal-basis]. Accepts :initial start vector."
  [graph k & {:keys [initial]}]
  (let [n (count graph)]
    (when-not (<= 1 k n)
      (throw (IllegalArgumentException.
              (str "Number of eigenvectors must be between 1 and the number "
                   "of nodes in the graph."))))
    (let [v0 (or initial (vec (repeatedly n rand)))
          v0 (mapv #(/ % (norm v0)) v0)
          basis (atom (assoc (vec (repeat k nil)) 0 v0))
          tri (atom (vec (repeat k (vec (repeat k 0.0)))))]
      (loop [j 0 prev-beta 0.0]
        (when (< j k)
          (let [qj (nth @basis j)
                r (multiply-matrix-vector graph qj)
                r (if (pos? j)
                    (mapv - r (mapv #(* prev-beta %) (nth @basis (dec j))))
                    r)
                alpha (dot qj r)
                r (mapv - r (mapv #(* alpha %) qj))
                beta (norm r)]
            (swap! tri assoc-in [j j] alpha)
            (when (< j (dec k))
              (swap! tri assoc-in [j (inc j)] beta)
              (swap! tri assoc-in [(inc j) j] beta)
              (when (> beta 1e-10)
                (swap! basis assoc (inc j) (mapv #(/ % beta) r))))
            (recur (inc j) beta))))
      [@tri @basis])))

(defn- jacobi-eigh
  "Eigendecomposition of symmetric matrix; [ascending-values column-vectors]."
  [a]
  (let [n (count a)
        a (atom (mapv #(mapv double %) a))
        v (atom (mapv (fn [i] (mapv #(if (= % i) 1.0 0.0) (range n)))
                      (range n)))]
    (dotimes [_ 100]
      (doseq [p (range n) q (range (inc p) n)]
        (let [apq (get-in @a [p q])]
          (when (> (Math/abs apq) 1e-12)
            (let [app (get-in @a [p p]) aqq (get-in @a [q q])
                  theta (/ (- aqq app) (* 2 apq))
                  t (/ (if (neg? theta) -1 1)
                       (+ (Math/abs theta)
                          (Math/sqrt (+ (* theta theta) 1))))
                  c (/ 1 (Math/sqrt (+ (* t t) 1)))
                  s (* c t)]
              (doseq [k (range n)]
                (when (and (not= k p) (not= k q))
                  (let [akp (get-in @a [k p]) akq (get-in @a [k q])]
                    (swap! a assoc-in [k p] (- (* c akp) (* s akq)))
                    (swap! a assoc-in [p k] (- (* c akp) (* s akq)))
                    (swap! a assoc-in [k q] (+ (* s akp) (* c akq)))
                    (swap! a assoc-in [q k] (+ (* s akp) (* c akq))))))
              (swap! a assoc-in [p p]
                     (+ (* c c app) (* s s aqq) (* -2 s c apq)))
              (swap! a assoc-in [q q]
                     (+ (* s s app) (* c c aqq) (* 2 s c apq)))
              (swap! a assoc-in [p q] 0.0)
              (swap! a assoc-in [q p] 0.0)
              (doseq [k (range n)]
                (let [vkp (get-in @v [k p]) vkq (get-in @v [k q])]
                  (swap! v assoc-in [k p] (- (* c vkp) (* s vkq)))
                  (swap! v assoc-in [k q] (+ (* s vkp) (* c vkq))))))))))
    [(mapv #(get-in @a [% %]) (range n))
     (mapv (fn [j] (mapv #(get-in @v [% j]) (range n))) (range n))]))

(defn find-lanczos-eigenvectors
  "Returns [descending-values eigenvector-columns] for the graph."
  [graph k & opts]
  (validate-adjacency-list graph)
  (let [[tri basis] (apply lanczos-iteration graph k opts)
        [vals vecs] (jacobi-eigh tri)
        order (vec (reverse (sort-by vals (range (count vals)))))
        desc-vals (mapv vals order)
        desc-vecs (mapv vecs order)
        full (mapv (fn [ev]
                     (reduce (fn [acc j]
                               (mapv + acc
                                     (mapv #(* (nth ev j) %) (nth basis j))))
                             (vec (repeat (count graph) 0.0))
                             (range k)))
                   desc-vecs)]
    [desc-vals full]))
