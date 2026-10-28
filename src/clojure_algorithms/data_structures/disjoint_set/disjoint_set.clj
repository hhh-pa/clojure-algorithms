(ns clojure-algorithms.data-structures.disjoint-set.disjoint-set
  "Port of data_structures/disjoint_set/disjoint_set.py.
  Union-find with union by rank and path compression.
  A set is {:nodes {data {:data :rank :parent}}} with parent by data.")

(defn make-set
  "Add DATA as its own set; returns the new set."
  [ds data]
  (assoc-in ds [:nodes data] {:data data :rank 0 :parent data}))

(defn- find-root [nodes data]
  (let [node (get nodes data)]
    (if (= (:parent node) data)
      [nodes data]
      (let [[nodes2 root] (find-root nodes (:parent node))]
        [(assoc-in nodes2 [data :parent] root) root]))))

(defn find-set
  "Vector [new-set root-node] for DATA with path compression."
  [ds data]
  (let [[nodes root] (find-root (:nodes ds) data)]
    [(assoc ds :nodes nodes) (get nodes root)]))

(defn union-set
  "Union the sets of X and Y by rank; returns the new set."
  [ds x y]
  (let [[ds xnode] (find-set ds x)
        [ds ynode] (find-set ds y)]
    (if (= (:data xnode) (:data ynode))
      ds
      (cond
        (> (:rank xnode) (:rank ynode))
        (assoc-in ds [:nodes (:data ynode) :parent] (:data xnode))
        :else
        (let [ds (assoc-in ds [:nodes (:data xnode) :parent] (:data ynode))]
          (if (= (:rank xnode) (:rank ynode))
            (update-in ds [:nodes (:data ynode) :rank] inc)
            ds))))))

(defn find-python-set
  "Demo helper: the hardcoded set (of {0,1,2} {3,4,5}) containing DATA."
  [data]
  (let [sets [#{0 1 2} #{3 4 5}]]
    (or (some #(when (contains? % data) %) sets)
        (throw (ex-info (str data " is not in " sets) {:type :value-error})))))
