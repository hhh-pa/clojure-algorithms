(ns clojure-algorithms.data-structures.binary-tree.serialize-deserialize-binary-tree
  "Port of data_structures/binary_tree/serialize_deserialize_binary_tree.py.
  Preorder serialization with \"null\" markers.")

(defn make-node
  "Create a node with integer VALUE and optional children."
  ([value] (make-node value nil nil))
  ([value left right]
   (when-not (integer? value)
     (throw (ex-info "Value must be an integer." {:type :type-error})))
   {:value value :left left :right right}))

(defn preorder-nodes
  "Preorder node sequence."
  [node]
  (when node
    (concat [node] (preorder-nodes (:left node))
            (preorder-nodes (:right node)))))

(defn node-count
  "Number of nodes."
  [node]
  (count (preorder-nodes node)))

(defn serialize
  "Preorder serialization, e.g. \"1,2,null,null,3,null,null\"."
  [node]
  (if (nil? node)
    "null"
    (str (:value node) "," (serialize (:left node)) ","
         (serialize (:right node)))))

(defn five-tree
  "The sample tree: 1 with 2 and 3(4, 5)."
  []
  (make-node 1 (make-node 2)
             (make-node 3 (make-node 4) (make-node 5))))

(defn deserialize
  "Rebuild a tree from SERIALIZE output."
  [data]
  (when (empty? data)
    (throw (ex-info "Data cannot be empty." {:type :value-error})))
  (let [tokens (atom (vec (.split ^String data ",")))]
    (letfn [(build []
              (let [value (first @tokens)]
                (swap! tokens subvec 1)
                (when-not (= value "null")
                  (let [node (make-node (Integer/parseInt value))
                        left (build)
                        right (build)]
                    (assoc node :left left :right right)))))]
      (build))))
