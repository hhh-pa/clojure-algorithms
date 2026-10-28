(ns clojure-algorithms.machine-learning.frequent-pattern-growth-test
  (:require [clojure-algorithms.machine-learning.frequent-pattern-growth :as sut]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(def ^:private data-set
  [["A" "B" "C"] ["A" "C"] ["A" "B" "E"] ["A" "B" "C" "E"] ["B" "E"]])

(defn- child
  "Child node atom of node under name."
  [node & names]
  (reduce #(get (:children @%1) %2) node names))

(deftest test-tre-node-basics
  (let [parent (sut/make-TreeNode "Parent" 1 nil)
        child (sut/make-TreeNode "Child" 2 parent)]
    (is (= "Child" (:name @child)))
    (is (= 2 (:count @child)))
    (is (= "TreeNode('Child', 2, TreeNode('Parent', 1, None))"
           (sut/TreeNode-repr child))))
  (testing "inc mutates the count"
    (let [node (sut/make-TreeNode "A" 3 nil)]
      (is (nil? (sut/TreeNode-inc node 2)))
      (is (= 5 (:count @node)))))
  (testing "disp prints the subtree"
    (let [root (sut/make-TreeNode "Null Set" 1 nil)]
      (sut/update-tree ["A"] root {"A" [[1 nil] nil]} 1)
      (let [out (with-out-str (sut/TreeNode-disp root))]
        (is (str/includes? out "Null Set"))
        (is (str/includes? out "A"))))))

(deftest test-fn-create-tree-examples
  (let [[tree header] (sut/create-tree data-set 2)]
    (is (= "TreeNode('Null Set', 1, None)" (sut/TreeNode-repr tree)))
    (is (= 4 (count header)))
    (is (= [4 nil] (get-in header ["A" 0])))
    (is (= "TreeNode('A', 4, TreeNode('Null Set', 1, None))"
           (sut/TreeNode-repr (get-in header ["A" 1]))))
    (is (= "TreeNode('E', 1, TreeNode('B', 3, TreeNode('A', 4, TreeNode('Null Set', 1, None))))"
           (sut/TreeNode-repr (get-in header ["E" 1]))))
    (is (= ["A" "B" "C" "E"] (sort (keys header))))
    (is (= "Null Set" (:name @tree)))
    (is (= ["A" "B"] (sort (keys (:children @tree)))))
    (is (= "A" (:name @(child tree "A"))))
    (is (= ["B" "C"] (sort (keys (:children @(child tree "A")))))))
  (testing "nothing frequent yields an empty header"
    (let [[tree header] (sut/create-tree [["A"] ["B"]] 5)]
      (is (= {} header))
      (is (= "Null Set" (:name @tree))))))

(deftest test-fn-update-tree-examples
  (let [[tree header] (sut/create-tree data-set 2)
        header (sut/update-tree ["A" "B" "E"] tree header 1)
        e-node (child tree "A" "B" "E")]
    (is (= "TreeNode('Null Set', 1, None)" (sut/TreeNode-repr tree)))
    (is (= {} (:children @e-node)))
    (is (= 2 (:count @e-node)))
    (is (= "E" (:name @(get-in header ["E" 1]))))))

(deftest test-fn-update-header-examples
  (let [node1 (sut/make-TreeNode "A" 3 nil)
        node2 (sut/make-TreeNode "B" 4 nil)]
    (is (= "TreeNode('A', 3, None)" (sut/TreeNode-repr node1)))
    (let [result (sut/update-header node1 node2)]
      (is (= "TreeNode('A', 3, None)" (sut/TreeNode-repr result)))
      (is (= "TreeNode('B', 4, None)"
             (sut/TreeNode-repr (:node-link @result))))
      (is (nil? (:node-link @node2)))))
  (testing "appends at the end of a chain, returning the previous tail"
    (let [n1 (sut/make-TreeNode "A" 1 nil)
          n2 (sut/make-TreeNode "B" 1 nil)
          n3 (sut/make-TreeNode "C" 1 nil)]
      (sut/update-header n1 n2)
      (is (= n2 (sut/update-header n1 n3)))
      (is (= n3 (:node-link @n2))))))

(deftest test-fn-ascend-tree-examples
  (let [[tree _] (sut/create-tree data-set 2)]
    (is (= ["A"] (sut/ascend-tree (child tree "A") [])))
    (is (= ["C" "B" "A"] (sut/ascend-tree (child tree "A" "B" "C"))))))

(deftest test-fn-find-prefix-path-examples
  (let [[tree header] (sut/create-tree data-set 2)]
    (is (= {} (sut/find-prefix-path #{"A"} (child tree "A"))))
    (testing "conditional base of the E chain"
      (let [paths (sut/find-prefix-path #{"E"} (get-in header ["E" 1]))]
        (is (= #{#{"A" "B"} #{"B"} #{"A" "B" "C"}} (set (keys paths))))
        (is (every? #(= 1 %) (vals paths)))))))

(deftest test-fn-mine-tree-examples
  (let [[tree header] (sut/create-tree data-set 2)
        found (sut/mine-tree tree header 2 #{} [])]
    (doseq [expected [#{"C"} #{"C" "A"} #{"E"} #{"A" "E"} #{"E" "B"} #{"A"}
                      #{"B"}]]
      (is (contains? (set found) expected) (str "missing " expected)))))
