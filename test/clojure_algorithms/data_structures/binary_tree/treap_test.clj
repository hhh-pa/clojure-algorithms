(ns clojure-algorithms.data-structures.binary-tree.treap-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.treap
             :refer [treap-insert treap-erase treap-split treap-merge
                     inorder-keys interact-treap]]))

(deftest test-interact-treap
  (let [root (interact-treap nil "+1")]
    (is (= [1] (vec (inorder-keys root))))
    (let [root (interact-treap root "+3 +5 +17 +19 +2 +16 +4 +0")]
      (is (= [0 1 2 3 4 5 16 17 19] (vec (inorder-keys root))))
      (let [root (interact-treap root "+4 +4 +4")]
        (is (= [0 1 2 3 4 4 4 4 5 16 17 19] (vec (inorder-keys root))))
        (let [root (interact-treap root "-0")]
          (is (= [1 2 3 4 4 4 4 5 16 17 19] (vec (inorder-keys root))))
          (let [root (interact-treap root "-4")]
            (is (= [1 2 3 5 16 17 19] (vec (inorder-keys root))))
            (is (= [1 2 3 5 16 17 19]
                   (vec (inorder-keys (interact-treap root "=0")))))))))))

(deftest test-split-merge
  (let [root (reduce treap-insert nil [5 3 8 1 4])
        [l r] (treap-split root 4)]
    (is (= [1 3] (vec (inorder-keys l))))
    (is (= [4 5 8] (vec (inorder-keys r))))
    (is (= [1 3 4 5 8] (vec (inorder-keys (treap-merge l r))))))
  (is (= [1 2 3] (vec (inorder-keys
                       (treap-erase (reduce treap-insert nil [1 2 3 4]) 4))))))
