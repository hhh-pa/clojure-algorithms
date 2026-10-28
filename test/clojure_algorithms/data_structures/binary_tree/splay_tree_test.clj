(ns clojure-algorithms.data-structures.binary-tree.splay-tree-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.binary-tree.splay-tree
             :refer [splay-insert splay-search splay-delete inorder-keys]]))

(deftest test-splay-basics
  (let [t (-> nil (splay-insert 10) (splay-insert 20) (splay-insert 30))]
    (is (= 30 (:key t)))
    (let [[found t2] (splay-search t 10)]
      (is (true? found))
      (is (= 10 (:key t2))))
    (let [[found _] (splay-search t 99)]
      (is (false? found)))
    (is (= [10 20 30] (vec (inorder-keys t))))))

(deftest test-splay-duplicates
  (let [t (reduce splay-insert nil [5 3 8 3])]
    (is (= [3 5 8] (vec (inorder-keys t))))
    (is (= 3 (:key t)))))

(deftest test-splay-search-missing
  (let [[found _] (splay-search nil 1)]
    (is (false? found)))
  (let [t (reduce splay-insert nil [40 20 60])
        [found t2] (splay-search t 20)]
    (is (true? found))
    (is (= 20 (:key t2)))))

(deftest test-splay-delete
  (let [t (reduce splay-insert nil [10 20 30 40])
        t (splay-delete t 20)]
    (is (= [10 30 40] (vec (inorder-keys t))))
    (let [t (splay-delete t 99)]
      (is (= [10 30 40] (vec (inorder-keys t))))
      (let [t (reduce splay-delete t [10 30 40])]
        (is (= [] (vec (inorder-keys t))))))))

(deftest test-splay-order
  (let [t (reduce splay-insert nil [9 4 7 1 2])]
    (is (= [1 2 4 7 9] (vec (inorder-keys t))))))
