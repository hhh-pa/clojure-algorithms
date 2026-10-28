(ns clojure-algorithms.data-structures.linked-list.skip-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.linked-list.skip-list
             :refer [make-node node->str make-skip-list random-level skip-find
                     skip-insert skip-insert* skip-delete skip-keys skip->str]]))

(deftest test-skip-node
  (is (= "Node(Key: 2)" (node->str (make-node "Key" 2))))
  (is (= 0 (count (:forward (make-node "Key" 2)))))
  (is (= "SkipList(level=0)" (skip->str (make-skip-list)))))

(deftest test-skip-insert-find
  (let [sl (-> (make-skip-list)
               (skip-insert "Key1" 3) (skip-insert "Key2" 12)
               (skip-insert "Key3" 41) (skip-insert "Key4" -19))]
    (is (= ["Key1" "Key2" "Key3" "Key4"] (skip-keys sl)))
    (is (= 3 (skip-find sl "Key1")))
    (is (= 12 (skip-find sl "Key2")))
    (is (= 41 (skip-find sl "Key3")))
    (is (= -19 (skip-find sl "Key4")))
    (is (nil? (skip-find sl "Missing")))
    (is (clojure.string/starts-with? (skip->str sl) "SkipList(level="))))

(deftest test-skip-override
  (let [sl (-> (make-skip-list)
               (skip-insert "Key1" 10) (skip-insert "Key1" 12)
               (skip-insert "Key5" 7) (skip-insert "Key7" 10)
               (skip-insert "Key10" 5) (skip-insert "Key7" 7)
               (skip-insert "Key5" 5) (skip-insert "Key10" 10))]
    (is (= 4 (count (skip-keys sl))))
    (is (= 12 (skip-find sl "Key1")))
    (is (= 7 (skip-find sl "Key7")))
    (is (= 5 (skip-find sl "Key5")))
    (is (= 10 (skip-find sl "Key10")))))

(deftest test-skip-delete
  (let [sl (-> (make-skip-list)
               (skip-insert* "a" 1 2) (skip-insert* "b" 2 1)
               (skip-insert* "c" 3 3))]
    (is (= ["a" "b" "c"] (skip-keys sl)))
    (let [sl (skip-delete sl "b")]
      (is (= ["a" "c"] (skip-keys sl)))
      (is (= 1 (skip-find sl "a")))
      (is (= 3 (skip-find sl "c")))
      (is (nil? (skip-find sl "b"))))
    (let [sl (skip-delete sl "zzz")]
      (is (= ["a" "b" "c"] (skip-keys sl))))))

(deftest test-random-level
  (let [sl (make-skip-list)]
    (is (every? #(<= 1 % 16) (repeatedly 50 #(random-level sl))))))
