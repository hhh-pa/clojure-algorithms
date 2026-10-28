(ns clojure-algorithms.data-structures.hashing.hash-map-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.hashing.hash-map
             :refer [make-hash-map next-index hm-set hm-del hm-get hm-len
                     hm-keys hm-repr]]))

(deftest test-next-index
  (let [hm (make-hash-map 5)]
    (is (= 4 (next-index hm 3)))
    (is (= 1 (next-index hm 5)))
    (is (= 2 (next-index hm 6)))
    (is (= 0 (next-index hm 9)))))

(deftest test-set-get
  (let [hm (-> (make-hash-map 5) (hm-set 1 10) (hm-set 2 20) (hm-set 3 30))]
    (is (= 10 (hm-get hm 1)))
    (is (= 3 (hm-len hm)))
    (is (= #{1 2 3} (set (hm-keys hm)))))
  (let [hm (-> (make-hash-map 5) (hm-set 10 -10) (hm-set 20 -20))]
    (is (= -20 (hm-get hm 20))))
  (let [hm (hm-set (make-hash-map 5) -1 10)]
    (is (= 10 (hm-get hm -1))))
  (let [hm (-> (make-hash-map 5) (hm-set 1 10) (hm-set 1 20))]
    (is (= 20 (hm-get hm 1)))
    (is (= 1 (hm-len hm))))
  (let [hm (hm-set (make-hash-map 5) 1.5 10)]
    (is (= 10 (hm-get hm 1.5)))))

(deftest test-delete
  (let [hm (-> (make-hash-map 5)
               (hm-set 1 10) (hm-set 2 20) (hm-set 3 30) (hm-del 3))]
    (is (= 2 (hm-len hm)))
    (is (thrown? clojure.lang.ExceptionInfo (hm-get hm 3))))
  (let [hm (-> (make-hash-map 5) (hm-set 1 10) (hm-set 2 20) (hm-set 3 30))]
    (is (thrown? clojure.lang.ExceptionInfo (hm-del hm 4))))
  (let [hm (-> (make-hash-map 5)
               (hm-set -5 10) (hm-set 6 30) (hm-set -7 20) (hm-del -5))]
    (is (= 30 (hm-get hm 6)))
    (is (= 20 (hm-get hm -7)))))

(deftest test-resize
  (let [hm (reduce (fn [hm i] (hm-set hm i i)) (make-hash-map 100) (range 75))]
    (is (= 100 (count (:buckets hm))))
    (let [hm (hm-set hm 75 75)]
      (is (= 200 (count (:buckets hm))))
      (let [hm (hm-del hm 75)]
        (is (= 200 (count (:buckets hm))))
        (let [hm (hm-del hm 74)]
          (is (= 100 (count (:buckets hm))))
          (doseq [i (range 74)]
            (is (= i (hm-get hm i)))))))))

(deftest test-dict-parity
  (let [ops [[:set "key_a" "val_a"] [:set "key_b" "val_b"]
             [:del "key_a"] [:del "key_b"] [:set "key_a" "val_a"]
             [:del "key_a"] [:set "x" 1] [:set "y" 2]]
        [hm d] (reduce (fn [[hm d] [op k v]]
                         (case op
                           :set [(hm-set hm k v) (assoc d k v)]
                           :del [(hm-del hm k) (dissoc d k)]))
                       [(make-hash-map 4) {}] ops)]
    (is (= d (into {} (map (fn [k] [k (hm-get hm k)])) (hm-keys hm))))
    (is (= (count d) (hm-len hm)))))
