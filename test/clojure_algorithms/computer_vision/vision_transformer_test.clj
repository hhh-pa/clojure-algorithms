(ns clojure-algorithms.computer-vision.vision-transformer-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.computer-vision.vision-transformer :as v]))
(defn- approx [a b] (< (Math/abs (double (- a b))) 1e-6))
(defn- rand-mat [rng r c]
  (vec (repeatedly r (fn [] (vec (repeatedly c (fn [] (.nextDouble ^java.util.Random rng))))))))
(deftest patches-test
  (let [img (vec (repeat 32 (vec (repeat 32 [0.1 0.2 0.3]))))
        [patches grid] (v/create-patches img 16)]
    (is (= [2 2] grid))
    (is (= 4 (count patches)))
    (is (thrown? IllegalArgumentException (v/create-patches img 15)))))
(deftest attention-test
  (let [rng (java.util.Random. 42)
        q (rand-mat rng 5 8) k (rand-mat rng 5 8) vv (rand-mat rng 5 8)
        [out w] (v/attention-mechanism q k vv)]
    (is (= [5 8] [(count out) (count (first out))]))
    (is (= [5 5] [(count w) (count (first w))]))
    (is (every? #(approx 1.0 (reduce + %)) w))))
(deftest norm-test
  (let [rng (java.util.Random. 7)
        e (rand-mat rng 4 8)
        n (v/layer-norm e)]
    (is (= [4 8] [(count n) (count (first n))]))
    (is (every? #(approx 0.0 (/ (reduce + %) (count %))) n))))
(deftest vit-test
  (let [rng (java.util.Random. 3)
        img (vec (repeatedly 8 (fn [] (vec (repeatedly 8 (fn [] (vec (repeatedly 3 (fn [] (.nextDouble rng))))))))))
        logits (v/vision-transformer img :patch-size 4 :embedding-dim 8
                                     :num-layers 1 :num-heads 2 :hidden-dim 16
                                     :num-classes 4 :rng (java.util.Random. 3))]
    (is (= 4 (count logits)))))
