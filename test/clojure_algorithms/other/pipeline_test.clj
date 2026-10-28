(ns clojure-algorithms.other.pipeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.pipeline :as p]))

(deftest pipeline-test
  (testing "doctest examples"
    (let [pl (-> (p/make-pipeline)
                 (p/pipe #(+ % 1))
                 (p/pipe #(* % 2)))]
      (is (= 4 (p/run pl 1))))
    (let [pl (-> (p/make-pipeline)
                 (p/pipe #(* % %))
                 (p/pipe #(- % 3)))]
      (is (= 6 (p/run pl 3))))
    (let [pl (-> (p/make-pipeline)
                 (p/pipe #(map (fn [x] (* x x)) %))
                 (p/pipe #(filter even? %))
                 (p/pipe #(reduce + %)))]
      (is (= 20 (p/run pl [1 2 3 4])))))
  (testing "pipeline-fn"
    (is (= 4 ((p/pipeline-fn [(fn [x] (+ x 1)) (fn [x] (* x 2))]) 1))))
  (testing "empty pipeline is identity"
    (is (= 7 (p/run (p/make-pipeline) 7)))))
