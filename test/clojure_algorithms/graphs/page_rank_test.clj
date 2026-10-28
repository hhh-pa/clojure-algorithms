(ns clojure-algorithms.graphs.page-rank-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.graphs.page-rank
             :refer [make-node add-inbound add-outbound page-rank]]))

(deftest test-page-rank
  (let [a (-> (make-node "A") (add-inbound "C")
              (add-outbound "B") (add-outbound "C"))
        b (-> (make-node "B") (add-inbound "A") (add-outbound "C"))
        c (-> (make-node "C") (add-inbound "A") (add-inbound "B")
              (add-outbound "A"))
        ranks (page-rank [a b c])]
    (is (< (Math/abs (- (reduce + (vals ranks)) 1.0)) 1e-6))
    (doseq [[k v] {"A" 0.3878 "B" 0.2148 "C" 0.3974}]
      (is (< (Math/abs (- (get ranks k) v)) 0.001)))
    (is (> (get ranks "C") (get ranks "A") (get ranks "B")))))
