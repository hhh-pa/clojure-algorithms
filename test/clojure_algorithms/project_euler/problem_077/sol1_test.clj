(ns clojure-algorithms.project-euler.problem-077.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-077.sol1 :as p]))

(deftest partition-test
  (is (= #{32 36 21 25 30} (p/partition 10)))
  (is (= #{192 160 105 44 112 243 180 150 216 26 125 126}
         (p/partition 15)))
  (is (= 26 (count (p/partition 20)))))

(deftest solution-test
  (is (= 10 (p/solution 4))))
