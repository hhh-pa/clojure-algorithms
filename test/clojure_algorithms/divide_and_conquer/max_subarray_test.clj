(ns clojure-algorithms.divide-and-conquer.max-subarray-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.divide-and-conquer.max-subarray :as ms]))

(deftest max-subarray-test
  (testing "divide and conquer maximum subarray"
    (let [nums [-2 1 -3 4 -1 2 1 -5 4]]
      (is (= [3 6 6] (ms/max-subarray nums 0 (dec (count nums))))))
    (is (= [0 2 19] (ms/max-subarray [2 8 9] 0 2)))
    (is (= [0 0 0] (ms/max-subarray [0 0] 0 1)))
    (is (= [2 2 1.0] (ms/max-subarray [-1.0 0.0 1.0] 0 2)))
    (is (= [2 2 -1] (ms/max-subarray [-2 -3 -1 -4 -6] 0 4)))
    (is (= [nil nil 0] (ms/max-subarray [] 0 0)))))

(deftest max-cross-sum-test
  (testing "crossing sum agrees with the full result"
    (let [nums [-2 1 -3 4 -1 2 1 -5 4]
          [l h s] (ms/max-cross-sum nums 0 4 8)]
      (is (= s (reduce + (subvec (vec nums) l (inc h))))))))
