(ns clojure-algorithms.machine-learning.rmsprop-test
  (:require [clojure-algorithms.machine-learning.rmsprop :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-module-docstring-examples
  (is (approx [-0.0316228] (sut/rmsprop [0.0] [0.1] 0.01)))
  (is (approx [0.9683772 -0.9683772] (sut/rmsprop [1.0 -1.0] [0.5 -0.5] 0.01))))

(deftest test-fn-rmsprop-examples
  (is (approx [0.0] (sut/rmsprop [0.0] [0.0] 0.01)))
  (is (approx [1.0] (sut/rmsprop [1.0] [0.0] 0.01)))
  (is (= 3 (count (sut/rmsprop [1.0 2.0 3.0] [0.1 0.2 0.3] 0.01))))
  (is (approx [0.9683772] (sut/rmsprop [1.0] [0.5] 0.01)))
  (testing "zero gradient leaves params unchanged"
    (is (approx [2.5 -3.0] (sut/rmsprop [2.5 -3.0] [0.0 0.0] 0.1)))))

(deftest test-fn-rmsprop-state
  (testing "atom moving average persists across calls (in-place update)"
    (let [avg (atom [0.0])
          once (sut/rmsprop [1.0] [0.5] 0.01 0.9 1e-8 avg)
          avg-after-once @avg
          twice (sut/rmsprop once [0.5] 0.01 0.9 1e-8 avg)]
      (is (approx [0.025] avg-after-once))
      (is (approx [0.0475] @avg))
      (is (< (first twice) (first once)))))
  (testing "sequential moving average is used as the initial value"
    (let [avg (atom [0.0])
          once (sut/rmsprop [1.0] [0.5] 0.01 0.9 1e-8 avg)
          via-atom (sut/rmsprop once [0.5] 0.01 0.9 1e-8 avg)
          via-seq (sut/rmsprop once [0.5] 0.01 0.9 1e-8 [0.025])]
      (is (approx via-atom via-seq)))))

(deftest test-fn-rmsprop-errors
  (is (thrown-with-msg? Exception #"same length"
                        (sut/rmsprop [1.0 2.0] [0.1] 0.01)))
  (is (= :ValueError (:type (try (sut/rmsprop [1.0] [0.1] 0.0)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e))))))
  (is (= :ValueError (:type (try (sut/rmsprop [1.0] [0.1] 0.01 1.0)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e))))))
  (is (= :ValueError (:type (try (sut/rmsprop [1.0] [0.1] 0.01 0.9 0.0)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e)))))))
