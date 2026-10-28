(ns clojure-algorithms.neural-network.lstm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.lstm :as l]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(defn- approx-vec= [a b eps]
  (and (= (count a) (count b))
       (every? true? (map #(approx= %1 %2 eps) a b))))

(deftest lstm-test
  (testing "constructor fields"
    (let [m (l/make-lstm "abcde" :hidden-size 10 :epochs 5 :learning-rate 0.01 :seed 1)]
      (is (= 10 (:hidden-size m)))
      (is (= 5 (:epochs m)))
      (is (= 0.01 (:lr m)))))
  (testing "one-hot encoding shape"
    (let [m (l/make-lstm (apply str (repeat 50 "abcde")) :hidden-size 10 :seed 1)
          v (l/one-hot-encode m \a)]
      (is (= 5 (count v)))
      (is (= 1.0 (reduce + v)))))
  (testing "weight shapes"
    (let [m (l/make-lstm (apply str (repeat 50 "abcde")) :hidden-size 10 :seed 1)]
      (is (= [10 15] [(count (:wf m)) (count (first (:wf m)))]))
      (is (= 10 (count (:bf m))))
      (is (= [5 10] [(count (:wy m)) (count (first (:wy m)))]))
      (is (= 5 (count (:by m))))))
  (testing "sigmoid values and derivative"
    (let [out (l/sigmoid-act [1 2 3])]
      (is (approx-vec= [0.731 0.881 0.953] out 1e-3))
      (is (approx-vec= [0.197 0.105 0.045] (l/sigmoid-act out true) 1e-3))))
  (testing "tanh values and derivative"
    (let [out (l/tanh-act [1 2 3])]
      (is (approx-vec= [0.762 0.964 0.995] out 1e-3))
      (is (approx-vec= [0.42 0.071 0.01] (l/tanh-act out true) 1e-2))))
  (testing "softmax sums to one"
    (let [out (l/softmax [1 2 3])]
      (is (approx-vec= [0.09 0.245 0.665] out 1e-3))
      (is (approx= 1.0 (reduce + out) 1e-9))))
  (testing "forward pass preserves length"
    (let [m (l/make-lstm (apply str (repeat 50 "abcde")) :hidden-size 4 :seed 1)
          inputs (mapv #(l/one-hot-encode m %) "abcde")
          [_ outs] (l/forward-pass m inputs)]
      (is (= 5 (count outs)))
      (is (every? #(= 5 (count %)) outs))))
  (testing "train then test-model returns a string of target length"
    (let [m (l/make-lstm (apply str (repeat 10 "abcde")) :hidden-size 4 :epochs 1 :seed 1)
          trained (l/train m)
          out (l/test-model trained)]
      (is (string? out))
      (is (= (count (:target-seq trained)) (count out))))))
