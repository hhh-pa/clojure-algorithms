(ns clojure-algorithms.machine-learning.multilayer-perceptron-classifier-test
  (:require [clojure-algorithms.machine-learning.multilayer-perceptron-classifier
             :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(def ^:private X [[0.0 0.0] [1.0 1.0] [1.0 0.0] [0.0 1.0]])
(def ^:private y [0 1 0 0])

(deftest test-dataloader
  (let [loader (sut/make-Dataloader X y)]
    (testing "train/test split shape"
      (is (= 4 (count (sut/Dataloader-get-train-test-data loader))))
      (let [[train-data train-labels test-data test-labels]
            (sut/Dataloader-get-train-test-data loader)]
        (is (= [[0.0 0.0] [1.0 1.0] [1.0 0.0]] train-data))
        (is (= [[0] [1] [0]] train-labels))
        (is (= [[0.0 1.0]] test-data))
        (is (= [[0]] test-labels))))
    (testing "one-hot encoding (ints and 1-vectors)"
      (is (approx [[0.99 0.0] [0.0 0.99] [0.99 0.0]]
                  (sut/Dataloader-one-hot-encode [0 1 0] 2)))
      (is (approx [[0.99 0.0 0.0] [0.0 0.0 0.99]]
                  (sut/Dataloader-one-hot-encode [0 2] 3)))
      (is (approx [[0.99 0.0 0.0] [0.0 0.99 0.0]]
                  (sut/Dataloader-one-hot-encode [[0] [1]] 3))))
    (testing "dims and shuffle passthrough"
      (is (= [2 3] (sut/Dataloader-get-inout-dim loader)))
      (is (= :x (sut/Dataloader-shuffle-data loader :x))))))

(deftest test-mlp-dims-and-init
  (let [mlp (sut/make-MLP (sut/make-Dataloader X y) 10 0.1)]
    (is (= [2 3] (sut/MLP-get-inout-dim mlp)))
    (let [[w1 w2] (sut/MLP-initialize mlp)]
      (is (= [3 2] [(count w1) (count (first w1))]))
      (is (= [2 3] [(count w2) (count (first w2))])))))

(deftest test-mlp-relu
  (let [mlp (sut/make-MLP nil 1 0.1)]
    (is (= [[0 2] [3 0]] (sut/MLP-relu mlp [[-1 2] [3 -4]])))
    (is (= [[0.0 1.0] [1.0 0.0]]
           (sut/MLP-relu-derivative mlp [[-1 2] [3 -4]])))))

(deftest test-mlp-forward
  (let [mlp (sut/make-MLP nil 1 0.1 1.0 2)
        x [[1.0 2.0 1.0]]
        w1 [[0.1 0.2] [0.3 0.4] [0.5 0.6]]
        w2 [[0.7 0.8] [0.9 1.0]]
        output (sut/MLP-forward mlp x w1 w2)]
    (is (= [1 2] [(count output) (count (first output))]))
    (is (approx [[2.28 2.56]] output))
    (testing "intermediates are cached for backprop"
      (is (= #{:z1 :a1 :z2 :a2} (set (keys @(:inter-variable mlp))))))
    (testing "no-gradient skips the cache"
      (reset! (:inter-variable mlp) {})
      (sut/MLP-forward mlp x w1 w2 true)
      (is (= {} @(:inter-variable mlp))))))

(deftest test-mlp-back-prop
  (let [mlp (sut/make-MLP nil 1 0.1 1.0 2)
        x [[1.0 2.0 1.0]]
        labels [[0.0 1.0]]
        w1 [[0.1 0.2] [0.3 0.4] [0.5 0.6]]
        w2 [[0.7 0.8] [0.9 1.0]]
        _ (sut/MLP-forward mlp x w1 w2)
        [grad-w1 grad-w2] (sut/MLP-back-prop mlp x labels w2)]
    (is (= [3 2] [(count grad-w1) (count (first grad-w1))]))
    (is (= [2 2] [(count grad-w2) (count (first grad-w2))]))
    (is (approx [[2.736 1.872] [3.648 2.496]] grad-w2))
    (is (approx [[2.844 3.612] [5.688 7.224] [2.844 3.612]] grad-w1))))

(deftest test-mlp-update-weights
  (let [mlp (sut/make-MLP nil 1 0.1)
        w1 [[0.1 0.2] [0.3 0.4] [0.5 0.6]]
        w2 [[0.7 0.8] [0.9 1.0]]
        [new-w1 new-w2] (sut/MLP-update-weights mlp w1 w2 w1 w2 0.1)]
    (is (approx [[0.09 0.18] [0.27 0.36] [0.45 0.54]] new-w1))
    (is (approx [[0.63 0.72] [0.81 0.9]] new-w2))))

(deftest test-mlp-learning-rate-and-loss
  (let [mlp (sut/make-MLP nil 1 0.1 0.9 2)]
    (is (approx 0.09 (sut/MLP-update-learning-rate mlp 0.1))))
  (let [label [[1 0] [0 1] [1 0]]
        y-hat [[0.9 0.1] [0.2 0.8] [0.6 0.4]]]
    (is (approx 1.0 (sut/MLP-accuracy label y-hat))))
  (let [output [[0.9 0.1] [0.2 0.8]]
        label [[1.0 0.0] [0.0 1.0]]]
    (is (approx 0.025 (sut/MLP-loss output label)))))

(deftest test-mlp-get-acc-loss
  (let [mlp (assoc (sut/make-MLP nil 1 0.1)
                   :test-accuracy [0.8 0.9] :test-loss [0.1 0.05])
        [acc loss] (sut/MLP-get-acc-loss mlp)]
    (is (= [0.8 0.9] acc))
    (is (= [0.1 0.05] loss))))

(deftest test-mlp-train
  (let [mlp (sut/make-MLP (sut/make-Dataloader X y) 2 0.1 1.0 2)
        trained (atom nil)
        out (with-out-str (reset! trained (sut/MLP-train mlp)))]
    (is (str/starts-with? out "Test accuracy: "))
    (is (= 2 (count (:test-accuracy @trained))))
    (is (= 2 (count (:test-loss @trained))))))
