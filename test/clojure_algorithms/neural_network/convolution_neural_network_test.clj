(ns clojure-algorithms.neural-network.convolution-neural-network-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.convolution-neural-network :as cnn]))

(defn- test-net []
  (cnn/make-cnn [3 2 1] 2 8 4 2 0.2 0.2 (java.util.Random. 11)))

(defn- test-image [v]
  (vec (for [_ (range 6)] (vec (repeat 6 v)))))

(deftest cnn-test
  (testing "convolute shapes"
    (let [[focus maps] (cnn/convolute (test-net) (test-image 0.5))]
      (is (= 16 (count focus)))
      (is (= 9 (count (first focus))))
      (is (= [2 4 4] [(count maps) (count (first maps)) (count (first (first maps)))]))))
  (testing "pooling halves 4x4 maps"
    (let [[_ maps] (cnn/convolute (test-net) (test-image 0.5))
          pooled (cnn/pooling maps 2)]
      (is (= [2 2 2] [(count pooled) (count (first pooled)) (count (first (first pooled)))]))
      (is (= 8 (count (cnn/expand-features pooled))))))
  (testing "max pooling picks maxima"
    (let [pooled (cnn/pooling [[[1.0 2.0] [3.0 4.0]]] 2 :max)]
      (is (= [[[4.0]]] pooled))))
  (testing "predict returns rounded rows"
    (let [out (cnn/predict (test-net) [(test-image 0.5) (test-image 0.1)])]
      (is (= [2 2] [(count out) (count (first out))]))
      (is (every? #(<= 0.0 % 1.0) (flatten out)))))
  (testing "one training epoch runs and returns finite mse"
    (let [[_ mse] (cnn/train (test-net) 2 [(test-image 0.5) (test-image 0.1)]
                             [[1.0 0.0] [0.0 1.0]] 1 1e-9)]
      (is (Double/isFinite (double mse)))))
  (testing "save and read round-trip"
    (let [tmp (java.io.File/createTempFile "cnn-model-test" ".edn")
          path (.getAbsolutePath tmp)]
      (cnn/save-model (test-net) path)
      (let [back (cnn/read-model path)]
        (is (= [3 2] (:conv back)))
        (is (= 8 (:bp1 back))))
      (.delete tmp))))
