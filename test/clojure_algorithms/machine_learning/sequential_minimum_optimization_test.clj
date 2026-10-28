(ns clojure-algorithms.machine-learning.sequential-minimum-optimization-test
  (:require [clojure-algorithms.machine-learning.sequential-minimum-optimization
             :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(deftest test-kernel-construction
  (testing "defaults and validation"
    (let [k (sut/make-Kernel "linear")]
      (is (= "linear" (sut/Kernel-repr k)))
      (is (= :linear (sut/Kernel-get-kernel k "linear"))))
    (is (= "rbf" (sut/Kernel-repr (sut/make-Kernel "rbf" 1.0 0.0 0.5))))
    (is (= :KeyError (:type (try (sut/make-Kernel "sigmoid")
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e))))))
    (is (= :ValueError (:type (try (sut/make-Kernel "rbf" 1.0 0.0 -1.0)
                                   (catch clojure.lang.ExceptionInfo e
                                     (ex-data e))))))
    (is (nil? (sut/Kernel-check (sut/make-Kernel "linear"))))))

(deftest test-kernel-evaluation
  (let [lin (sut/make-Kernel "linear")
        poly (sut/make-Kernel "poly" 3.0 1.0 0.5)
        rbf (sut/make-Kernel "rbf" 1.0 0.0 0.5)]
    (is (approx 11.0 (sut/Kernel-linear lin [1 2] [3 4])))
    (is (approx 11.0 (sut/Kernel-call lin [1 2] [3 4])))
    (is (approx 274.625 (sut/Kernel-polynomial poly [1 2] [3 4])))
    (is (approx (Math/exp -4.0) (sut/Kernel-rbf rbf [1 2] [3 4])))
    (is (approx (Math/exp -4.0) (sut/Kernel-call rbf [1 2] [3 4])))))

(defn- tiny-model
  "Two-sample model with identity kernel matrix and zero alphas."
  []
  {:c 1.0 :b 0.0 :tol 0.001 :eps 0.001
   :tags [1 -1] :samples [[0.0] [1.0]]
   :alphas [0.0 0.0]
   :Kernel (sut/make-Kernel "linear")
   :all-samples [0 1]
   :-K-matrix [[1.0 0.0] [0.0 1.0]]
   :error [0.0 0.0] :unbound []
   :auto-norm false})

(deftest test-smo-construction
  (testing "train split, tolerance clamp and kernel matrix"
    (let [model (sut/make-SmoSVM [[1 0.0] [-1 2.0]]
                                 (sut/make-Kernel "linear")
                                 nil 0.4 0.0 0.00001 false)]
      (is (= [1 -1] (:tags model)))
      (is (= [[0.0] [2.0]] (:samples model)))
      (is (= [0.0 0.0] (:alphas model)))
      (is (= 0.001 (:tol model)))
      (is (approx [[0.0 0.0] [0.0 4.0]] (:-K-matrix model)))
      (is (= 2 (sut/SmoSVM-length model)))))
  (testing "auto normalization scales to [0, 1]"
    (let [model (sut/make-SmoSVM [[1 0.0] [-1 2.0]]
                                 (sut/make-Kernel "linear"))]
      (is (approx [[0.0] [1.0]] (:samples model)))
      (is (= [0.0] (:min model)))
      (is (= [2.0] (:max model))))))

(deftest test-smo-k-and-e
  (let [model (tiny-model)]
    (is (approx 1.0 (sut/SmoSVM-k model 0 0)))
    (is (approx 0.0 (sut/SmoSVM-k model 0 1)))
    (testing "sample vectors evaluate the kernel directly"
      (is (approx 2.0 (sut/SmoSVM-k model 1 [2.0]))))
    (testing "bound error is g(xi) - yi"
      ;; alphas zero, b zero: g = 0, y = 1 -> -1
      (is (approx -1.0 (sut/SmoSVM-e model 0))))
    (testing "unbound error comes from the cache"
      (let [model (assoc model :alphas [0.5 0.0] :error [0.25 0.0])]
        (is (approx 0.25 (sut/SmoSVM-e model 0)))))))

(deftest test-smo-kkt-and-supports
  (let [model (tiny-model)]
    (is (true? (sut/SmoSVM-check-obey-kkt model 0)))
    (is (false? (sut/SmoSVM-is-unbound model 0)))
    (is (false? (sut/SmoSVM-is-support model 0)))
    (let [model (assoc model :alphas [0.5 0.0] :unbound [0])]
      (is (true? (sut/SmoSVM-is-unbound model 0)))
      (is (true? (sut/SmoSVM-is-support model 0)))
      (is (= [0] (sut/SmoSVM-unbound model)))
      (is (= [0] (sut/SmoSVM-support model))))))

(deftest test-smo-get-new-alpha
  (let [model (tiny-model)]
    (testing "identical indices cannot step"
      (is (= [nil nil] (sut/SmoSVM-get-new-alpha model 0 0 0 0 0 0 1 1))))
    (testing "coincident bounds cannot step"
      ;; s = 1, a1 = a2 = 0, c = 1: L = H = 0
      (is (= [nil nil] (sut/SmoSVM-get-new-alpha model 0 1 0.0 0.0 1.0 -1.0
                                                 1 1))))
    (testing "eta path clips to the bounds"
      ;; K = I, y = [1 -1]: s = -1, L = 0, H = 1, eta = 2
      ;; e1 = 2, e2 = 0 -> a2unc = -0.5 -> clips to 0
      (is (approx [0.0 0.0]
                  (sut/SmoSVM-get-new-alpha model 0 1 0.5 0.5 2.0 0.0 1 -1)))
      ;; e1 = -2 -> a2unc = 1.5 -> clips to 1, a1 moves to 1
      (is (approx [1.0 1.0]
                  (sut/SmoSVM-get-new-alpha model 0 1 0.5 0.5 -2.0 0.0 1 -1))))))

(deftest test-smo-predict
  (testing "feature mismatch raises"
    (let [model (tiny-model)]
      (is (= :ValueError
             (:type (try (sut/SmoSVM-predict model [[1.0 2.0]])
                         (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
  (testing "hand-set hyperplane classifies"
    ;; linear kernel: f(x) = -x + 0.5, positive left of 0.5
    (let [model (assoc (tiny-model)
                       :alphas [0.0 1.0] :b 0.5
                       :samples [[0.0] [1.0]]
                       :-K-matrix [[0.0 0.0] [0.0 1.0]])]
      (is (= [1 -1] (sut/SmoSVM-predict model [[0.0] [1.0]])))
      (is (approx [0.5 -0.5] (sut/SmoSVM-predict model [[0.0] [1.0]] false))))))

(deftest test-smo-norm
  (let [model (assoc (tiny-model) :init true)]
    (is (approx [[0.0] [0.5] [1.0]] (sut/SmoSVM-norm model [[2.0] [3.0] [4.0]]))))
  (let [model (assoc (tiny-model) :init false :min [0.0] :max [2.0])]
    (is (approx [[0.5] [1.0]] (sut/SmoSVM-norm model [[1.0] [2.0]])))))

(deftest test-count-time
  (let [wrapped (sut/count-time (fn [x] (* x 2)))
        result (atom nil)
        out (with-out-str (reset! result (wrapped 21)))]
    (is (= 42 @result))
    (is (str/includes? out "SMO algorithm cost"))))

(deftest test-smo-stubs
  (doseq [[f args] [[sut/SmoSVM-fit [(tiny-model)]]
                    [sut/SmoSVM-choose-alphas [(tiny-model)]]
                    [sut/SmoSVM-choose-a1 [(tiny-model)]]
                    [sut/SmoSVM-choose-a2 [(tiny-model) 0]]
                    [sut/test-cancer-data []]
                    [sut/test-demonstration []]
                    [sut/test-linear-kernel [nil 1]]
                    [sut/test-rbf-kernel [nil 1]]
                    [sut/plot-partition-boundary [nil nil nil nil]]]]
    (is (= :UnsupportedOperation
           (:type (try (apply f args)
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
