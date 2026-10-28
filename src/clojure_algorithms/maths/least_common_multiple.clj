(ns clojure-algorithms.maths.least-common-multiple
  "Port of maths/least_common_multiple.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure-algorithms.maths.greatest-common-divisor :as m1]))

(declare least-common-multiple-slow least-common-multiple-fast benchmark)

(defn least-common-multiple-slow "Find the least common multiple of two numbers.\n\nLearn more: https://en.wikipedia.org/wiki/Least_common_multiple\n\n>>> least_common_multiple_slow(5, 2)\n10\n>>> least_common_multiple_slow(12, 76)\n228\n>>> least_common_multiple_slow(0, 5)\n0\n>>> least_common_multiple_slow(5, 0)\n0\n>>> least_common_multiple_slow(0, 0)\n0"
  [first-num second-num] (let [common-mult (atom nil) max-num (atom nil)] (try (when (py-truthy (let [__b1 (py-eq first-num 0)] (if (py-truthy __b1) __b1 (py-eq second-num 0)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (reset! max-num (if (py-truthy (py-ge first-num second-num)) first-num second-num)) (reset! common-mult @max-num) (while (py-truthy (let [__b2 (py-gt (py-mod @common-mult first-num) 0)] (if (py-truthy __b2) __b2 (py-gt (py-mod @common-mult second-num) 0)))) (swap! common-mult py-add @max-num)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @common-mult})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn least-common-multiple-fast "Find the least common multiple of two numbers.\nhttps://en.wikipedia.org/wiki/Least_common_multiple#Using_the_greatest_common_divisor\n>>> least_common_multiple_fast(5,2)\n10\n>>> least_common_multiple_fast(12,76)\n228\n>>> least_common_multiple_fast(0, 5)\n0\n>>> least_common_multiple_fast(5, 0)\n0\n>>> least_common_multiple_fast(0, 0)\n0"
  [first-num second-num] (try (when (py-truthy (py-in 0 (set [first-num second-num]))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul (py-floordiv first-num (m1/greatest-common-divisor first-num second-num)) second-num)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn benchmark
  [& _args]
  ;; stub: original requires timeit; pure-Clojure port not applicable
  (throw (ex-info "requires timeit" {:type :UnsupportedOperation})))

;; NOTE: benchmark stubbed (timeit)
