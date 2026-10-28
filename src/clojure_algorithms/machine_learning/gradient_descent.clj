(ns clojure-algorithms.machine-learning.gradient-descent
  "Implementation of gradient descent algorithm for minimizing cost of a linear hypothesis\nfunction."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare -error -hypothesis-value output calculate-hypothesis-value summation-of-cost-derivative get-cost-derivative run-gradient-descent test-gradient-descent train-data test-data parameter-vector m LEARNING-RATE)

(defn -error ":param data_set: train data or test data\n:param example_no: example number whose error has to be checked\n:return: error in example pointed by example number.\n\n>>> _error(0)  # hypothesis 39 minus train output 15\n24"
  ([example-no] (-error example-no "train"))
  ([example-no data-set] (py-sub (calculate-hypothesis-value example-no data-set) (output example-no data-set))))

(defn -hypothesis-value "Calculates hypothesis function value for a given input\n:param data_input_tuple: Input tuple of a particular example\n:return: Value of hypothesis function at that point.\nNote that there is an 'biased input' whose value is fixed as 1.\nIt is not explicitly mentioned in input data.. But, ML hypothesis functions use it.\nSo, we have to take care of it separately. Line 36 takes care of it.\n\n>>> _hypothesis_value((5, 2, 3))  # 5*4 + 2*1 + 3*5 + bias 2\n39"
  [data-input-tuple] (let [hyp-val (atom nil) i (atom nil)] (try (reset! hyp-val 0) (doseq [__i1 (py-iter (range (py-sub (count @parameter-vector) 1)))] (reset! i __i1) (swap! hyp-val py-add (py-mul (py-get data-input-tuple @i) (py-get @parameter-vector (py-add @i 1))))) (swap! hyp-val py-add (py-get @parameter-vector 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @hyp-val})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn output ":param data_set: test data or train data\n:param example_no: example whose output is to be fetched\n:return: output for that example\n\n>>> output(0, \"train\")\n15\n>>> output(1, \"test\")\n150\n>>> output(0, \"unknown\") is None\nTrue"
  [example-no data-set] (try (if (py-truthy (py-eq data-set "train")) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get train-data example-no) 1)}))) (do (when (py-truthy (py-eq data-set "test")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-get (py-get test-data example-no) 1)}))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn calculate-hypothesis-value "Calculates hypothesis value for a given example\n:param data_set: test data or train_data\n:param example_no: example whose hypothesis value is to be calculated\n:return: hypothesis value for that example\n\n>>> calculate_hypothesis_value(0, \"train\")\n39\n>>> calculate_hypothesis_value(0, \"test\")\n2149\n>>> calculate_hypothesis_value(0, \"unknown\") is None\nTrue"
  [example-no data-set] (try (if (py-truthy (py-eq data-set "train")) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (-hypothesis-value (py-get (py-get train-data example-no) 0))}))) (do (when (py-truthy (py-eq data-set "test")) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (-hypothesis-value (py-get (py-get test-data example-no) 0))}))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value nil})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn summation-of-cost-derivative "Calculates the sum of cost function derivative\n:param index: index wrt derivative is being calculated\n:param end: value where summation ends, default is m, number of examples\n:return: Returns the summation of cost derivative\nNote: If index is -1, this means we are calculating summation wrt to biased\n    parameter.\n\n>>> summation_of_cost_derivative(-1)  # sum of errors over all examples\n243\n>>> summation_of_cost_derivative(0)\n2234"
  ([index] (summation-of-cost-derivative index m))
  ([index end] (let [i (atom nil) summation-value (atom nil)] (try (reset! summation-value 0) (doseq [__i1 (py-iter (range end))] (reset! i __i1) (if (py-truthy (py-eq index (py-neg 1))) (do (swap! summation-value py-add (-error @i))) (do (swap! summation-value py-add (py-mul (-error @i) (py-get (py-get (py-get train-data @i) 0) index)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @summation-value})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(defn get-cost-derivative ":param index: index of the parameter vector wrt to derivative is to be calculated\n:return: derivative wrt to that index\nNote: If index is -1, this means we are calculating summation wrt to biased\n    parameter.\n\n>>> get_cost_derivative(-1)  # 243 / 5 examples\n48.6\n>>> get_cost_derivative(0)  # 2234 / 5 examples\n446.8"
  [index] (let [cost-derivative-value (atom nil)] (try (reset! cost-derivative-value (py-div (summation-of-cost-derivative index m) m)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @cost-derivative-value})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn run-gradient-descent
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn test-gradient-descent
  [] (let [i (atom nil)] (doseq [__i1 (py-iter (range (count test-data)))] (reset! i __i1) (println (clojure.string/join " " [(py-str ["Actual output value:" (output @i "test")])])) (println (clojure.string/join " " [(py-str ["Hypothesis output:" (calculate-hypothesis-value @i "test")])]))) nil))

(def train-data [[[5 2 3] 15] [[6 5 9] 25] [[11 12 13] 41] [[1 1 1] 8] [[11 12 13] 41]])

(def test-data [[[515 22 13] 555] [[61 35 49] 150]])

(def parameter-vector (atom [2 4 1 5]))

(def m (count train-data))

(def LEARNING-RATE 0.009)

;; NOTE: run_gradient_descent stubbed (np)
