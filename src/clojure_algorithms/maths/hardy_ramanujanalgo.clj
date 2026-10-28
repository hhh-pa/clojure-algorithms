(ns clojure-algorithms.maths.hardy-ramanujanalgo
  "Port of maths/hardy_ramanujanalgo.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare exact-prime-factor-count)

(defn exact-prime-factor-count ">>> exact_prime_factor_count(51242183)\n3"
  [n] (let [n (atom n)] (let [count-v (atom nil) i (atom nil)] (try (reset! count-v 0) (when (py-truthy (py-eq (py-mod @n 2) 0)) (swap! count-v py-add 1) (while (py-truthy (py-eq (py-mod @n 2) 0)) (reset! n (py-int (py-div @n 2) nil)))) (reset! i 3) (while (py-truthy (py-le @i (py-int (Math/sqrt @n) nil))) (when (py-truthy (py-eq (py-mod @n @i) 0)) (swap! count-v py-add 1) (while (py-truthy (py-eq (py-mod @n @i) 0)) (reset! n (py-int (py-div @n @i) nil)))) (reset! i (py-add @i 2))) (when (py-truthy (py-gt @n 2)) (swap! count-v py-add 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @count-v})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
