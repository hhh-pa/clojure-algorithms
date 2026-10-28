(ns clojure-algorithms.maths.fermat-little-theorem
  "Port of maths/fermat_little_theorem.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare binary-exponentiation p a b)

(defn binary-exponentiation "Calculate (a ** n) % mod using binary exponentiation, which runs in O(log n) time.\n\n>>> binary_exponentiation(2, 10, 17)\n4\n>>> binary_exponentiation(3, 0, 5)\n1\n>>> binary_exponentiation(5, 3, 13)\n8"
  [a n mod-v] (let [b (atom nil)] (try (if (py-truthy (py-eq n 0)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))) (do (if (py-truthy (py-eq (py-mod n 2) 1)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mod (py-mul (binary-exponentiation a (py-sub n 1) mod-v) a) mod-v)}))) (do (reset! b (binary-exponentiation a (py-floordiv n 2) mod-v)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mod (py-mul @b @b) mod-v)})))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(def p 701)

(def a 1000000000)

(def b 10)

(println (clojure.string/join " " [(py-str (py-eq (py-mod (py-div a b) p) (py-mod (py-mul a (binary-exponentiation b (py-sub p 2) p)) p)))]))

(println (clojure.string/join " " [(py-str (py-eq (py-mod (py-div a b) p) (py-mod (py-mul a (py-pow b (py-sub p 2))) p)))]))
