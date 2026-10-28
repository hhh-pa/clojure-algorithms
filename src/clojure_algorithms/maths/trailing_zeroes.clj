(ns clojure-algorithms.maths.trailing-zeroes
  "https://en.wikipedia.org/wiki/Trailing_zero"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare trailing-zeroes)

(defn trailing-zeroes "Finding the Trailing Zeroes i.e. zeroes present at the end of number\nArgs:\n    num: A integer.\nReturns:\n    No. of zeroes in the end of an integer.\n\n>>> trailing_zeroes(1000)\n3\n>>> trailing_zeroes(102983100000)\n5\n>>> trailing_zeroes(0)\n1\n>>> trailing_zeroes(913273)\n0"
  [num] (let [num (atom num)] (let [ans (atom nil)] (try (reset! ans 0) (when (py-truthy (py-lt @num 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-neg 1)}))) (when (py-truthy (py-eq @num 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))) (try (while (py-truthy (py-gt @num 0)) (if (py-truthy (py-eq (py-mod @num 10) 0)) (do (swap! ans py-add 1)) (do (throw (ex-info "__break__" {:__control__ true :__break__ true})))) (swap! num py-floordiv 10)) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @ans})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
