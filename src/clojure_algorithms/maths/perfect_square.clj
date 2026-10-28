(ns clojure-algorithms.maths.perfect-square
  "Port of maths/perfect_square.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare perfect-square perfect-square-binary-search)

(defn perfect-square "Check if a number is perfect square number or not\n:param num: the number to be checked\n:return: True if number is square number, otherwise False\n\n>>> perfect_square(9)\nTrue\n>>> perfect_square(16)\nTrue\n>>> perfect_square(1)\nTrue\n>>> perfect_square(0)\nTrue\n>>> perfect_square(10)\nFalse"
  [num] (py-eq (py-mul (Math/sqrt num) (Math/sqrt num)) num))

(defn perfect-square-binary-search "Check if a number is perfect square using binary search.\nTime complexity : O(Log(n))\nSpace complexity: O(1)\n\n>>> perfect_square_binary_search(9)\nTrue\n>>> perfect_square_binary_search(16)\nTrue\n>>> perfect_square_binary_search(1)\nTrue\n>>> perfect_square_binary_search(0)\nTrue\n>>> perfect_square_binary_search(10)\nFalse\n>>> perfect_square_binary_search(-1)\nFalse\n>>> perfect_square_binary_search(1.1)\nFalse\n>>> perfect_square_binary_search(\"a\")\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'int' and 'str'\n>>> perfect_square_binary_search(None)\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'int' and 'NoneType'\n>>> perfect_square_binary_search([])\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'int' and 'list'"
  [n] (let [left (atom nil) mid (atom nil) right (atom nil)] (try (reset! left 0) (reset! right n) (while (py-truthy (py-le @left @right)) (reset! mid (py-floordiv (py-add @left @right) 2)) (if (py-truthy (py-eq (py-pow @mid 2) n)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value true}))) (do (if (py-truthy (py-gt (py-pow @mid 2) n)) (do (reset! right (py-sub @mid 1))) (do (reset! left (py-add @mid 1))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
