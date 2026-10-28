(ns clojure-algorithms.maths.perfect-cube
  "Port of maths/perfect_cube.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare perfect-cube perfect-cube-binary-search)

(defn perfect-cube "Check if a number is a perfect cube or not.\n\n>>> perfect_cube(27)\nTrue\n>>> perfect_cube(4)\nFalse"
  [n] (let [val (atom nil)] (try (reset! val (py-pow n (py-div 1 3))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq (py-mul (py-mul @val @val) @val) n)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn perfect-cube-binary-search "Check if a number is a perfect cube or not using binary search.\nTime complexity : O(Log(n))\nSpace complexity: O(1)\n\n>>> perfect_cube_binary_search(27)\nTrue\n>>> perfect_cube_binary_search(64)\nTrue\n>>> perfect_cube_binary_search(4)\nFalse\n>>> perfect_cube_binary_search(\"a\")\nTraceback (most recent call last):\n    ...\nTypeError: perfect_cube_binary_search() only accepts integers\n>>> perfect_cube_binary_search(0.1)\nTraceback (most recent call last):\n    ...\nTypeError: perfect_cube_binary_search() only accepts integers"
  [n] (let [n (atom n)] (let [left (atom nil) mid (atom nil) right (atom nil)] (try (when (py-truthy (not (py-truthy (integer? @n)))) (throw (ex-info (py-str "perfect_cube_binary_search() only accepts integers") {:type :TypeError}))) (when (py-truthy (py-lt @n 0)) (reset! n (py-neg @n))) (reset! left 0) (reset! right @n) (while (py-truthy (py-le @left @right)) (reset! mid (py-add @left (py-floordiv (py-sub @right @left) 2))) (if (py-truthy (py-eq (py-mul (py-mul @mid @mid) @mid) @n)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value true}))) (do (if (py-truthy (py-lt (py-mul (py-mul @mid @mid) @mid) @n)) (do (reset! left (py-add @mid 1))) (do (reset! right (py-sub @mid 1))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
