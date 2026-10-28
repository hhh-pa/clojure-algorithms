(ns clojure-algorithms.core-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.core :as core]))

(deftest greet-test
  (is (= "Hello, Clojure!" (core/greet)))
  (is (= "Hello, World!" (core/greet "World"))))