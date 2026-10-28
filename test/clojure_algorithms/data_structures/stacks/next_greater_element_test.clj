(ns clojure-algorithms.data-structures.stacks.next-greater-element-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.next-greater-element
             :refer [arr expect next-greatest-element-slow
                     next-greatest-element-fast next-greatest-element]]))

(deftest test-next-greater
  (is (= expect (next-greatest-element-slow arr)))
  (is (= expect (next-greatest-element-fast arr)))
  (is (= expect (next-greatest-element arr)))
  (is (= [-1] (next-greatest-element [5])))
  (is (= [] (next-greatest-element []))))
