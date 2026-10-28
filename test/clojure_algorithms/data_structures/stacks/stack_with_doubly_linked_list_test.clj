(ns clojure-algorithms.data-structures.stacks.stack-with-doubly-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.stack-with-doubly-linked-list
             :refer [make-stack stack-empty? stack-push stack-pop stack-top
                     print-stack]]))

(deftest test-stack-doubly
  (let [st (make-stack)]
    (is (true? (stack-empty? st)))
    (is (= [nil []] [(first (stack-pop st)) (second (stack-pop st))]))
    (is (nil? (stack-top st))))
  (let [st (-> (make-stack) (stack-push 0) (stack-push 1) (stack-push 2)
               (stack-push 3))]
    (is (= 4 (count st)))
    (is (= 3 (stack-top st)))
    (is (false? (stack-empty? st)))
    (print-stack st)
    (let [[v st] (stack-pop st)]
      (is (= 3 v))
      (is (= 2 (stack-top st))))))
