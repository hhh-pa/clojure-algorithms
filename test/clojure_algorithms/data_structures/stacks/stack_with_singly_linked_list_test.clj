(ns clojure-algorithms.data-structures.stacks.stack-with-singly-linked-list-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.stack-with-singly-linked-list
             :refer [make-stack stack-empty? stack-str stack-push stack-pop
                     stack-top stack-clear]]))

(deftest test-stack-singly
  (let [st (make-stack)]
    (is (true? (stack-empty? st))))
  (let [st (-> (make-stack) (stack-push "c") (stack-push "b") (stack-push "a"))]
    (is (= "a->b->c" (stack-str st)))
    (is (= "a" (stack-top st)))
    (is (false? (stack-empty? st)))
    (is (nil? (stack-top (stack-clear st))))
    (let [[v st] (stack-pop st)]
      (is (= "a" v))
      (is (= "b->c" (stack-str st)))
      (let [st (stack-push st "a")]
        (is (= "a->b->c" (stack-str st))))))
  (is (thrown? clojure.lang.ExceptionInfo (stack-pop (make-stack))))
  (let [st (-> (make-stack) (stack-push 1) (stack-push "x"))]
    (is (= "x->1" (stack-str st)))))
