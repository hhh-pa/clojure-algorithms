(ns clojure-algorithms.data-structures.stacks.stack-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.stack
             :refer [make-stack stack-bool stack-str stack-push stack-pop
                     stack-peek stack-empty? stack-full? stack-size
                     stack-contains?]]))

(deftest test-stack-basic
  (let [st (-> (make-stack 2) (stack-push 10) (stack-push 20))]
    (is (= "[10, 20]" (stack-str st))))
  (is (thrown? clojure.lang.ExceptionInfo
               (-> (make-stack 1) (stack-push 10) (stack-push 20))))
  (let [st (-> (make-stack) (stack-push -5) (stack-push 10))]
    (is (= 10 (first (stack-pop st))))
    (is (= 10 (stack-peek st))))
  (is (thrown? clojure.lang.ExceptionInfo (stack-pop (make-stack))))
  (is (thrown? clojure.lang.ExceptionInfo (stack-peek (make-stack))))
  (is (true? (stack-empty? (make-stack))))
  (is (false? (stack-empty? (stack-push (make-stack) 10))))
  (is (false? (stack-full? (make-stack))))
  (is (true? (stack-full? (stack-push (make-stack 1) 10))))
  (is (= 0 (stack-size (make-stack 3))))
  (is (= 1 (stack-size (stack-push (make-stack 3) 10))))
  (is (= 2 (stack-size (-> (make-stack 3) (stack-push 10) (stack-push 20)))))
  (is (true? (stack-contains? (stack-push (make-stack 3) 10) 10)))
  (is (false? (stack-contains? (stack-push (make-stack 3) 10) 20))))

(deftest test-stack-fill
  (let [st (make-stack 10)]
    (is (false? (stack-bool st)))
    (is (true? (stack-empty? st)))
    (is (false? (stack-full? st)))
    (is (= "[]" (stack-str st))))
  (let [st (reduce stack-push (make-stack 10) (range 10))]
    (is (true? (stack-bool st)))
    (is (false? (stack-empty? st)))
    (is (true? (stack-full? st)))
    (is (= "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]" (stack-str st)))))
