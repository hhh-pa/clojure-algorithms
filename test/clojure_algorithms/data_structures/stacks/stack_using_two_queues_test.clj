(ns clojure-algorithms.data-structures.stacks.stack-using-two-queues-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.stack-using-two-queues
             :refer [make-stack stack-push stack-peek stack-pop]]))

(deftest test-stack-two-queues
  (let [st (-> (make-stack) (stack-push 1) (stack-push 2) (stack-push 3))]
    (is (= 3 (stack-peek st)))
    (let [[a st] (stack-pop st)]
      (is (= 3 a))
      (let [[b st] (stack-pop st)]
        (is (= 2 b))
        (is (= 1 (stack-peek st)))
        (let [st (stack-push st 4)]
          (is (= 4 (stack-peek st)))
          (let [[c st] (stack-pop st)
                [d st] (stack-pop st)]
            (is (= [4 1] [c d]))
            (is (nil? (stack-peek st)))
            (is (thrown? clojure.lang.ExceptionInfo (stack-pop st)))))))))
