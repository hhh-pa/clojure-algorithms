(ns clojure-algorithms.data-structures.stacks.stack-tracking-min-max-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.stacks.stack-tracking-min-max
             :refer [make-min-max-stack push-value pop-value get-current-max
                     get-current-min]]))

(deftest test-min-max-stack
  (let [st (make-min-max-stack 3)
        [ok st] (push-value st 33)
        [_ st] (push-value st 22)
        [_ st] (push-value st 44)]
    (is (true? ok))
    (is (= 44 (get-current-max st)))
    (is (= 22 (get-current-min st)))
    (is (thrown? clojure.lang.ExceptionInfo (push-value st 11)))
    (let [[v st] (pop-value st)]
      (is (= 44 v))
      (is (= 33 (get-current-max st)))
      (is (= 22 (get-current-min st)))
      (let [[v st] (pop-value st)]
        (is (= 22 v))
        (is (= 33 (get-current-max st)))
        (is (= 33 (get-current-min st))))))
  (let [st (make-min-max-stack)]
    (is (= ##-Inf (get-current-max st)))
    (is (= ##-Inf (get-current-min st)))
    (is (= ##-Inf (first (pop-value st))))))
