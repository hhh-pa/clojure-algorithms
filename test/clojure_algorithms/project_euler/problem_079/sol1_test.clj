(ns clojure-algorithms.project-euler.problem-079.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-079.sol1 :refer [find-secret-passcode
                                                                       solution]]))

(deftest find-secret-passcode-test
  (is (= 123 (find-secret-passcode ["123"])))
  (is (= 1234 (find-secret-passcode ["123" "134"])))
  (is (= 12359 (find-secret-passcode ["135" "259" "235"])))
  (is (thrown? clojure.lang.ExceptionInfo
               (find-secret-passcode ["123" "321"]))))

(deftest solution-test
  (is (= 1234 (solution "123\n134"))))
