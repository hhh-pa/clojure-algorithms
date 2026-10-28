(ns clojure-algorithms.file-transfer.send-file-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.file-transfer.send-file :as sf]))

(deftest send-file-stub-test
  (testing "network-dependent stub explains itself"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Not ported"
                          (sf/send-file)))
    (try
      (sf/send-file "mytext.txt")
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :network-dependent (:reason (ex-data e))))))))
