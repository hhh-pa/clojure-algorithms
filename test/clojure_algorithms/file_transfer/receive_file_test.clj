(ns clojure-algorithms.file-transfer.receive-file-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.file-transfer.receive-file :as rf]))

(deftest receive-file-stub-test
  (testing "network-dependent stub explains itself"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Not ported"
                          (rf/receive-file)))
    (try
      (rf/receive-file)
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :network-dependent (:reason (ex-data e))))))))
