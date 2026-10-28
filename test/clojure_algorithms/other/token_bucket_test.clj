(ns clojure-algorithms.other.token-bucket-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.token-bucket :as t]))

(deftest token-bucket-test
  (testing "starts full; requests consume tokens"
    (let [b (t/make-bucket 1 2 60000 0)]
      (is (true? (t/allow-request? b 0)))
      (is (true? (t/allow-request? b 0)))
      (is (false? (t/allow-request? b 0)))))
  (testing "refills after a full period"
    (let [b (t/make-bucket 1 2 60000 0)]
      (t/allow-request? b 0)
      (t/allow-request? b 0)
      (is (false? (t/allow-request? b 59999)))
      (is (true? (t/allow-request? b 60000)))))
  (testing "refill caps at capacity"
    (let [b (t/make-bucket 4 4 60000 0)]
      (is (true? (t/allow-request? b 3600000)))
      (is (= 3 (:tokens @b))))))
