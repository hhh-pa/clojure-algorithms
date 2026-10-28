(ns clojure-algorithms.maths.is-ipv6-address-valid-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.is-ipv6-address-valid :as sut]))

(deftest test-module-examples
  (is (= true (boolean (sut/is-ipv6-address-valid
                        "2001:0db8:85a3:0000:0000:8a2e:0370:7334"))))
  (is (= true (boolean (sut/is-ipv6-address-valid
                        "2001:db8:85a3:0:0:8A2E:0370:7334"))))
  (is (= true (boolean (sut/is-ipv6-address-valid
                        "2001:db8:85a3::8A2E:0370:7334"))))
  (is (= true (boolean (sut/is-ipv6-address-valid "::1"))))
  (is (= true (boolean (sut/is-ipv6-address-valid "2001:db8::"))))
  (is (= true (boolean (sut/is-ipv6-address-valid "::"))))
  (is (= false (boolean (sut/is-ipv6-address-valid ""))))
  (is (= false (boolean (sut/is-ipv6-address-valid
                         "2001:db8:85a3:::8a2e:370:7334"))))
  (is (= false (boolean (sut/is-ipv6-address-valid
                         "2001:db8:85a3:0:0:8a2e:370:7334:abcd"))))
  (is (= false (boolean (sut/is-ipv6-address-valid
                         "2001:db8:85a3:0:0:8a2e:0370:g334"))))
  (is (= false (boolean (sut/is-ipv6-address-valid
                         "2001:db8:85a3:0:0:8a2e:0370:"))))
  (is (= false (boolean (sut/is-ipv6-address-valid ":2001:db8::1"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "a::b:c:d:e:f:g:h"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "1::1:1:1:1:1:1:1")))))

(deftest test-fn-is-valid-group-examples
  (is (= true (boolean (sut/-is-valid-group "1"))))
  (is (= true (boolean (sut/-is-valid-group "abcd"))))
  (is (= false (boolean (sut/-is-valid-group "12345"))))
  (is (= false (boolean (sut/-is-valid-group "g")))))
