(ns clojure-algorithms.maths.is-ipv6-address-valid-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.is-ipv6-address-valid :as sut]))

(deftest test-module-examples
  (is (= true (boolean (sut/is-ipv6-address-valid "::"))))
  (is (= false (boolean (sut/is-ipv6-address-valid ""))))
  (is (= false (boolean (sut/is-ipv6-address-valid "2001:db8:85a3:::8a2e:370:7334"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "2001:db8:85a3:0:0:8a2e:370:7334:abcd"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "2001:db8:85a3:0:0:8a2e:0370:g334"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "2001:db8:85a3:0:0:8a2e:0370:"))))
  (is (= false (boolean (sut/is-ipv6-address-valid ":2001:db8::1"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "a::b:c:d:e:f:g:h"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "1::1:1:1:1:1:1:1")))))

(deftest test-fn-is-valid-group-examples
  (is (= false (boolean (sut/-is-valid-group "12345"))))
  (is (= false (boolean (sut/-is-valid-group "g")))))
