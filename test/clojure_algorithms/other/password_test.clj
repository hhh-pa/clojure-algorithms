(ns clojure-algorithms.other.password-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.password :as p]
            [clojure.string :as str]))

(deftest password-generator-test
  (is (= 8 (count (p/password-generator))))
  (is (= 16 (count (p/password-generator 16))))
  (is (= 257 (count (p/password-generator 257))))
  (is (= "" (p/password-generator 0)))
  (is (= "" (p/password-generator -1))))

(deftest alternative-password-generator-test
  (testing "includes required chars at the requested length"
    (let [pw (p/alternative-password-generator "ab" 12)]
      (is (= 12 (count pw)))
      (is (str/includes? pw "a"))
      (is (str/includes? pw "b")))))

(deftest strong-password-test
  (is (true? (p/strong-password? "Hwea7$2!")))
  (is (false? (p/strong-password? "Sh0r1")))
  (is (false? (p/strong-password? "Hello123")))
  (is (true? (p/strong-password? "Hello1238udfhiaf038fajdvjjf!jaiuFhkqi1")))
  (is (false? (p/strong-password? "0"))))
