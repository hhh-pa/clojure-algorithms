(ns clojure-algorithms.strings.detecting-english-programmatically-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.detecting-english-programmatically
             :refer [remove-non-letters is-english]]))

(deftest test-remove-non-letters
  (is (= "Hi how are you" (remove-non-letters "Hi! how are you?")))
  (is (= "Python" (remove-non-letters "P^y%t)h@o*n")))
  (is (= "" (remove-non-letters "1+1=2")))
  (is (= "wwwgooglecom" (remove-non-letters "www.google.com/")))
  (is (= "" (remove-non-letters ""))))

(deftest test-is-english
  (is (true? (is-english "Hello World")))
  (is (false? (is-english "llold HorWd"))))
