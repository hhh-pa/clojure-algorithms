(ns clojure-algorithms.web-programming.random-anime-character-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.random-anime-character
             :refer [image-title random-anime-character]]))

(deftest test-pure-logic
  (is (= "Nezuko_Kamado.png"
         (image-title "Nezuko Kamado" "https://x/img/abc.png")))
  (is (= "A.jpg?x" (image-title " A " "https://x/a.jpg?x"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (random-anime-character))))
