(ns clojure-algorithms.web-programming.daily-horoscope-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.daily-horoscope :refer [horoscope-url
                                                                        horoscope
                                                                        zodiac-signs]]))

(deftest test-pure-logic
  (is (= (str "https://www.horoscope.com/us/horoscopes/general/"
              "horoscope-general-daily-today.aspx?sign=5")
         (horoscope-url 5 "today")))
  (is (= "Leo" (zodiac-signs 5))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (horoscope 5 "today"))))
