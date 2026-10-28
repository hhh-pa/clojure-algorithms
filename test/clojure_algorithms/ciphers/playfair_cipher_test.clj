(ns clojure-algorithms.ciphers.playfair-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.playfair-cipher :as c]))
(deftest playfair-test
  (is (= "CFSUPM" (c/encode "Hello" "MONARCHY")))
  (is (= "DQZSBYFSDZFMFNLOHFDRSG" (c/encode "attack on the left flank" "EMERGENCY")))
  (is (= "AVXETX" (c/encode "Sorry!" "SPECIAL")))
  (is (= "UMBENF" (c/encode "Number 1" "NUMBER")))
  (is (= "OEMHQHVCHESUKE" (c/encode "Photosynthesis!" "THE SUN")))
  (is (= "FIREHAZARD" (c/decode "BMZFAZRZDH" "HAZARD")))
  (is (= "DRIVINGX" (c/decode "HNBWBPQT" "AUTOMOBILE")))
  (is (= "ATXTACKX" (c/decode "SLYSSAQS" "CASTLE"))))
