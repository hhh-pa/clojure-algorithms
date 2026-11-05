(ns clojure-algorithms.ciphers.affine-cipher-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.affine-cipher :as c]))
(deftest affine-test
  (is (= "VL}p MM{I}p~{HL}Gp{vp pFsH}pxMpyxIx JHL O}F{~pvuOvF{FuF{xIp~{HL}Gi"
         (c/encrypt-message 4545 (str "The affine cipher is a type of monoalphabetic "
                                      "substitution cipher."))))
  (is (= "The affine cipher is a type of monoalphabetic substitution cipher."
         (c/decrypt-message 4545 (str "VL}p MM{I}p~{HL}Gp{vp pFsH}pxMpyxIx JHL O}F{~pvuOvF{FuF"
                                      "{xIp~{HL}Gi"))))
  (let [k (c/get-random-key) msg "This is a test!"]
    (is (= msg (c/decrypt-message k (c/encrypt-message k msg))))))
(deftest random-key-never-weak-test
  ;; Regression: get-random-key admitted k = 1 (~1/73 draws), which
  ;; check-keys rejects in encrypt mode, flaking this namespace.
  (doseq [k (repeatedly 500 c/get-random-key)]
    (let [[a _] ((juxt quot rem) k (count c/symbols))]
      (is (pos? a))
      (is (not= 1 a)))))
