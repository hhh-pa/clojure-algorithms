(ns clojure-algorithms.ciphers.enigma-machine2-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.ciphers.enigma-machine2 :as c]))
(deftest plugboard-test
  (is (= {\P \I \I \P \C \T \T \C \U \R \R \U \E \S \S \E}
         (c/plugboard "PICTURES")))
  (is (= {} (c/plugboard ""))))
(deftest enigma-test
  (is (= "KORYH JUHHI!" (c/enigma "Hello World!" [1 2 1]
                                  [c/rotor1 c/rotor2 c/rotor3] "pictures")))
  (is (= "HELLO, WORLD!" (c/enigma "KORYH, juhhi!" [1 2 1]
                                   [c/rotor1 c/rotor2 c/rotor3] "pictures")))
  (is (= "FPNCZ QWOBU!" (c/enigma "hello world!" [1 1 1]
                                  [c/rotor1 c/rotor2 c/rotor3] "pictures")))
  (let [enc (c/enigma "ATTACK AT DAWN" [5 10 20]
                      [c/rotor2 c/rotor4 c/rotor8] "ABCD")]
    (is (= "ATTACK AT DAWN" (c/enigma enc [5 10 20]
                                      [c/rotor2 c/rotor4 c/rotor8] "ABCD")))))
