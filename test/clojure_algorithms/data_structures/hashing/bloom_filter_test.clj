(ns clojure-algorithms.data-structures.hashing.bloom-filter-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.hashing.bloom-filter
             :refer [make-bloom bloom-add bloom-exists? bitstring format-hash
                     estimated-error-rate]]))

(deftest test-bloom
  (let [bloom (make-bloom 8)]
    (is (= "00000000" (bitstring bloom)))
    (is (false? (bloom-exists? bloom "Titanic")))
    (let [bloom (bloom-add bloom "Titanic")]
      (is (= "01100000" (bitstring bloom)))
      (is (true? (bloom-exists? bloom "Titanic")))
      (let [bloom (bloom-add bloom "Avatar")]
        (is (true? (bloom-exists? bloom "Avatar")))
        (is (= "00000100" (format-hash bloom "Avatar")))
        (is (= "01100100" (bitstring bloom)))
        (is (= {"The Godfather" "00000101" "Interstellar" "00000011"
                "Parasite" "00010010" "Pulp Fiction" "10000100"}
               (into {} (map (fn [f] [f (format-hash bloom f)]))
                     ["The Godfather" "Interstellar" "Parasite"
                      "Pulp Fiction"])))
        (is (not (some #(bloom-exists? bloom %)
                       ["The Godfather" "Interstellar" "Parasite"
                        "Pulp Fiction"])))
        (is (true? (bloom-exists? bloom "Ratatouille")))
        (is (= "01100000" (format-hash bloom "Ratatouille")))
        (is (= 0.140625 (estimated-error-rate bloom)))
        (let [bloom (bloom-add bloom "The Godfather")]
          (is (= 0.25 (estimated-error-rate bloom)))
          (is (= "01100101" (bitstring bloom))))))))
