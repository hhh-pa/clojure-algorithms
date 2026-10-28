(ns clojure-algorithms.ciphers.rsa-key-generator
  "RSA key generation."
  (:require [clojure-algorithms.ciphers.cryptomath-module :as cryptomath]
            [clojure-algorithms.ciphers.rabin-miller :as rabin-miller]
            [clojure.java.io :as io]))

(defn- gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn generate-key
  "Generates an RSA [[n e] [n d]] pair with key-size-bit primes."
  [key-size]
  (let [p (rabin-miller/generate-large-prime key-size)
        q (rabin-miller/generate-large-prime key-size)
        n (* p q)
        phi (* (dec p) (dec q))
        e (loop []
            (let [candidate (+ (.shiftLeft BigInteger/ONE (dec key-size))
                               (BigInteger. (dec key-size) (java.util.Random.)))]
              (if (= 1 (bigint (gcd candidate phi)))
                candidate
                (recur))))
        d (cryptomath/find-mod-inverse e phi)]
    [[n e] [n d]]))

(defn make-key-files
  "Writes <name>_pubkey.txt and <name>_privkey.txt."
  [name key-size]
  (when (or (.exists (io/file (str name "_pubkey.txt")))
            (.exists (io/file (str name "_privkey.txt"))))
    (throw (ex-info "Key files already exist." {:name name})))
  (let [[[n e] [_ d]] (generate-key key-size)]
    (spit (str name "_pubkey.txt") (str key-size "," n "," e))
    (spit (str name "_privkey.txt") (str key-size "," n "," d))))
