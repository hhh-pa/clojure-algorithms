(ns clojure-algorithms.ciphers.elgamal-key-generator
  "ElGamal key generation."
  (:require [clojure-algorithms.ciphers.cryptomath-module :as cryptomath]
            [clojure-algorithms.ciphers.rabin-miller :as rabin-miller]
            [clojure.java.io :as io]))

(defn- mod-pow [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defn primitive-root
  "Finds a pseudo primitive root of p by trial."
  [p-val]
  (loop []
    (let [g (+ 3 (rand-int (- p-val 3)))]
      (if (or (.equals ^BigInteger (mod-pow g 2 p-val) BigInteger/ONE)
              (.equals ^BigInteger (mod-pow g p-val p-val) BigInteger/ONE))
        (recur)
        g))))

(defn generate-key
  "Generates an ElGamal [public-key private-key] pair."
  [key-size]
  (let [p (rabin-miller/generate-large-prime key-size)
        e1 (primitive-root p)
        d (+ 3 (rand-int (- p 3)))
        e2 (cryptomath/find-mod-inverse
            (long (mod-pow e1 d p)) p)]
    [[key-size e1 e2 p] [key-size d]]))

(defn make-key-files
  "Writes public/private key files named <name>_pubkey.txt etc."
  [name key-size]
  (when (or (.exists (io/file (str name "_pubkey.txt")))
            (.exists (io/file (str name "_privkey.txt"))))
    (throw (ex-info "Key files already exist." {:name name})))
  (let [[[ks e1 e2 p] [_ d]] (generate-key key-size)]
    (spit (str name "_pubkey.txt") (str ks "," e1 "," e2 "," p))
    (spit (str name "_privkey.txt") (str ks "," d))))
