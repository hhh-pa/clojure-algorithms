(ns clojure-algorithms.maths.is-ip-v4-address-valid
  "wiki: https://en.wikipedia.org/wiki/IPv4

Is IP v4 address valid?
A valid IP address must be four octets in the form of A.B.C.D,
where A, B, C and D are numbers from 0-255."
  (:require [clojure.string :as str]))

(defn- valid-octet?
  "Checks one dotted-decimal octet: all digits, no leading zeros, 0-255."
  [octet]
  (and (re-matches #"\d+" octet)
       (= (str (Long/parseLong octet)) octet)
       (<= 0 (Long/parseLong octet) 255)))

(defn is-ip-v4-address-valid
  "Returns true if the string is a valid IPv4 address, false otherwise."
  [ip]
  (let [octets (str/split ip #"\." -1)]
    (boolean (and (= 4 (count octets)) (every? valid-octet? octets)))))
