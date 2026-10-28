(ns clojure-algorithms.maths.is-ipv6-address-valid
  "Validate IPv6 addresses (full and compressed forms).

  References:
  - Wikipedia: https://en.wikipedia.org/wiki/IPv6_address
  - RFC 4291: https://tools.ietf.org/html/rfc4291"
  (:require [clojure.string :as str]))

(def -HEX-DIGITS
  (set "0123456789abcdefABCDEF"))

(defn -is-valid-group
  "Return true iff group is 1-4 hex digits."
  [group]
  (boolean (and (<= 1 (count group) 4) (every? -HEX-DIGITS group))))

(defn is-ipv6-address-valid
  "Return true iff address is a valid IPv6 address in full or compressed form."
  [address]
  (boolean
   (and (seq address)
        (not (and (str/starts-with? address ":")
                  (not (str/starts-with? address "::"))))
        (not (and (str/ends-with? address ":")
                  (not (str/ends-with? address "::"))))
        (not (str/includes? address ":::"))
        (let [sides (str/split address #"::" -1)
              double-colon-count (dec (count sides))]
          (and (<= double-colon-count 1)
               (if (zero? double-colon-count)
                 (let [parts (str/split address #":" -1)]
                   (and (= 8 (count parts))
                        (every? seq parts)
                        (every? -is-valid-group parts)))
                 (let [left (first sides)
                       right (second sides)
                       left-groups (if (empty? left)
                                     []
                                     (remove empty? (str/split left #":" -1)))
                       right-groups (if (empty? right)
                                      []
                                      (remove empty? (str/split right #":" -1)))]
                   (and (< (+ (count left-groups) (count right-groups)) 8)
                        (every? -is-valid-group (concat left-groups right-groups))))))))))
