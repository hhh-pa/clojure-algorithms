(ns clojure-algorithms.strings.is-valid-email-address
  "Email address validation.
  See https://en.wikipedia.org/wiki/Email_address
  Ported from TheAlgorithms/Python."
  (:require [clojure.string :as str]))

(def email-tests
  [["simple@example.com" true]
   ["very.common@example.com" true]
   ["disposable.style.email.with+symbol@example.com" true]
   ["other-email-with-hyphen@and.subdomains.example.com" true]
   ["fully-qualified-domain@example.com" true]
   ["user.name+tag+sorting@example.com" true]
   ["x@example.com" true]
   ["example-indeed@strange-example.com" true]
   ["test/test@test.com" true]
   ["123456789012345678901234567890123456789012345678901234567890123@example.com"
    true]
   ["admin@mailserver1" true]
   ["example@s.example" true]
   ["Abc.example.com" false]
   ["A@b@c@example.com" false]
   ["abc@example..com" false]
   ["a(c)d,e:f;g<h>i[j\\k]l@example.com" false]
   ["12345678901234567890123456789012345678901234567890123456789012345@example.com"
    false]
   ["i.like.underscores@but_its_not_allowed_in_this_part" false]
   ["" false]])

(def max-local-part-octets 64)
(def max-domain-octets 255)

(def ^:private local-allowed
  (set (str "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.(!#$%&'*+-/=?^_`{|}~)")))

(def ^:private domain-allowed
  (set (str "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.-")))

(defn is-valid-email-address
  "True when `email` is a syntactically valid email address."
  [email]
  (boolean
   (and (= 1 (count (filter #(= \@ %) email)))
        (let [[local domain] (str/split email #"@" -1)]
          (and (<= (count local) max-local-part-octets)
               (<= (count domain) max-domain-octets)
               (every? #(contains? local-allowed %) local)
               (not (str/starts-with? local "."))
               (not (str/ends-with? local "."))
               (not (str/includes? local ".."))
               (every? #(contains? domain-allowed %) domain)
               (not (str/starts-with? domain "-"))
               (not (str/ends-with? domain "."))
               (not (str/starts-with? domain "."))
               (not (str/includes? domain "..")))))))
