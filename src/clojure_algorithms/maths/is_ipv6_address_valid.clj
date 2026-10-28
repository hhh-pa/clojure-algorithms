(ns clojure-algorithms.maths.is-ipv6-address-valid
  "Validate IPv6 addresses (full and compressed forms).\n\nReferences:\n- Wikipedia: https://en.wikipedia.org/wiki/IPv6_address\n- RFC 4291: https://tools.ietf.org/html/rfc4291\n\nRules implemented (subset of RFC 4291):\n- Hex groups are 1-4 hex digits (0-9, a-f, A-F).\n- Exactly 8 groups in total. A single '::' may compress one or more groups of\n  zeros. No more than one '::' is allowed.\n- No leading/trailing single ':' (e.g., ':1:2::' is invalid).\n- Does NOT support IPv4-mapped endings like '::ffff:192.0.2.128'.\n\nExamples (doctests):\n\n>>> is_ipv6_address_valid(\"2001:0db8:85a3:0000:0000:8a2e:0370:7334\")\nTrue\n>>> is_ipv6_address_valid(\"2001:db8:85a3:0:0:8A2E:0370:7334\")\nTrue\n>>> is_ipv6_address_valid(\"2001:db8:85a3::8A2E:0370:7334\")  # '::' compresses two groups\nTrue\n>>> is_ipv6_address_valid(\"::1\")  # loopback, 7 groups compressed\nTrue\n>>> is_ipv6_address_valid(\"2001:db8::\")  # trailing compression is OK\nTrue\n>>> is_ipv6_address_valid(\"::\")  # all groups compressed\nTrue\n\nInvalids:\n\n>>> is_ipv6_address_valid(\"\")  # empty\nFalse\n>>> is_ipv6_address_valid(\"2001:db8:85a3:::8a2e:370:7334\")  # multiple '::'\nFalse\n>>> is_ipv6_address_valid(\"2001:db8:85a3:0:0:8a2e:370:7334:abcd\")  # 9 groups\nFalse\n>>> is_ipv6_address_valid(\"2001:db8:85a3:0:0:8a2e:0370:g334\")  # non-hex char\nFalse\n>>> is_ipv6_address_valid(\"2001:db8:85a3:0:0:8a2e:0370:\")  # trailing ':'\nFalse\n>>> is_ipv6_address_valid(\":2001:db8::1\")  # leading ':'\nFalse\n>>> is_ipv6_address_valid(\"a::b:c:d:e:f:g:h\")  # '::' compresses zero groups (already 8)\nFalse\n>>> is_ipv6_address_valid(\"1::1:1:1:1:1:1:1\")  # same idea; zero groups compressed\nFalse"
  (:require [clojure.string :as str]))

(declare -is-valid-group is-ipv6-address-valid -HEX-DIGITS)

(declare py-dunder py-no-dunder py-truthy py-eq py-num-bool py-cmp-safe py-lt py-le py-gt py-ge py-in py-iter py-add py-set py-count py-split py-startswith py-endswith)

(def ^:private py-no-dunder (Object.))

(defmulti py-dunder (fn [op a & _] [op (class a)]))

(defmethod py-dunder :default [& _] py-no-dunder)

(defn- py-truthy [v]
  (cond (nil? v) false
        (false? v) false
        (true? v) true
        (number? v) (not (zero? v))
        (string? v) (not (empty? v))
        (instance? clojure.lang.IRecord v)
        (let [r (py-dunder :bool v)]
          (if (identical? r py-no-dunder)
            (let [l (py-dunder :len v)]
              (if (identical? l py-no-dunder) true (not (zero? l))))
            (boolean (py-truthy r))))
        (coll? v) (not (empty? v))
        :else true))

(defn- py-eq [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (string? a) (string? b)) (= a b)
        (and (instance? clojure.lang.IRecord a)
             (instance? clojure.lang.IRecord b)
             (= (class a) (class b)))
        (let [r (py-dunder :eq a b)]
          (if (identical? r py-no-dunder) (= a b) (boolean (py-truthy r))))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? (fn [[k v]] (and (contains? b k) (py-eq v (get b k)))) a))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map py-eq a b)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(defn- py-num-bool [x] (if (boolean? x) (if x 1 0) x))

(defn- py-cmp-safe [a b]
  (try (compare (py-num-bool a) (py-num-bool b))
       (catch ClassCastException e
         (throw (ex-info "'<' not supported between instances" {:type :TypeError})))))

(defn- py-lt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :lt a b)]
      (if (identical? r py-no-dunder) (neg? (compare a b)) (boolean (py-truthy r))))
    (neg? (py-cmp-safe a b))))

(defn- py-le [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :le a b)]
      (if (identical? r py-no-dunder) (not (pos? (compare a b))) (boolean (py-truthy r))))
    (not (pos? (py-cmp-safe a b)))))

(defn- py-gt [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :gt a b)]
      (if (identical? r py-no-dunder) (pos? (compare a b)) (boolean (py-truthy r))))
    (pos? (py-cmp-safe a b))))

(defn- py-ge [a b]
  (if (and (instance? clojure.lang.IRecord a)
           (instance? clojure.lang.IRecord b) (= (class a) (class b)))
    (let [r (py-dunder :ge a b)]
      (if (identical? r py-no-dunder) (not (neg? (compare a b))) (boolean (py-truthy r))))
    (not (neg? (py-cmp-safe a b)))))

(defn- py-in [x c]
  (cond (nil? c) false
        (string? c) (if (string? x)
                      (clojure.string/includes? c x)
                      (throw (ex-info "a string is required" {:type :TypeError})))
        (map? c) (contains? c x)
        (set? c) (contains? c x)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :contains c x)]
          (if (identical? r py-no-dunder) false (boolean (py-truthy r))))
        :else (boolean (some #(py-eq % x) c))))

(defn- py-iter [c]
  (cond (instance? clojure.lang.Atom c) (py-iter @c)
        (instance? clojure.lang.IRecord c)
        (let [r (py-dunder :iter c)]
          (if (identical? r py-no-dunder)
            (throw (ex-info "object is not iterable" {:type :TypeError})) r))
        (nil? c) []
        (map? c) (keys c)
        :else c))

(defn- py-add [a b]
  (let [a (py-num-bool a) b (py-num-bool b)]
    (cond (and (number? a) (number? b)) (+' a b)
          (and (string? a) (string? b)) (str a b)
          (and (sequential? a) (sequential? b)) (vec (concat a b))
          (instance? clojure.lang.IRecord a)
          (let [r (py-dunder :add a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          (instance? clojure.lang.IRecord b)
          (let [r (py-dunder :add-r a b)]
            (if (identical? r py-no-dunder)
              (throw (ex-info "unsupported operand for +" {:type :TypeError})) r))
          :else (throw (ex-info "unsupported operand for +" {:type :TypeError})))))

(defn- py-set [x]
  (cond (nil? x) #{}
        (instance? clojure.lang.Atom x) (let [v (set @x)] (reset! x []) v)
        (string? x) (set (map str (seq x)))
        (map? x) (set (keys x))
        (sequential? x) (set x)
        (set? x) x
        (instance? clojure.lang.IRecord x) (set (py-iter x))
        :else (throw (ex-info "object is not iterable" {:type :TypeError}))))

(defn- py-count [c x]
  (cond (and (string? c) (string? x))
        (if (empty? x) (inc (count c))
            (loop [n 0 i 0]
              (let [j (.indexOf ^String c ^String x (int i))]
                (if (neg? j) n (recur (inc n) (+ j (count x)))))))
        (sequential? c) (count (filter #(py-eq % x) c))
        :else (throw (ex-info "no attribute 'count'" {:type :AttributeError}))))

(defn- py-split
  ([s] (py-split s nil -1))
  ([s sep] (py-split s sep -1))
  ([s sep mx]
   (let [s (str s)]
     (if (nil? sep)
       (let [parts (vec (remove empty? (clojure.string/split s #"\s+")))]
         (if (or (neg? mx) (>= mx (count parts))) parts
             (let [head (clojure.string/join " " (take (- (count parts) mx) parts))]
               (vec (concat [head] (take-last mx parts))))))
       (do (when (empty? sep) (throw (ex-info "empty separator" {:type :ValueError})))
           (let [re (re-pattern (java.util.regex.Pattern/quote sep))]
             (if (neg? mx) (vec (.split ^String s (.pattern re) -1))
                 (vec (.split ^String s (.pattern re) (int (inc mx)))))))))))

(defn- py-startswith [s pre]
  (if (sequential? pre)
    (boolean (some #(clojure.string/starts-with? s (str %)) pre))
    (clojure.string/starts-with? s (str pre))))

(defn- py-endswith [s suf]
  (if (sequential? suf)
    (boolean (some #(clojure.string/ends-with? s (str %)) suf))
    (clojure.string/ends-with? s (str suf))))

(defn -is-valid-group "Return True iff group is 1-4 hex digits.\nArgs:\n    group (str): The hex group to validate.\n\nReturns:\n    bool: True if the group is 1-4 hex digits, False otherwise.\n\nExamples:\n\n>>> _is_valid_group(\"1\")\nTrue\n>>> _is_valid_group(\"abcd\")\nTrue\n>>> _is_valid_group(\"12345\")  # too long\nFalse\n>>> _is_valid_group(\"g\")  # non-hex character\nFalse"
  [group] (let [__b4 (let [__c1 1 __c2 (count group) __c3 4] (and (py-le __c1 __c2) (py-le __c2 __c3)))] (if (py-truthy __b4) (every? py-truthy (doall (for [ch (py-iter group)] (py-in ch -HEX-DIGITS)))) __b4)))

(defn is-ipv6-address-valid "Return True iff `address` is a valid IPv6 address in full or compressed form.\n\nSee module docstring for rules and doctests."
  [address] (let [double-colon-count (atom nil) left (atom nil) left-groups (atom nil) parts (atom nil) right (atom nil) right-groups (atom nil) total-groups (atom nil)] (try (when (py-truthy (not (py-truthy address))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (when (py-truthy (let [__b3 (let [__b1 (py-startswith address ":")] (if (py-truthy __b1) (not (py-truthy (py-startswith address "::"))) __b1))] (if (py-truthy __b3) __b3 (let [__b2 (py-endswith address ":")] (if (py-truthy __b2) (not (py-truthy (py-endswith address "::"))) __b2))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (when (py-truthy (py-in ":::" address)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (reset! double-colon-count (py-count address "::")) (when (py-truthy (py-gt @double-colon-count 1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value false}))) (when (py-truthy (py-eq @double-colon-count 0)) (reset! parts (py-split address ":" -1)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (let [__b5 (py-eq (count @parts) 8)] (if (py-truthy __b5) (let [__b4 (every? py-truthy (doall (for [part (py-iter @parts)] (not (py-eq part "")))))] (if (py-truthy __b4) (every? py-truthy (doall (for [part (py-iter @parts)] (-is-valid-group part)))) __b4)) __b5))}))) (let [__d6 (vec (py-split address "::" 1))] (reset! left (nth __d6 0)) (reset! right (nth __d6 1))) (reset! left-groups (if (py-truthy @left) (vec (for [g (py-iter (py-split @left ":" -1)) :when (py-truthy (not (py-eq g "")))] g)) [])) (reset! right-groups (if (py-truthy @right) (vec (for [g (py-iter (py-split @right ":" -1)) :when (py-truthy (not (py-eq g "")))] g)) [])) (reset! total-groups (py-add (count @left-groups) (count @right-groups))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (let [__b7 (py-lt @total-groups 8)] (if (py-truthy __b7) (every? py-truthy (doall (for [g (py-iter (py-add @left-groups @right-groups))] (-is-valid-group g)))) __b7))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(def -HEX-DIGITS (py-set "0123456789abcdefABCDEF"))
