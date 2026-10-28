(ns clojure-algorithms.bit-manipulation.binary-and-operator
  "Bitwise AND helper.

  Returns a zero-padded binary string representing (a & b) where the width
  is the maximum bit length of the inputs. Only non-negative integers
  are accepted.")

(defn- bit-length
  "Number of bits needed to represent non-negative integer n."
  [n]
  (if (zero? n) 0 (.bitLength (biginteger n))))

(defn- pad-left
  [s width ch]
  (let [pad (max 0 (- width (count s)))]
    (str (apply str (repeat pad ch)) s)))

(defn binary-and
  "Returns a \"0b\"-prefixed binary string of (bit-and a b), zero-padded to
  the maximum bit length of a and b."
  [a b]
  (when (or (not (integer? a)) (not (integer? b)))
    (throw (ex-info "inputs must be integers" {})))
  (when (or (neg? a) (neg? b))
    (throw (ex-info "inputs must be non-negative integers" {})))
  (let [width (max (bit-length a) (bit-length b))]
    (str "0b" (pad-left (Integer/toBinaryString (bit-and a b)) width \0))))
