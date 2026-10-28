(ns clojure-algorithms.bit-manipulation.binary-or-operator
  "Bitwise OR helper.

  Returns a zero-padded binary string representing (a | b) where the width
  is the maximum bit length of the inputs. Only non-negative integers
  are accepted.")

(defn- bit-length
  "Number of bits needed to represent non-negative integer n."
  [n]
  (if (zero? n) 0 (.bitLength (biginteger n))))

(defn binary-or
  "Returns a \"0b\"-prefixed binary string of (bit-or a b), zero-padded to
  the maximum bit length of a and b."
  [a b]
  (when (or (not (integer? a)) (not (integer? b)))
    (throw (ex-info "inputs must be integers" {})))
  (when (or (neg? a) (neg? b))
    (throw (ex-info "inputs must be non-negative integers" {})))
  (let [width (max (bit-length a) (bit-length b))
        s (Integer/toBinaryString (bit-or a b))]
    (str "0b" (str (apply str (repeat (max 0 (- width (count s))) "0")) s))))
