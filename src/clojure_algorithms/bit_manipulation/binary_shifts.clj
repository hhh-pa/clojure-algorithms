(ns clojure-algorithms.bit-manipulation.binary-shifts
  "Logical and arithmetic binary shifts returning binary string results.")

(defn logical-left-shift
  "Returns the \"0b\"-prefixed binary string of `number` logically left
  shifted by `shift-amount`. Both inputs must be non-negative."
  [number shift-amount]
  (when (or (neg? number) (neg? shift-amount))
    (throw (ex-info "both inputs must be positive integers" {})))
  (str "0b" (Integer/toBinaryString number) (apply str (repeat shift-amount "0"))))

(defn logical-right-shift
  "Returns the \"0b\"-prefixed binary string of `number` logically right
  shifted by `shift-amount`. Both inputs must be non-negative."
  [number shift-amount]
  (when (or (neg? number) (neg? shift-amount))
    (throw (ex-info "both inputs must be positive integers" {})))
  (let [binary (Integer/toBinaryString number)]
    (if (>= shift-amount (count binary))
      "0b0"
      (str "0b" (subs binary 0 (- (count binary) shift-amount))))))

(defn- signed-binary
  "Minimal sign-prefixed binary form mirroring the Python reference:
  positives get a leading 0, negatives use two's complement with a leading 1."
  [number]
  (if (>= number 0)
    (str "0" (Integer/toBinaryString number))
    (let [magnitude (Math/abs (long number))
          n (count (Integer/toBinaryString magnitude))
          diff (- magnitude (bit-shift-left 1 n))
          raw (if (zero? diff) "" (Integer/toBinaryString (- diff)))]
      (str "1" (apply str (repeat (- n (count raw)) "0")) raw))))

(defn arithmetic-right-shift
  "Returns the \"0b\"-prefixed binary string of `number` arithmetically
  right shifted by `shift-amount` (sign bit is replicated)."
  [number shift-amount]
  (let [binary (signed-binary number)]
    (if (>= shift-amount (count binary))
      (str "0b" (apply str (repeat (count binary) (first binary))))
      (str "0b"
           (apply str (repeat shift-amount (first binary)))
           (subs binary 0 (- (count binary) shift-amount))))))
