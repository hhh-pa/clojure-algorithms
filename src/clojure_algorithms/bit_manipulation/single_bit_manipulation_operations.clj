(ns clojure-algorithms.bit-manipulation.single-bit-manipulation-operations
  "Set, clear, flip, test and read a single bit of an integer.")

(defn set-bit
  "Returns `number` with the bit at `position` set to 1."
  [number position]
  (bit-or number (bit-shift-left 1 position)))

(defn clear-bit
  "Returns `number` with the bit at `position` set to 0."
  [number position]
  (bit-and number (bit-not (bit-shift-left 1 position))))

(defn flip-bit
  "Returns `number` with the bit at `position` toggled."
  [number position]
  (bit-xor number (bit-shift-left 1 position)))

(defn is-bit-set
  "Returns true when the bit at `position` in `number` is 1."
  [number position]
  (= 1 (bit-and (bit-shift-right number position) 1)))

(defn get-bit
  "Returns the bit (0 or 1) at `position` in `number`."
  [number position]
  (if (zero? (bit-and number (bit-shift-left 1 position))) 0 1))

(defn clear-least-significant-set-bit
  "Clears the rightmost 1 bit of `number` (n & (n - 1))."
  [number]
  (bit-and number (dec number)))
