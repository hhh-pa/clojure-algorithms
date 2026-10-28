(ns clojure-algorithms.bit-manipulation.swap-all-odd-and-even-bits
  "Swap all odd and even bits of an integer: mask out the even bits
  (0xAAAAAAAA) and odd bits (0x55555555), shift them toward each other,
  and combine with OR.")

(defn swap-odd-even-bits
  "Returns `num` with every pair of adjacent bits swapped."
  [num]
  (let [even-bits (bit-and num 0xAAAAAAAA)
        odd-bits (bit-and num 0x55555555)]
    (bit-or (bit-shift-right even-bits 1) (bit-shift-left odd-bits 1))))

(defn- padded-binary
  "Binary of `n`, zero-padded to width 8 like Python's `:08b` format."
  [n]
  (if (neg? n)
    (let [mag (Long/toBinaryString (- (long n)))]
      (str "-" (str (apply str (repeat (max 0 (- 7 (count mag))) "0")) mag)))
    (let [s (Long/toBinaryString (long n))]
      (str (apply str (repeat (max 0 (- 8 (count s))) "0")) s))))

(defn show-bits
  "Formats `before`/`after` values as right-aligned decimal plus binary,
  mirroring the Python reference display helper."
  [before after]
  (format "%5d: %s%n%5d: %s"
          before (padded-binary before) after (padded-binary after)))
