(ns clojure-algorithms.hashes.elf
  "ELF hash (a PJW hash variant) for strings.")

(defn elf-hash
  "Returns the ELF hash of string data."
  [data]
  (reduce (fn [h letter]
            (let [h (+ (bit-shift-left h 4) (int letter))
                  x (bit-and h 0xF0000000)
                  h (if (zero? x) h (bit-xor h (unsigned-bit-shift-right x 24)))]
              (bit-and h (bit-not x))))
          0
          data))
