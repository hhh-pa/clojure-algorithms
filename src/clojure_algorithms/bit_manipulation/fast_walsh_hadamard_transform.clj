(ns clojure-algorithms.bit-manipulation.fast-walsh-hadamard-transform
  "Fast Walsh-Hadamard Transform (FWHT) for bitwise XOR, AND and OR
  convolutions in O(N log N), where N is a positive power of 2.
  Ref: https://en.wikipedia.org/wiki/Fast_Walsh%E2%80%93Hadamard_transform
  Ref: https://cp-algorithms.com/algebra/walsh-hadamard-transform.html")

(defn- check-power-of-two
  [sequence]
  (let [n (count sequence)]
    (when (or (zero? n) (not (zero? (bit-and n (dec n)))))
      (throw (ex-info "Length of sequence must be a positive power of 2." {})))
    n))

(defn fwht-xor
  "Fast Walsh-Hadamard Transform for XOR. With `inverse` true, divides the
  result by the sequence length (elements must be divisible by it)."
  ([sequence] (fwht-xor sequence false))
  ([sequence inverse]
   (let [n (check-power-of-two sequence)
         result (object-array (map long sequence))]
     (loop [half-block 1]
       (when (< half-block n)
         (let [block-size (* 2 half-block)]
           (doseq [block-start (range 0 n block-size)
                   offset (range half-block)]
             (let [li (+ block-start offset)
                   ri (+ li half-block)
                   l (aget result li)
                   r (aget result ri)]
               (aset result li (+ l r))
               (aset result ri (- l r)))))
         (recur (* 2 half-block))))
     (let [out (vec (map long (seq result)))]
       (if inverse
         (do (when (some #(not (zero? (mod % n))) out)
               (throw (ex-info (str "Inverse XOR transform requires elements "
                                    "divisible by sequence length.") {})))
             (mapv #(quot % n) out))
         out)))))

(defn xor-convolution
  "Bitwise XOR convolution: C[k] = sum over i^j=k of A[i]*B[j]."
  [sequence-a sequence-b]
  (when (not= (count sequence-a) (count sequence-b))
    (throw (ex-info "Input sequences must have equal length." {})))
  (let [ta (fwht-xor sequence-a)
        tb (fwht-xor sequence-b)]
    (fwht-xor (mapv * ta tb) true)))

(defn fwht-or
  "Fast Walsh-Hadamard Transform for OR (forward, or inverse to undo)."
  ([sequence] (fwht-or sequence false))
  ([sequence inverse]
   (let [n (check-power-of-two sequence)
         result (object-array (map long sequence))]
     (loop [half-block 1]
       (when (< half-block n)
         (let [block-size (* 2 half-block)]
           (doseq [block-start (range 0 n block-size)
                   offset (range half-block)]
             (let [li (+ block-start offset)
                   ri (+ li half-block)]
               (if inverse
                 (aset result ri (- (aget result ri) (aget result li)))
                 (aset result ri (+ (aget result ri) (aget result li))))))
           (recur (* 2 half-block)))))
     (vec (map long (seq result))))))

(defn or-convolution
  "Bitwise OR convolution: C[k] = sum over i|j=k of A[i]*B[j]."
  [sequence-a sequence-b]
  (when (not= (count sequence-a) (count sequence-b))
    (throw (ex-info "Input sequences must have equal length." {})))
  (let [ta (fwht-or sequence-a)
        tb (fwht-or sequence-b)]
    (fwht-or (mapv * ta tb) true)))

(defn fwht-and
  "Fast Walsh-Hadamard Transform for AND (forward, or inverse to undo)."
  ([sequence] (fwht-and sequence false))
  ([sequence inverse]
   (let [n (check-power-of-two sequence)
         result (object-array (map long sequence))]
     (loop [half-block 1]
       (when (< half-block n)
         (let [block-size (* 2 half-block)]
           (doseq [block-start (range 0 n block-size)
                   offset (range half-block)]
             (let [li (+ block-start offset)
                   ri (+ li half-block)]
               (if inverse
                 (aset result li (- (aget result li) (aget result ri)))
                 (aset result li (+ (aget result li) (aget result ri))))))
           (recur (* 2 half-block)))))
     (vec (map long (seq result))))))

(defn and-convolution
  "Bitwise AND convolution: C[k] = sum over i&j=k of A[i]*B[j]."
  [sequence-a sequence-b]
  (when (not= (count sequence-a) (count sequence-b))
    (throw (ex-info "Input sequences must have equal length." {})))
  (let [ta (fwht-and sequence-a)
        tb (fwht-and sequence-b)]
    (fwht-and (mapv * ta tb) true)))
