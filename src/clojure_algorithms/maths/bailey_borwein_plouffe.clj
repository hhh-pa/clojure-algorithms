(ns clojure-algorithms.maths.bailey-borwein-plouffe
  "Bailey-Borwein-Plouffe pi-digit-extraction algorithm.")

(defn -subsum
  "Private helper implementing the summation functionality."
  [digit-pos-to-extract denominator-addend precision]
  (loop [sum-index 0
         total 0.0]
    (if (>= sum-index (+ digit-pos-to-extract precision))
      total
      (let [denominator (+ (* 8 sum-index) denominator-addend)
            exponential-term (if (< sum-index digit-pos-to-extract)
                               (.modPow (biginteger 16)
                                        (biginteger (- digit-pos-to-extract 1 sum-index))
                                        (biginteger denominator))
                               (Math/pow 16 (- digit-pos-to-extract 1 sum-index)))]
        (recur (inc sum-index)
               (+ total (/ (double exponential-term) denominator)))))))

(defn bailey-borwein-plouffe
  "Calculate the nth hex digit of pi with the Bailey-Borwein-Plouffe formula.
  The digit immediately after the decimal point is located at position 1."
  ([digit-position] (bailey-borwein-plouffe digit-position 1000))
  ([digit-position precision]
   (when (or (not (integer? digit-position)) (<= digit-position 0))
     (throw (ex-info "Digit position must be a positive integer" {:type :ValueError})))
   (when (or (not (integer? precision)) (< precision 0))
     (throw (ex-info "Precision must be a nonnegative integer" {:type :ValueError})))
   (let [sum-result (- (* 4 (-subsum digit-position 1 precision))
                       (* 2 (-subsum digit-position 4 precision))
                       (-subsum digit-position 5 precision)
                       (-subsum digit-position 6 precision))]
     (Long/toHexString (long (* (mod sum-result 1) 16))))))
