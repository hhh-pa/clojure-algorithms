(ns clojure-algorithms.linear-algebra.src.polynom-for-points
  "Fitting polynomial through a set of 2D points via Gauss-Jordan
  elimination on the Vandermonde system."
  (:require [clojure.string :as str]))

(defn- solve-vandermonde
  "Solves the Vandermonde system for coordinates; returns solution strings."
  [coordinates]
  (let [n (count coordinates)
        matrix (mapv (fn [[x _]] (mapv #(Math/pow (double x) (- n (inc %))) (range n)))
                     coordinates)
        vector (mapv (comp double second) coordinates)
        [matrix vector]
        (loop [cnt 0 matrix matrix vector vector]
          (if (>= cnt n)
            [matrix vector]
            (let [[matrix vector]
                  (if (zero? (get-in matrix [cnt cnt]))
                    (let [pivot (apply max-key #(Math/abs (double (get-in matrix [% cnt])))
                                       (range (inc cnt) n))]
                      (when (or (= pivot cnt) (zero? (get-in matrix [pivot cnt])))
                        (throw (ex-info "The program cannot work out a fitting polynomial." {})))
                      [(-> matrix (assoc cnt (nth matrix pivot)) (assoc pivot (nth matrix cnt)))
                       (-> vector (assoc cnt (nth vector pivot)) (assoc pivot (nth vector cnt)))])
                    [matrix vector])
                  [matrix vector]
                  (reduce (fn [[matrix vector] number]
                            (if (= cnt number)
                              [matrix vector]
                              (let [fraction (/ (get-in matrix [number cnt])
                                                (get-in matrix [cnt cnt]))]
                                [(assoc matrix number
                                        (mapv - (nth matrix number)
                                              (mapv #(* fraction %) (nth matrix cnt))))
                                 (assoc vector number
                                        (- (nth vector number)
                                           (* (nth vector cnt) fraction)))])))
                          [matrix vector] (range n))]
              (recur (inc cnt) matrix vector))))]
    (mapv #(str (/ (nth vector %) (get-in matrix [% %]))) (range n))))

(defn points-to-polynomial
  "Returns the fitting polynomial for coordinates ([[x y] ...]) as a
  string like \"f(x)=x^2*1.0+x^1*-0.0+x^0*0.0\", or \"x=N\" for a
  vertical line. Throws when no fitting polynomial exists."
  [coordinates]
  (when (or (empty? coordinates) (not (every? #(= 2 (count %)) coordinates)))
    (throw (ex-info "The program cannot work out a fitting polynomial." {})))
  (when (not= (count (set (map vec coordinates))) (count coordinates))
    (throw (ex-info "The program cannot work out a fitting polynomial." {})))
  (let [xs (set (map first coordinates))]
    (if (= 1 (count xs))
      (str "x=" (ffirst coordinates))
      (do
        (when (not= (count xs) (count coordinates))
          (throw (ex-info "The program cannot work out a fitting polynomial." {})))
        (let [n (count coordinates)
              solution (solve-vandermonde coordinates)]
          (str "f(x)="
               (str/join "+"
                         (map-indexed (fn [i s]
                                        (let [parts (str/split s #"E")
                                              s (if (> (count parts) 1)
                                                  (str (first parts) "*10^" (second parts))
                                                  s)]
                                          (str "x^" (- n (inc i)) "*" s)))
                                      solution))))))))
