(ns clojure-algorithms.matrix.matrix-class-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.matrix-class :as sut]))

(def sample-rows [[1 2 3] [4 5 6] [7 8 9]])

(defn sample-matrix [] (sut/make-Matrix sample-rows))

(deftest test-construction
  (is (= sample-rows (:rows (sample-matrix))))
  (is (= [] (:rows (sut/make-Matrix []))))
  (is (thrown? Exception (sut/make-Matrix [[]])))
  (is (thrown? Exception (sut/make-Matrix [[1 2] [3]])))
  (is (thrown? Exception (sut/make-Matrix [[1 2] ["a" "b"]]))))

(deftest test-information
  (is (= [[1 4 7] [2 5 8] [3 6 9]] (sut/Matrix-columns (sample-matrix))))
  (is (= 3 (sut/Matrix-num-rows (sample-matrix))))
  (is (= 3 (sut/Matrix-num-columns (sample-matrix))))
  (is (= [3 3] (sut/Matrix-order (sample-matrix))))
  (is (= true (sut/Matrix-is-square (sample-matrix))))
  (is (= false (sut/Matrix-is-square (sut/make-Matrix [[1 2 3] [4 5 6]]))))
  (is (= false (sut/Matrix-is-invertable (sample-matrix))))
  (is (= true (sut/Matrix-is-invertable (sut/make-Matrix [[1 2] [3 4]])))))

(deftest test-determinant
  (is (= 0 (sut/Matrix-determinant (sample-matrix))))
  (is (= -2 (sut/Matrix-determinant (sut/make-Matrix [[1 2] [3 4]]))))
  (is (= 5 (sut/Matrix-determinant (sut/make-Matrix [[5]]))))
  (is (= 1 (sut/Matrix-determinant (sut/make-Matrix []))))
  (is (= 0 (sut/Matrix-determinant (sut/make-Matrix [[1 2 3] [4 5 6]])))))

(deftest test-derived-matrices
  (is (= "[[1. 0. 0.]\n [0. 1. 0.]\n [0. 0. 1.]]"
         (sut/Matrix-str (sut/Matrix-identity (sample-matrix)))))
  (is (= "[[1. 0.]\n [0. 1.]]"
         (sut/Matrix-str (sut/Matrix-identity (sut/make-Matrix [[1 2] [3 4]])))))
  (is (= "[[-3. -6. -3.]\n [-6. -12. -6.]\n [-3. -6. -3.]]"
         (sut/Matrix-str (sut/Matrix-minors (sample-matrix)))))
  (is (= "[[-3. 6. -3.]\n [6. -12. 6.]\n [-3. 6. -3.]]"
         (sut/Matrix-str (sut/Matrix-cofactors (sample-matrix)))))
  (is (= "[[-3. 6. -3.]\n [6. -12. 6.]\n [-3. 6. -3.]]"
         (sut/Matrix-str (sut/Matrix-adjugate (sample-matrix)))))
  (is (= -3 (sut/Matrix-get-minor (sample-matrix) 0 0)))
  (is (= -3 (sut/Matrix-get-cofactor (sample-matrix) 0 0)))
  (is (= 6 (sut/Matrix-get-cofactor (sample-matrix) 0 1))))

(deftest test-inverse
  (is (thrown? Exception (sut/Matrix-inverse (sample-matrix))))
  (is (= [[1 0] [0 1]]
         (:rows (sut/Matrix-inverse (sut/make-Matrix [[1 0] [0 1]]))))))

(deftest test-rendering
  (is (= "[[1. 2. 3.]\n [4. 5. 6.]\n [7. 8. 9.]]"
         (sut/Matrix-str (sample-matrix))))
  (is (= "[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"
         (sut/Matrix-repr (sample-matrix))))
  (is (= "[]" (sut/Matrix-str (sut/make-Matrix []))))
  (is (= "[[1. 2. 3.]\n [4. 5. 6.]\n [7. 8. 9.]]\n"
         (with-out-str (println (sample-matrix))))))

(deftest test-arithmetic
  (is (= "[[-1. -2. -3.]\n [-4. -5. -6.]\n [-7. -8. -9.]]"
         (sut/Matrix-str (sut/Matrix-neg (sample-matrix)))))
  (is (= "[[3. 6. 9.]\n [12. 15. 18.]\n [21. 24. 27.]]"
         (sut/Matrix-str (sut/Matrix-mul (sample-matrix) 3))))
  (is (= "[[4. 8. 12.]\n [16. 20. 24.]\n [28. 32. 36.]]"
         (sut/Matrix-str (sut/Matrix-add (sample-matrix)
                                         (sut/Matrix-mul (sample-matrix) 3)))))
  (is (= "[[-2. -4. -6.]\n [-8. -10. -12.]\n [-14. -16. -18.]]"
         (sut/Matrix-str (sut/Matrix-sub (sample-matrix)
                                         (sut/Matrix-mul (sample-matrix) 3)))))
  (is (= "[[468. 576. 684.]\n [1062. 1305. 1548.]\n [1656. 2034. 2412.]]"
         (sut/Matrix-str (sut/Matrix-pow (sample-matrix) 3))))
  (is (= 32 (sut/Matrix-dot-product [1 2 3] [4 5 6])))
  (is (= true (sut/Matrix-eq (sample-matrix) (sample-matrix))))
  (is (= false (sut/Matrix-eq (sample-matrix)
                              (sut/Matrix-mul (sample-matrix) 3))))
  (is (= false (sut/Matrix-eq (sample-matrix) [[1 2 3]])))
  (is (= true (sut/Matrix-ne (sample-matrix)
                             (sut/Matrix-mul (sample-matrix) 3))))
  (is (= false (sut/Matrix-ne (sample-matrix) (sample-matrix)))))

(deftest test-arithmetic-errors
  (is (thrown? Exception (sut/Matrix-add (sample-matrix)
                                         (sut/make-Matrix [[1 2] [3 4]]))))
  (is (thrown? Exception (sut/Matrix-sub (sample-matrix)
                                         (sut/make-Matrix [[1 2] [3 4]]))))
  (is (thrown? Exception (sut/Matrix-mul (sample-matrix)
                                         (sut/make-Matrix [[1 2] [3 4]]))))
  (is (thrown? Exception (sut/Matrix-mul (sample-matrix) "x")))
  (is (thrown? Exception (sut/Matrix-pow (sample-matrix) 1.5)))
  (is (thrown? Exception (sut/Matrix-pow (sut/make-Matrix [[1 2 3]
                                                           [4 5 6]])
                                         2)))
  (is (thrown? Exception (sut/Matrix-pow (sample-matrix) -1))))

(deftest test-pow-zero-and-identity
  (is (= [[1 0 0] [0 1 0] [0 0 1]]
         (:rows (sut/Matrix-pow (sample-matrix) 0)))))

(deftest test-add-row-and-column
  (let [m (sut/Matrix-add-row (sample-matrix) [10 11 12])]
    (is (= "[[1. 2. 3.]\n [4. 5. 6.]\n [7. 8. 9.]\n [10. 11. 12.]]"
           (sut/Matrix-str m))))
  (let [m2 (sut/Matrix-add-column (sut/Matrix-mul (sample-matrix) 3) [8 16 32])]
    (is (= "[[3. 6. 9. 8.]\n [12. 15. 18. 16.]\n [21. 24. 27. 32.]]"
           (sut/Matrix-str m2)))
    (is (= "[[90. 108. 126. 136.]\n [198. 243. 288. 304.]\n [306. 378. 450. 472.]\n [414. 513. 612. 640.]]"
           (sut/Matrix-str (sut/Matrix-mul (sut/Matrix-add-row (sample-matrix)
                                                               [10 11 12])
                                           m2)))))
  (is (thrown? Exception (sut/Matrix-add-row (sample-matrix) [1 2])))
  (is (thrown? Exception (sut/Matrix-add-row (sample-matrix) "abc")))
  (is (thrown? Exception (sut/Matrix-add-column (sample-matrix) [1 2])))
  (is (thrown? Exception (sut/Matrix-add-column (sample-matrix) "abc"))))
