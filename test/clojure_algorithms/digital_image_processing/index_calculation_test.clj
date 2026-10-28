(ns clojure-algorithms.digital-image-processing.index-calculation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.digital-image-processing.index-calculation
             :as ic]))

(def bands {:red 50 :green 23487 :blue 14578 :red-edge 51045 :nir 100})

(defn- approx [a b] (< (Math/abs (- (double a) (double b))) 1e-9))

(deftest test-scalar-indices
  ;; NDVI (red = 50, nir = 100) = 50/150 = 1/3
  ;; (the 0.3157894736842105 value in the Python comments is stale)
  (is (approx (ic/ndvi {:red 50 :nir 100}) (/ 1 3)))
  (is (approx (ic/calculation {:red 50 :nir 100} "NDVI") (/ 1 3)))
  (is (= 50 (ic/gdvi {:green 50 :nir 100})))
  (is (= 2 (ic/dvi {:red 50 :nir 100})))
  (is (= 2 (ic/rvi {:red 50 :nir 100})))
  (is (approx (ic/arv12 {:red 50 :nir 100})
              (+ -0.18 (* 1.17 (/ 50 150)))))
  (is (approx (ic/ngrdi {:red 50 :green 100}) (/ 1 3)))
  (is (approx (ic/ri {:red 50 :green 100}) (/ -1 3)))
  (is (approx (ic/gndvi {:green 50 :nir 100}) (/ 1 3)))
  (is (approx (ic/bndvi {:blue 50 :nir 100}) (/ 1 3)))
  (is (approx (ic/red-edge-ndvi {:red 50 :red-edge 100}) (/ 1 3)))
  (is (approx (ic/ndre {:red-edge 50 :nir 100}) (/ 1 3)))
  (is (approx (ic/ci {:red 100 :blue 25}) 0.75))
  (is (approx (ic/ci-green {:green 50 :nir 100}) 1.0))
  (is (approx (ic/norm-r {:red 50 :green 50 :nir 100}) 0.25))
  (is (approx (ic/tvi {:red 50 :nir 100})
              (Math/pow (+ (/ 50 150) 0.5) 0.5)))
  (is (approx (ic/s-index {:red 100 :green 50 :blue 25}) 0.75))
  (is (approx (ic/ivi {:red 50 :nir 100} 2.0 10.0) 0.9))
  (is (approx (ic/gli {:red 10 :green 50 :blue 10})
              (/ (- 100 20) (+ 100 20))))
  (is (approx (ic/intensity {:red 10 :green 10 :blue 10.5}) 1.0)))

(deftest test-matrix-indices
  (let [m {:red [[50 50] [50 50]] :nir [[100 100] [100 100]]}]
    (is (= [[(/ 1 3) (/ 1 3)] [(/ 1 3) (/ 1 3)]] (ic/ndvi m))))
  (let [m {:red [[100 50]] :green [[50 50]] :blue [[25 50]]}]
    (is (approx (ic/s-index m) 0.75))))

(deftest test-calculation-dispatch
  (is (= 40 (count ic/indices)))
  (doseq [name ["ARVI2" "CCCI" "CVI" "GLI" "NDVI" "BNDVI" "redEdgeNDVI"
                "GNDVI" "GBNDVI" "GRNDVI" "RBNDVI" "PNDVI" "ATSAVI"
                "BWDRVI" "CIgreen" "CIrededge" "CI" "CTVI" "GDVI" "EVI"
                "GEMI" "GOSAVI" "GSAVI" "Hue" "IPVI" "I" "RVI" "MRVI"
                "MSAVI" "NormG" "NormNIR" "NormR" "NGRDI" "RI" "S" "IF"
                "DVI" "TVI" "NDRE"]]
    (is (number? (ic/calculation bands name))
        (str name " should compute a number")))
  (is (false? (ic/calculation bands "NOPE")))
  (is (thrown? clojure.lang.ExceptionInfo (ic/calculation bands "IVI"))))
