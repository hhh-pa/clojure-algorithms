(ns clojure-algorithms.maths.entropy-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.entropy :as sut]))

(defn- approx
  "Tolerant equality for ported tests: exact on integers, 1e-6 on floats."
  [a b]
  (cond (and (integer? a) (integer? b)) (= a b)
        (and (number? a) (number? b))
        (let [x (double a) y (double b)]
          (or (= x y) (< (Math/abs (- x y)) 1e-6)))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map approx a b)))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? true? (map (fn [[k v]]
                                  (and (contains? b k) (approx v (get b k))))
                                a)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(deftest test-fn-calculate-prob-examples
  (is (= "4.0\n6.0\n2.0\n" (with-out-str (let [text "Behind Winston's back the voice from the telescreen was still babbling and the overfulfilment"] (sut/calculate-prob text)))))
  (is (= "4.0\n5.0\n1.0\n" (with-out-str (let [text "The Ministry of Truth—Minitrue, in Newspeak [Newspeak was the officialface in elegant lettering, the three"] (sut/calculate-prob text)))))
  (is (= "4.0\n7.0\n3.0\n" (with-out-str (let [text "Had repulsive dashwoods suspicion sincerity but advantage now him. Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. You greatest jointure saw horrible. He private he on be imagine suppose. Fertile beloved evident through no service elderly is. Blind there if every no so at. Own neglected you preferred way sincerity delivered his attempted. To of message cottage windows do besides against uncivil.  Delightful unreserved impossible few estimating men favourable see entreaties. She propriety immediate was improving. He or entrance humoured likewise moderate. Much nor game son say feel. Fat make met can must form into gate. Me we offending prevailed discovery."] (sut/calculate-prob text))))))
