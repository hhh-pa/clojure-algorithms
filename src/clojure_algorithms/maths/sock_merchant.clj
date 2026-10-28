(ns clojure-algorithms.maths.sock-merchant
  "Port of maths/sock_merchant.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare sock-merchant)

(defn sock-merchant ">>> sock_merchant([10, 20, 20, 10, 10, 30, 50, 10, 20])\n3\n>>> sock_merchant([1, 1, 3, 3])\n2"
  [colors] (reduce +' 0 (doall (for [socks-by-color (py-iter (vec (vals (py-counter colors))))] (py-floordiv socks-by-color 2)))))
