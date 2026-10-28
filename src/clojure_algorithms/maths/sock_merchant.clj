(ns clojure-algorithms.maths.sock-merchant
  "Port of maths/sock_merchant.py")

(defn sock-merchant
  "Count how many pairs of socks can be formed from the given sock colors."
  [colors]
  (reduce + 0 (map #(quot % 2) (vals (frequencies colors)))))
