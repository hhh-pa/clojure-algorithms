(ns clojure-algorithms.maths.pi-generator
  "Pi digit generation via the Leibniz-formula spigot algorithm.")

(defn- floordiv
  "Integer division rounding toward negative infinity."
  [a b]
  (let [q (quot a b)
        r (rem a b)]
    (if (or (zero? r) (= (neg? a) (neg? b))) q (dec q))))

(defn calculate-pi
  "Return pi as a string with limit digits after the decimal point."
  [limit]
  (loop [q 1
         r 0
         t 1
         k 1
         n 3
         m 3
         counter 0
         result ""]
    (if (= counter (inc limit))
      result
      (if (< (-' (+' (*' 4 q) r) t) (*' n t))
        (let [result (cond-> (str result n) (zero? counter) (str "."))]
          (if (= limit counter)
            result
            (let [nr (*' 10 (-' r (*' n t)))
                  nn (-' (floordiv (*' 10 (+' (*' 3 q) r)) t) (*' 10 n))]
              (recur (*' q 10) nr t k nn m (inc counter) result))))
        (let [nr (*' (+' (*' 2 q) r) m)
              nn (floordiv (+' (*' q (*' 7 k)) 2 (*' r m)) (*' t m))]
          (recur (*' q k) nr (*' t m) (inc k) nn (+ m 2) counter result))))))

(defn main
  []
  (println (str "calculate_pi(50) = '" (calculate-pi 50) "'"))
  nil)
