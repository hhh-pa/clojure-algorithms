(ns clojure-algorithms.strings.rabin-karp
  "Rabin-Karp pattern search with a rolling hash.
  Ported from TheAlgorithms/Python.")

(def alphabet-size 256)
(def modulus 1000003)

(defn rabin-karp
  "True when `pattern` occurs in `text`."
  [pattern text]
  (let [m (count pattern)
        n (count text)]
    (if (> m n)
      false
      (let [[ph th power]
            (loop [i 0 ph 0 th 0 pw 1]
              (if (>= i m)
                [ph th pw]
                (recur (inc i)
                       (mod (+ (int (nth pattern i)) (* ph alphabet-size))
                            modulus)
                       (mod (+ (int (nth text i)) (* th alphabet-size))
                            modulus)
                       (if (= i (dec m)) pw
                           (mod (* pw alphabet-size) modulus)))))]
        (loop [i 0 th th]
          (cond
            (> i (- n m)) false
            (and (= th ph) (= (subs text i (+ i m)) pattern)) true
            (= i (- n m)) false
            :else
            (recur (inc i)
                   (mod (+ (* (- th (* (int (nth text i)) power))
                              alphabet-size)
                           (int (nth text (+ i m))))
                        modulus))))))))
