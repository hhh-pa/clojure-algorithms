(ns clojure-algorithms.ciphers.atbash
  "Atbash mirror cipher.")

(defn atbash-slow
  "Mirrors ASCII letters via character arithmetic."
  [sequence]
  (apply str (map (fn [c]
                    (let [n (int c)]
                      (cond (<= 65 n 90) (char (- 155 n))
                            (<= 97 n 122) (char (- 219 n))
                            :else c)))
                  sequence)))

(def ^:private letters
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def ^:private letters-reversed
  (str (apply str (reverse (subs letters 0 26)))
       (apply str (reverse (subs letters 26 52)))))
(def ^:private mirror (zipmap letters letters-reversed))

(defn atbash
  "Mirrors ASCII letters via a lookup table."
  [sequence]
  (apply str (map #(get mirror % %) sequence)))
