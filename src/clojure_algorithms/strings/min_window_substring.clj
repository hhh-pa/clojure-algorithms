(ns clojure-algorithms.strings.min-window-substring
  "Minimum window substring (sliding window).
  Ported from TheAlgorithms/Python.")

(defn min-window
  "Smallest substring of `search-str` containing all chars of `target`
  (with multiplicity), or \"\" when impossible."
  [search-str target]
  (let [need (frequencies target)
        required (count need)]
    (if (or (empty? target) (> (count target) (count search-str)))
      ""
      (loop [left 0 right 0 have {} formed 0 best nil]
        (if (>= right (count search-str))
          (if best (subs search-str (first best) (second best)) "")
          (let [ch (nth search-str right)
                have2 (if (contains? need ch)
                        (update have ch (fnil inc 0)) have)
                formed2 (if (and (contains? need ch)
                                 (= (get have2 ch) (get need ch)))
                          (inc formed) formed)
                [left2 have3 formed3 best2]
                (loop [l left h have2 f formed2 b best]
                  (if (and (= f required) (<= l right))
                    (let [b2 (if (or (nil? b) (< (- right l -1) (- (second b)
                                                                   (first b))))
                               [l (inc right)] b)
                          cl (nth search-str l)
                          h2 (if (contains? need cl)
                               (update h cl dec) h)
                          f2 (if (and (contains? need cl)
                                      (< (get h2 cl) (get need cl)))
                               (dec f) f)]
                      (recur (inc l) h2 f2 b2))
                    [l h f b]))]
            (recur left2 (inc right) have3 formed3 best2)))))))
