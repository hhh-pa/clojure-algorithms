(ns clojure-algorithms.other.cheap-progress
  "Minimal progress-reporting wrapper over a seqable.

  Port of TheAlgorithms/Python `other/cheap_progress.py`.
  Progress is printed to stderr; the returned seq yields the items.")

(defn progress
  "Return a seq of `items` that prints a one-line progress indicator to
  stderr as it is consumed. `desc` labels the output; `total` overrides
  the inferred count (0 means infer from count when possible)."
  ([items] (progress items "" 0))
  ([items desc] (progress items desc 0))
  ([items desc total]
   (let [total (max (long total) 0)
         total (if (and (zero? total) (counted? items)) (count items) total)
         n (atom 0)]
     (map (fn [item]
            (swap! n inc)
            (let [suffix (if (pos? total)
                           (str @n "/" total)
                           (str @n))]
              (binding [*out* *err*]
                (print (str "\r\033[K" desc ": " suffix))
                (flush)))
            item)
          items))))
