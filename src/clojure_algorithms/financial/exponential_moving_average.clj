(ns clojure-algorithms.financial.exponential-moving-average)

(defn exponential-moving-average
  "Return a lazy seq of exponential moving averages of stock-prices.
  Uses the simple average of the first points until window-size is reached,
  then st = alpha * xt + (1 - alpha) * st-prev with alpha = 2 / (1 + window-size)."
  [stock-prices window-size]
  (when (<= window-size 0)
    (throw (ex-info "window_size must be > 0" {:window-size window-size})))
  (let [alpha (/ 2.0 (+ 1 window-size))]
    (letfn [(step [i prev [price & rest-prices]]
              (when (some? price)
                (let [avg (cond
                            (or (zero? i) (= window-size 1)) (double price)
                            (<= i window-size) (* (+ prev price) 0.5)
                            :else (+ (* alpha price) (* (- 1 alpha) prev)))]
                  (lazy-seq (cons avg (step (inc i) avg rest-prices))))))]
      (step 0 0.0 (seq stock-prices)))))
