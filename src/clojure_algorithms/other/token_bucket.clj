(ns clojure-algorithms.other.token-bucket
  "Token bucket rate limiter.

  Port of TheAlgorithms/Python `other/token_bucket.py`.
  Time is injectable (`now-ms`) so refill logic is testable without
  sleeping; the atom-backed limiter keeps the Python thread-safety.")

(defn make-bucket
  "Create a bucket that gains `rate` tokens every `frequency-ms`
  milliseconds, holding at most `capacity` tokens (starts full)."
  ([rate capacity frequency-ms]
   (make-bucket rate capacity frequency-ms (System/currentTimeMillis)))
  ([rate capacity frequency-ms now-ms]
   (atom {:rate rate :capacity capacity :frequency-ms frequency-ms
          :tokens capacity :last-checked now-ms})))

(defn- refill
  "Add tokens earned since `:last-checked` as of `now-ms`."
  [state now-ms]
  (let [elapsed (- now-ms (:last-checked state))]
    (if (>= elapsed (:frequency-ms state))
      (let [periods (quot elapsed (:frequency-ms state))]
        (-> state
            (update :tokens #(min (:capacity state)
                                  (+ % (* periods (:rate state)))))
            (update :last-checked + (* periods (:frequency-ms state)))))
      state)))

(defn allow-request?
  "Consume one token if available; return true when the request passes.
  Optional `now-ms` overrides the clock (for tests)."
  ([bucket] (allow-request? bucket (System/currentTimeMillis)))
  ([bucket now-ms]
   (let [[allowed _]
         (swap-vals! bucket
                     (fn [state]
                       (let [state (refill state now-ms)]
                         (if (>= (:tokens state) 1)
                           (update state :tokens dec)
                           state))))]
     (pos? (:tokens (refill allowed now-ms))))))
