(ns clojure-algorithms.other.sliding-window-maximum
  "Maximum of each sliding window in O(n) with a monotonic deque.

  Port of TheAlgorithms/Python `other/sliding_window_maximum.py`."
  (:import [java.util ArrayDeque]))

(defn sliding-window-maximum
  "Return the maximum of each window of size `window-size` in `numbers`."
  [numbers window-size]
  (when (<= window-size 0)
    (throw (IllegalArgumentException. "Window size must be a positive integer")))
  (let [nums (vec numbers)
        dq (ArrayDeque.)]
    (loop [i 0 result []]
      (if (>= i (count nums))
        result
        (do
          (when (and (not (.isEmpty dq))
                     (= (.peekFirst dq) (- i window-size)))
            (.pollFirst dq))
          (while (and (not (.isEmpty dq))
                      (< (nums (.peekLast dq)) (nums i)))
            (.pollLast dq))
          (.addLast dq i)
          (recur (inc i)
                 (if (>= i (dec window-size))
                   (conj result (nums (.peekFirst dq)))
                   result)))))))
