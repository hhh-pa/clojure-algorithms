(ns clojure-algorithms.maths.allocation-number
  "In a multi-threaded download, this algorithm could be used to provide
each worker thread with a block of non-overlapping bytes to download.
For example:
    for i in allocation_list:
        httpx2.get(url, headers={\"Range\": f\"bytes={i}\"})")

(defn allocation-num
  "Divide a number of bytes into x partitions.
  number-of-bytes: the total of bytes.
  partitions: the number of partitions to allocate.
  Returns a vector of \"start-end\" byte ranges, one per worker thread."
  [number-of-bytes partitions]
  (when (<= partitions 0)
    (throw (ex-info "partitions must be a positive number!" {:type :ValueError})))
  (when (> partitions number-of-bytes)
    (throw (ex-info "partitions can not > number_of_bytes!" {:type :ValueError})))
  (let [bytes-per-partition (quot number-of-bytes partitions)]
    (mapv (fn [i]
            (str (inc (* i bytes-per-partition))
                 "-"
                 (if (= i (dec partitions))
                   number-of-bytes
                   (* (inc i) bytes-per-partition))))
          (range partitions))))
