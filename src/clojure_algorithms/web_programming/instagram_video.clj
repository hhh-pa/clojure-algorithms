(ns clojure-algorithms.web-programming.instagram-video
  "Instagram video download via downloadgram.

  STUB: resolving + downloading need network access. The API URL builder
  and timestamped filename builder are ported."
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def api-base
  "https://downloadgram.net/wp-json/wppress/video-downloader/video?url=")

(defn api-url
  [url]
  (str api-base url))

(defn timestamped-filename
  "UTC filename like 2024-01-12_10-30-00.mp4."
  ([] (timestamped-filename (LocalDateTime/now)))
  ([date-time]
   (str (.format date-time
                 (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss"))
        ".mp4")))

(defn download-video
  "STUB: would return the video bytes for `url`."
  [url]
  (throw (ex-info "Network access required: downloadgram"
                  {:reason :network-required :url (api-url url)})))
