(ns clojure-algorithms.web-programming.instagram-pic
  "Image download via the og:image meta tag.

  STUB: fetching + HTML parsing need network and an HTML parser. The
  timestamped filename builder and status messages are ported."
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def no-meta-message "No meta tag with property 'og:image' was found.")

(defn timestamped-filename
  "UTC filename like 2024-01-12_10-30-00.jpg."
  ([] (timestamped-filename (LocalDateTime/now)))
  ([date-time]
   (str (.format date-time
                 (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss"))
        ".jpg")))

(defn saved-message
  [file-name]
  (str "Image downloaded and saved in the file " file-name))

(defn download-image
  "STUB: would download the og:image of `url` to a timestamped file."
  [url]
  (throw (ex-info "Network access required: scrape og:image"
                  {:reason :network-required :url url})))
