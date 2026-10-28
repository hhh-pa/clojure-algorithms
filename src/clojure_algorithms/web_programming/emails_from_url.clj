(ns clojure-algorithms.web-programming.emails-from-url
  "Collect site emails by crawling links.

  PARTIAL PORT: URL parsing and email extraction are pure and fully
  ported; the HTTP crawling itself needs network access (stubbed)."
  (:require [clojure.string :as str]))

(defn get-sub-domain-name
  "Host part of `url` (\"\" when unparsable)."
  [url]
  (try
    (.getHost (java.net.URI. url))
    (catch Exception _ "")))

(defn get-domain-name
  "Main domain (last two host labels) of `url`."
  [url]
  (let [host (or (get-sub-domain-name url) "")]
    (if (empty? host)
      ""
      (str/join "." (take-last 2 (str/split host #"\."))))))

(defn extract-emails
  "Sorted unique [local@domain] addresses found in `html` for `domain`."
  [html domain]
  (vec (sort (set (re-seq (re-pattern (str "[a-zA-Z0-9]+@"
                                           (java.util.regex.Pattern/quote
                                            domain)))
                          html)))))

(defn emails-from-url
  "STUB: would crawl `url` and return sorted emails found."
  ([] (emails-from-url "https://github.com"))
  ([url]
   (throw (ex-info "Network access required: crawl site"
                   {:reason :network-required :url url}))))
