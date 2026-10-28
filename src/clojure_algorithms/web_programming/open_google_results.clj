(ns clojure-algorithms.web-programming.open-google-results
  "Open the first Google result in a browser.

  STUB: the Python original is a script (no functions) needing network,
  HTML parsing and a browser. Query encoding + search URL building are
  ported as pure logic."
  (:require [clojure.string :as str])
  (:import [java.net URLEncoder]))

(defn encode-args-query
  "Join CLI args with %20 (mirrors the argv branch)."
  [args]
  (str/join "%20" args))

(defn encode-input-query
  "URL-quote an interactive query string."
  [query]
  (URLEncoder/encode query "UTF-8"))

(defn search-url
  [query]
  (str "https://www.google.com/search?q=" query "&num=100"))

(defn open-first-result
  "STUB: would open the first result for `query` in a browser."
  [query]
  (throw (ex-info "Network/browser access required"
                  {:reason :network-required :url (search-url query)})))
