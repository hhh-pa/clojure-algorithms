(ns clojure-algorithms.web-programming.crawl-google-results
  "Google result crawling script.

  STUB: the Python original is a script (no functions) that fetches Google
  search HTML (httpx2 + BeautifulSoup) and opens links in a browser. This
  namespace keeps the pure query-URL builder."
  (:require [clojure.string :as str]))

(def search-base "https://www.google.com/search?q=")

(defn search-url
  "Google search URL for `terms`."
  [terms]
  (str search-base (str/join " " terms)))

(defn crawl-and-open
  "STUB: would fetch results for `terms` and open them in a browser."
  [terms]
  (throw (ex-info "Network/browser access required"
                  {:reason :network-required :url (search-url terms)})))
