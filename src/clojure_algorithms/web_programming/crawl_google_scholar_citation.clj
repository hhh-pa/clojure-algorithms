(ns clojure-algorithms.web-programming.crawl-google-scholar-citation
  "Google Scholar citation lookup.

  STUB: the Python original fetches scholar.google.com HTML (httpx2) and
  parses it (BeautifulSoup). No HTML parser exists in Clojure stdlib, so
  only the endpoint constant and params helper are ported.")

(def scholar-lookup-url "https://scholar.google.com/scholar_lookup")

(defn citation-params
  "Query params map for a publication lookup."
  [title journal volume pages year]
  {"title" title "journal" journal "volume" volume
   "pages" pages "year" year "hl" "en"})

(defn get-citation
  "STUB: would return the citation count string for `params`."
  [base-url params]
  (throw (ex-info "Network access required: GET scholar.google.com"
                  {:reason :network-required
                   :url base-url :params params})))
