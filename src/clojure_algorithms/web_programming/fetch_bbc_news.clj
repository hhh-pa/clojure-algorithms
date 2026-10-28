(ns clojure-algorithms.web-programming.fetch-bbc-news
  "BBC news headlines via newsapi.org.

  STUB: fetching needs network access plus an API key. URL building and
  headline formatting are ported as pure logic.")

(def news-api-base
  "https://newsapi.org/v1/articles?source=bbc-news&sortBy=top&apiKey=")

(defn request-url
  [api-key]
  (str news-api-base api-key))

(defn format-headline
  "Formats the i-th article headline line."
  [i article]
  (str i ".) " (get article "title")))

(defn fetch-bbc-news
  "STUB: would print top BBC headlines."
  [api-key]
  (throw (ex-info "Network access required: GET newsapi.org"
                  {:reason :network-required :url (request-url api-key)})))
