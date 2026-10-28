(ns clojure-algorithms.web-programming.covid-stats-via-xpath
  "COVID-19 stats scraping (Worldometers archive, XPath selectors).

  STUB: the Python original fetches live HTML (httpx2 + lxml). This
  namespace keeps the CovidData record and the pure extraction logic;
  fetching throws until network access is available.")

(def default-url
  (str "https://web.archive.org/web/20250825095350/"
       "https://www.worldometers.info/coronavirus/"))

(def xpath-str "//div[@class = \"maincounter-number\"]/span/text()")

(defrecord CovidData [cases deaths recovered])

(def unavailable (->CovidData "N/A" "N/A" "N/A"))

(defn covid-data-from-strings
  "Builds CovidData from the three scraped counter strings; returns the
  N/A record when the page shape is unexpected."
  [data]
  (if (= 3 (count data))
    (apply ->CovidData data)
    unavailable))

(defn covid-stats
  "STUB: would fetch and parse COVID stats from `url`."
  ([] (covid-stats default-url))
  ([url]
   (throw (ex-info "Network access required: GET worldometers archive"
                   {:reason :network-required :url url}))))
