(ns clojure-algorithms.web-programming.co2-emission
  "UK CO2 emission data (CarbonIntensity API).

  STUB: the Python original performs live HTTPS requests (httpx2). This
  namespace keeps the pure URL-building logic and the endpoint constant;
  the fetching functions throw until network access is available.")

(def base-url "https://api.carbonintensity.org.uk/intensity")

(defn intensity-url
  "URL querying intensity between `start` and `end` (date strings)."
  [start end]
  (str base-url "/" start "/" end))

(defn fetch-last-half-hour
  "STUB: would return the actual intensity of the last half hour."
  []
  (throw (ex-info "Network access required: GET carbonintensity.org.uk"
                  {:reason :network-required :url base-url})))

(defn fetch-from-to
  "STUB: would return intensity entries between `start` and `end`."
  [start end]
  (throw (ex-info "Network access required: GET carbonintensity.org.uk"
                  {:reason :network-required
                   :url (intensity-url start end)})))
