(ns clojure-algorithms.web-programming.fetch-quotes
  "Quotes from the ZenQuotes API (free tier, no key needed).

  STUB: fetching needs network access. Endpoint constants are ported.")

(def api-endpoint-url "https://zenquotes.io/api")
(def today-url (str api-endpoint-url "/today"))
(def random-url (str api-endpoint-url "/random"))

(defn quote-of-the-day
  "STUB: would return today's quote entries."
  []
  (throw (ex-info "Network access required: GET zenquotes.io"
                  {:reason :network-required :url today-url})))

(defn random-quotes
  "STUB: would return random quote entries."
  []
  (throw (ex-info "Network access required: GET zenquotes.io"
                  {:reason :network-required :url random-url})))
