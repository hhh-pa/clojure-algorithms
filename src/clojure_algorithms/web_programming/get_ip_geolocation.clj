(ns clojure-algorithms.web-programming.get-ip-geolocation
  "IP geolocation via ipinfo.io.

  STUB: the API call needs network access. URL building and response
  formatting are ported as pure logic.")

(defn api-url
  [ip-address]
  (str "https://ipinfo.io/" ip-address "/json"))

(defn format-location
  "Formats a decoded ipinfo response map as a location string."
  [data]
  (if (and (contains? data "city")
           (contains? data "region")
           (contains? data "country"))
    (str "Location: " (get data "city") ", " (get data "region") ", "
         (get data "country"))
    "Location data not found."))

(defn get-ip-geolocation
  "STUB: would return the location string for `ip-address`."
  [ip-address]
  (throw (ex-info "Network access required: GET ipinfo.io"
                  {:reason :network-required :url (api-url ip-address)})))
