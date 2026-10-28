(ns clojure-algorithms.web-programming.current-weather
  "Current weather via OpenWeatherMap / Weatherstack.

  STUB: API calls need network access plus API keys. Key validation and
  endpoint constants are ported as pure logic.")

(def openweathermap-url-base "https://api.openweathermap.org/data/2.5/weather")
(def weatherstack-url-base "http://api.weatherstack.com/current")

(defn openweathermap-params
  [location api-key]
  {"q" location "appid" api-key})

(defn weatherstack-params
  [location api-key]
  {"query" location "access_key" api-key})

(defn current-weather
  "STUB: would return weather data for `location`. With no API keys it
  raises, mirroring the Python ValueError."
  ([location] (current-weather location "" ""))
  ([location openweathermap-key weatherstack-key]
   (when (and (empty? openweathermap-key) (empty? weatherstack-key))
     (throw (IllegalArgumentException.
             "No API keys provided or no valid data returned.")))
   (throw (ex-info "Network access required: GET weather APIs"
                   {:reason :network-required :location location}))))
