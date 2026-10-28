(ns clojure-algorithms.web-programming.crypto-price-tracker
  "Cryptocurrency price lookup via the CoinGecko API.

  STUB: the HTTP call needs network access. URL building and response
  extraction are ported as pure functions.")

(defn price-url
  "CoinGecko simple-price URL for `coin`."
  [coin]
  (str "https://api.coingecko.com/api/v3/simple/price?ids=" coin
       "&vs_currencies=usd"))

(defn extract-price
  "Extracts the USD price for `coin` from a decoded API response map,
  defaulting to 0.0 (mirrors the Python fallback)."
  [response coin]
  (double (get-in response [coin "usd"] 0.0)))

(defn crypto-price
  "STUB: would return the live USD price of `coin`."
  ([] (crypto-price "bitcoin"))
  ([coin]
   (throw (ex-info "Network access required: GET CoinGecko"
                   {:reason :network-required :url (price-url coin)}))))
