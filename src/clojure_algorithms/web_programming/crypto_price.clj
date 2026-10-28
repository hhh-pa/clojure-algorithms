(ns clojure-algorithms.web-programming.crypto-price
  "ETH -> USD conversion with a live CoinGecko price.

  STUB: the price fetch needs network access. The pure multiplication is
  ported as `convert-eth`."
  (:require [clojure.string :as str]))

(def coingecko-url
  "https://api.coingecko.com/api/v3/simple/price?ids=ethereum&vs_currencies=usd")

(defn convert-eth
  "Converts `eth-amount` to USD at the given `price-usd`."
  [eth-amount price-usd]
  (* eth-amount price-usd))

(defn format-conversion
  "Formats the conversion line printed by the original script."
  [eth-amount usd-value]
  (format "%s ETH = $%.2f USD" (str eth-amount) (double usd-value)))

(defn get-eth-price-usd
  "STUB: would fetch the live ETH price in USD."
  []
  (throw (ex-info "Network access required: GET CoinGecko"
                  {:reason :network-required :url coingecko-url})))

(defn eth-to-usd
  "STUB: would convert `eth-amount` at the live price."
  [eth-amount]
  (convert-eth eth-amount (get-eth-price-usd)))
