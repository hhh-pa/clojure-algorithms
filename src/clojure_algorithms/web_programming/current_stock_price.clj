(ns clojure-algorithms.web-programming.current-stock-price
  "Yahoo Finance stock price scraping.

  STUB: fetching + HTML parsing (httpx2 + BeautifulSoup) need network and
  an HTML parser. The quote URL builder and the missing-tag message are
  ported as pure logic.")

(def missing-tag-message
  "No <fin-streamer> tag with the specified data-testid attribute found.")

(defn quote-url
  "Yahoo Finance quote URL for `symbol`."
  [symbol]
  (str "https://finance.yahoo.com/quote/" symbol "?p=" symbol))

(defn stock-price
  "STUB: would return the current price string for `symbol`."
  ([] (stock-price "AAPL"))
  ([symbol]
   (throw (ex-info "Network access required: GET finance.yahoo.com"
                   {:reason :network-required :url (quote-url symbol)}))))
