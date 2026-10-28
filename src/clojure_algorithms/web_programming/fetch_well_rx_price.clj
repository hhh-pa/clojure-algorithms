(ns clojure-algorithms.web-programming.fetch-well-rx-price
  "Prescription price scraping from wellrx.com.

  STUB: fetching + HTML parsing need network and an HTML parser. The
  input guard (nil -> nil) and request URL builder are ported.")

(def base-url "https://www.wellrx.com/prescriptions/{}/{}/?freshSearch=true")

(defn request-url
  [drug-name zip-code]
  (-> base-url
      (clojure.string/replace-first "{}" drug-name)
      (clojure.string/replace-first "{}" zip-code)))

(defn fetch-pharmacy-and-price-list
  "Returns nil unless both inputs are given (mirrors the Python guard);
  otherwise STUB: would return [{:pharmacy-name :price}]."
  [drug-name zip-code]
  (if (or (empty? drug-name) (empty? zip-code))
    nil
    (throw (ex-info "Network access required: GET wellrx.com"
                    {:reason :network-required
                     :url (request-url drug-name zip-code)}))))
