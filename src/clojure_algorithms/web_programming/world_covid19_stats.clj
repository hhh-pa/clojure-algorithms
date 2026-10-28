(ns clojure-algorithms.web-programming.world-covid19-stats
  "Worldwide COVID-19 stats scraping (worldometers.info).

  STUB: fetching + HTML parsing need network and an HTML parser. The
  key/value zip and report formatting are ported as pure logic."
  (:require [clojure.string :as str]))

(def default-url "https://www.worldometers.info/coronavirus/")

(defn stats-map
  "Zips scraped `keys` and `values` (stripped) into a stats map."
  [keys values]
  (into {} (map (fn [k v] [(str/trim k) (str/trim v)]) keys values)))

(defn format-report
  "Renders a stats map as the multi-line report the script prints."
  [stats]
  (str/join "\n" (mapcat (fn [[k v]] [k v]) stats)))

(defn world-covid19-stats
  "STUB: would return the stats map scraped from `url`."
  ([] (world-covid19-stats default-url))
  ([url]
   (throw (ex-info "Network access required: GET worldometers.info"
                   {:reason :network-required :url url}))))
