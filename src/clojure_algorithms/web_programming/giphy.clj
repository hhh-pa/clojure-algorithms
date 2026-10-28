(ns clojure-algorithms.web-programming.giphy
  "GIF search via the Giphy API.

  STUB: the API call needs network access plus an API key. Query
  formatting, URL building and gif-url extraction are ported."
  (:require [clojure.string :as str]))

(def api-key-placeholder "YOUR API KEY")

(defn format-query
  [query]
  (str/join "+" (str/split query #"\s+")))

(defn search-url
  [query api-key]
  (str "https://api.giphy.com/v1/gifs/search?q=" (format-query query)
       "&api_key=" api-key))

(defn extract-gif-urls
  "Pulls gif page URLs out of a decoded \"data\" list."
  [data]
  (mapv #(get % "url") data))

(defn get-gifs
  "STUB: would return gif URLs for `query`."
  ([query] (get-gifs query api-key-placeholder))
  ([query api-key]
   (throw (ex-info "Network access required: GET api.giphy.com"
                   {:reason :network-required
                    :url (search-url query api-key)}))))
