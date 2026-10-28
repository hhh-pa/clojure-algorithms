(ns clojure-algorithms.web-programming.nasa-data
  "NASA APOD + image archive API.

  STUB: API calls need network access plus an API key. Endpoint builders
  and the image-name/path helpers are ported."
  (:require [clojure.string :as str]))

(def apod-url "https://api.nasa.gov/planetary/apod")
(def archive-url "https://images-api.nasa.gov/search")

(defn apod-params
  [api-key]
  {"api_key" api-key})

(defn archive-params
  [query]
  {"q" query})

(defn image-name
  "File name from an image URL (last path segment)."
  [img-url]
  (last (str/split img-url #"/")))

(defn save-path
  [path img-name]
  (str path "/" img-name))

(defn get-apod-data
  "STUB: would return the APOD data map."
  [api-key]
  (throw (ex-info "Network access required: GET api.nasa.gov"
                  {:reason :network-required :url apod-url
                   :params (apod-params api-key)})))

(defn save-apod
  "STUB: would save the APOD image under `path`."
  ([api-key] (save-apod api-key "."))
  ([api-key path]
   (throw (ex-info "Network access required: GET api.nasa.gov"
                   {:reason :network-required :path path}))))

(defn get-archive-data
  "STUB: would return archive search results for `query`."
  [query]
  (throw (ex-info "Network access required: GET images-api.nasa.gov"
                  {:reason :network-required :url archive-url
                   :params (archive-params query)})))
