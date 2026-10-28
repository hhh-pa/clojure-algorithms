(ns clojure-algorithms.web-programming.fetch-anime-and-play
  "Anime search / episode scraping (gogoanime).

  STUB: all three entry points fetch and parse live HTML. URL builders and
  the embed->playlist download-URL transform are ported as pure logic."
  (:require [clojure.string :as str]))

(def base-url "https://ww7.gogoanime2.org")

(defn search-url
  [anime-name]
  (str base-url "/search?keyword=" anime-name))

(defn episode-page-url
  [endpoint]
  (str base-url endpoint))

(defn download-url
  "Derives the .m3u8 download URL from an embed `episode-url`."
  [episode-url]
  (str (str/replace episode-url "/embed/" "/playlist/") ".m3u8"))

(defn search-scraper
  "STUB: would return [{:title :url}] anime matches."
  [anime-name]
  (throw (ex-info "Network access required: gogoanime search"
                  {:reason :network-required :url (search-url anime-name)})))

(defn search-anime-episode-list
  "STUB: would return [{:title :url}] episodes."
  [episode-endpoint]
  (throw (ex-info "Network access required: gogoanime episodes"
                  {:reason :network-required
                   :url (episode-page-url episode-endpoint)})))

(defn get-anime-episode
  "STUB: would return [watch-url download-url]."
  [episode-endpoint]
  (throw (ex-info "Network access required: gogoanime episode"
                  {:reason :network-required
                   :url (episode-page-url episode-endpoint)})))
