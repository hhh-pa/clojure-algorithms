(ns clojure-algorithms.web-programming.get-top-hn-posts
  "Hacker News top stories.

  PARTIAL PORT: URL builders and the stories->markdown renderer are pure
  and fully ported; the Firebase fetches are stubbed (network)."
  (:require [clojure.string :as str]))

(def topstories-url
  "https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty")

(defn story-url
  [story-id]
  (str "https://hacker-news.firebaseio.com/v0/item/" story-id
       ".json?print=pretty"))

(defn stories-as-markdown
  "Renders story maps (with :title/:url or \"title\"/\"url\") as a
  bullet markdown list."
  [stories]
  (str/join "\n" (map (fn [s]
                        (let [title (or (:title s) (get s "title"))
                              url (or (:url s) (get s "url"))]
                          (str "* [" title "](" url ")")))
                      stories)))

(defn get-hackernews-story
  "STUB: would fetch one story map."
  [story-id]
  (throw (ex-info "Network access required: GET Hacker News API"
                  {:reason :network-required :url (story-url story-id)})))

(defn hackernews-top-stories
  "STUB: would fetch the top `max-stories` story maps."
  ([] (hackernews-top-stories 10))
  ([max-stories]
   (throw (ex-info "Network access required: GET Hacker News API"
                   {:reason :network-required :url topstories-url
                    :max-stories max-stories}))))

(defn hackernews-top-stories-as-markdown
  "STUB: would fetch and render the top stories as markdown."
  ([] (hackernews-top-stories-as-markdown 10))
  ([max-stories]
   (stories-as-markdown (hackernews-top-stories max-stories))))
