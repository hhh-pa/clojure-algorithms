(ns clojure-algorithms.web-programming.instagram-crawler
  "Instagram profile scraping.

  PARTIAL PORT: fetching + HTML/JSON parsing need network and libraries
  (httpx2/BeautifulSoup/json) with no stdlib equivalent, so they are
  stubbed. The profile-URL builder, the embedded-JSON slice helper and
  every user-field accessor over a plain data map are ported."
  (:require [clojure.string :as str]))

(defn profile-url
  [username]
  (str "https://www.instagram.com/" username "/"))

(defn config-json-slice
  "Slices the embedded JSON blob out of a script body: from '{\"config\"'
  to the last char (mirrors the Python slice)."
  [script-text]
  (let [i (str/index-of script-text "{\"config\"")]
    (when i
      (subs script-text i (dec (count script-text))))))

(defn username_ [user-data] (get user-data "username"))
(defn fullname [user-data] (get user-data "full_name"))
(defn biography [user-data] (get user-data "biography"))
(defn email [user-data] (get user-data "business_email"))
(defn website [user-data] (get user-data "external_url"))
(defn number-of-followers [user-data]
  (get-in user-data ["edge_followed_by" "count"]))
(defn number-of-followings [user-data]
  (get-in user-data ["edge_follow" "count"]))
(defn number-of-posts [user-data]
  (get-in user-data ["edge_owner_to_timeline_media" "count"]))
(defn profile-picture-url [user-data] (get user-data "profile_pic_url_hd"))
(defn verified? [user-data] (get user-data "is_verified"))
(defn private? [user-data] (get user-data "is_private"))

(defn describe-user
  "One-line description (mirrors __str__)."
  [user-data]
  (str (fullname user-data) " (" (username_ user-data) ") is "
       (biography user-data)))

(defn fetch-user-data
  "STUB: would scrape and return the user data map for `username`."
  [username]
  (throw (ex-info "Network access required: GET instagram.com"
                  {:reason :network-required :url (profile-url username)})))
