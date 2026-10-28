(ns clojure-algorithms.web-programming.search-books-by-isbn
  "Book/author lookup via Open Library.

  PARTIAL PORT: OLID validation + URL building, ISBN validation and the
  pure summary projection are fully ported; HTTP fetches are stubbed."
  (:require [clojure.string :as str]))

(defn normalize-olid
  "Trims whitespace/slashes and validates the `olid` shape (one slash).
  Returns the normalized olid; throws IllegalArgumentException otherwise."
  [olid]
  (let [clean (str/replace olid #"^[\s/]+|[\s/]+$" "")]
    (when-not (= 1 (count (filter #(= \/ %) clean)))
      (throw (IllegalArgumentException.
              (str olid " is not a valid Open Library olid"))))
    clean))

(defn olid-url
  [olid]
  (str "https://openlibrary.org/" (normalize-olid olid) ".json"))

(defn valid-isbn?
  "True when `isbn` is a 10- or 13-digit string."
  [isbn]
  (boolean (and (contains? #{10 13} (count isbn))
                (re-matches #"\d+" isbn))))

(def summary-keys
  {"title" "Title" "publish_date" "Publish date" "authors" "Authors"
   "number_of_pages" "Number of pages" "isbn_10" "ISBN (10)"
   "isbn_13" "ISBN (13)"})

(defn summarize-book-data
  "Pure projection of Open Library `book-data` onto summary keys.
  `resolve-author` maps an author key (\"/authors/...\") to a name."
  [book-data resolve-author]
  (let [data (into {} (map (fn [[k better]] [better (get book-data k)])
                           summary-keys))]
    (into {} (map (fn [[k v]]
                    [k (if (and (= k "Authors") (vector? v))
                         (str/join ", " (map #(resolve-author (get % "key")) v))
                         (if (vector? v) (str/join ", " v) v))])
                  data))))

(defn get-openlibrary-data
  "STUB: would return the decoded JSON map for `olid`."
  ([] (get-openlibrary-data "isbn/0140328726"))
  ([olid]
   (throw (ex-info "Network access required: GET openlibrary.org"
                   {:reason :network-required :url (olid-url olid)}))))

(defn summarize-book
  "STUB: would summarize live Open Library `book-data`."
  [book-data]
  (summarize-book-data book-data
                       (fn [author-key]
                         (get (get-openlibrary-data author-key) "name"))))
