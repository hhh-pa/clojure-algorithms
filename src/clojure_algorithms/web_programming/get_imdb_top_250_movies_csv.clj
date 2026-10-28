(ns clojure-algorithms.web-programming.get-imdb-top-250-movies-csv
  "IMDb Top-250 scraping + CSV export.

  PARTIAL PORT: fetching needs network and an HTML parser (stubbed), but
  CSV writing from a {title rating} map is fully ported."
  (:require [clojure.java.io :as io]))

(def default-url "https://www.imdb.com/chart/top/?ref_=nv_mv_250")

(defn write-movies-csv
  "Writes {title rating} to `filename` as CSV. Returns the filename."
  [movies filename]
  (with-open [w (io/writer filename)]
    (.write w "Movie title,IMDb rating\n")
    (doseq [[title rating] movies]
      (.write w (str "\"" (clojure.string/replace title "\"" "\"\"") "\""
                     "," rating "\n"))))
  filename)

(defn get-imdb-top-250-movies
  "STUB: would return {title rating} scraped from IMDb."
  ([] (get-imdb-top-250-movies ""))
  ([url]
   (throw (ex-info "Network access required: GET imdb.com"
                   {:reason :network-required
                    :url (if (empty? url) default-url url)}))))

(defn write-movies
  "STUB: would fetch the chart and write IMDb_Top_250_Movies.csv."
  ([] (write-movies "IMDb_Top_250_Movies.csv"))
  ([filename]
   (write-movies-csv (get-imdb-top-250-movies) filename)))
