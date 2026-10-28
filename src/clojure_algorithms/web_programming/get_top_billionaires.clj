(ns clojure-algorithms.web-programming.get-top-billionaires
  "Forbes real-time billionaires.

  PARTIAL PORT: age computation, worth formatting and row building are
  pure and fully ported with the doctest cases; only the Forbes API fetch
  is stubbed (network)."
  (:require [clojure.string :as str])
  (:import [java.time Instant LocalDate ZoneOffset]))

(def limit 10)

(def api-url
  (str "https://www.forbes.com/forbesapi/person/rtb/0/position/true.json"
       "?fields=personName,gender,source,countryOfCitizenship,birthDate,finalWorth"
       "&limit=" limit))

(defn years-old
  "Age in years for a birth epoch-seconds `birth-timestamp`, as of `today`
  (defaults to today UTC)."
  ([birth-timestamp] (years-old birth-timestamp (LocalDate/now ZoneOffset/UTC)))
  ([birth-timestamp today]
   (let [birth (.toLocalDate (.atZone (Instant/ofEpochSecond (long birth-timestamp))
                                      ZoneOffset/UTC))
         years (- (.getYear today) (.getYear birth))]
     (if (neg? (compare [(.getMonthValue today) (.getDayOfMonth today)]
                        [(.getMonthValue birth) (.getDayOfMonth birth)]))
       (dec years)
       years))))

(defn format-worth
  "Formats a finalWorth value (millions) as \"X.X Billion\"."
  [final-worth]
  (format "%.1f Billion" (double (/ final-worth 1000))))

(defn person-row
  "Builds the display row map for one Forbes `person` entry."
  ([person] (person-row person (LocalDate/now ZoneOffset/UTC)))
  ([person today]
   {"Name" (get person "personName")
    "Source" (get person "source")
    "Country" (get person "countryOfCitizenship")
    "Gender" (get person "gender")
    "Worth ($)" (format-worth (get person "finalWorth"))
    "Age" (str (years-old (/ (get person "birthDate") 1000) today))}))

(defn format-table
  "Renders billionaire `rows` as a plain-text table string."
  [rows]
  (let [cols (keys (first rows))
        lines (map (fn [row] (str/join " | " (map #(get row %) cols))) rows)]
    (str (str/join " | " cols) "\n" (str/join "\n" lines))))

(defn get-forbes-real-time-billionaires
  "STUB: would fetch the top-10 list from the Forbes API."
  []
  (throw (ex-info "Network access required: GET forbes.com API"
                  {:reason :network-required :url api-url})))
