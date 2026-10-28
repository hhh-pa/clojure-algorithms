(ns clojure-algorithms.web-programming.random-anime-character
  "Random anime character from mywaifulist.moe.

  STUB: fetching + HTML parsing need network and an HTML parser. The
  image filename builder is ported as pure logic."
  (:require [clojure.string :as str]))

(def random-url "https://www.mywaifulist.moe/random")

(defn image-title
  "Local filename for a character: title with spaces as _, plus the
  image URL's extension."
  [title image-url]
  (let [base (last (str/split image-url #"/"))
        ext (if (str/includes? base ".")
              (subs base (str/last-index-of base "."))
              "")]
    (str (str/replace (str/trim title) " " "_") ext)))

(defn random-anime-character
  "STUB: would return [title description image-title]."
  []
  (throw (ex-info "Network access required: GET mywaifulist.moe"
                  {:reason :network-required :url random-url})))
