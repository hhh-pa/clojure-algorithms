(ns clojure-algorithms.web-programming.download-images-from-google-query
  "Google image search + download.

  STUB: searching, parsing and downloading need network access. The pure
  helpers (abuse cap, query params, folder/file naming) are ported.")

(def search-url "https://www.google.com/search")

(defn capped-max-images
  "Abuse cap: at most 50 images (mirrors the Python original)."
  [max-images]
  (min max-images 50))

(defn search-params
  "Query params for an image search of `query`."
  [query]
  {"q" query "tbm" "isch" "hl" "en" "ijn" "0"})

(defn query-folder
  "Download folder name for `query`."
  [query]
  (str "query_" (clojure.string/replace query " " "_")))

(defn image-filename
  "File name for the `index`-th downloaded image."
  [folder index]
  (str folder "/original_size_img_" index ".jpg"))

(defn download-images-from-google-query
  "STUB: would download up to `max-images` images for `query`."
  ([] (download-images-from-google-query "dhaka" 5))
  ([query] (download-images-from-google-query query 5))
  ([query max-images]
   (throw (ex-info "Network access required: Google image search"
                   {:reason :network-required
                    :params (search-params query)
                    :max-images (capped-max-images max-images)}))))
