(ns clojure-algorithms.web-programming.get-amazon-product-data
  "Amazon product scraping (pandas DataFrame in the Python original).

  STUB: fetching + HTML parsing need network and an HTML parser. The
  search URL builder and the discount computation are ported as pure
  logic; rows are plain maps instead of DataFrame rows.")

(def columns
  ["Product Title" "Product Link" "Current Price of the product"
   "Product Rating" "MRP of the product" "Discount"])

(defn search-url
  [product]
  (str "https://www.amazon.in/laptop/s?k=" product))

(defn product-link
  [href]
  (str "https://www.amazon.in/" href))

(defn- parse-rupees
  [s]
  (Double/parseDouble (clojure.string/replace
                       (clojure.string/replace s "₹" "") "," "")))

(defn compute-discount
  "Discount % from MRP and price strings (\"₹1,000\" style).
  Returns NaN on unparseable input (mirrors the Python ValueError path)."
  [mrp price]
  (try
    (* (/ (- (parse-rupees mrp) (parse-rupees price))
          (parse-rupees mrp))
       100.0)
    (catch NumberFormatException _
      Double/NaN)))

(defn get-amazon-product-data
  "STUB: would return a vector of product row maps."
  ([] (get-amazon-product-data "laptop"))
  ([product]
   (throw (ex-info "Network access required: GET amazon.in"
                   {:reason :network-required :url (search-url product)}))))
