(ns clojure-algorithms.web-programming.fetch-jobs
  "Job scraping from Indeed.

  STUB: fetching + HTML parsing need network and an HTML parser. The
  search URL builder is ported as pure logic.")

(def jobs-url-base "https://www.indeed.co.in/jobs?q=mobile+app+development&l=")

(defn jobs-url
  [location]
  (str jobs-url-base location))

(defn fetch-jobs
  "STUB: would return a seq of [job-title company] pairs."
  ([] (fetch-jobs "mumbai"))
  ([location]
   (throw (ex-info "Network access required: GET indeed.co.in"
                   {:reason :network-required :url (jobs-url location)}))))
