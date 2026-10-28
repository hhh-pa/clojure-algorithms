(ns clojure-algorithms.web-programming.fetch-github-info
  "Authenticated GitHub user info via api.github.com.

  STUB: the API call needs network access plus a token. Endpoint
  constants, header building and token validation are ported.")

(def base-url "https://api.github.com")
(def authenticated-user-endpoint (str base-url "/user"))

(defn auth-headers
  "Authorization headers for `token`."
  [token]
  {"Authorization" (str "token " token)
   "Accept" "application/vnd.github.v3+json"})

(defn user-token
  "USER_TOKEN from the environment (\"\" when unset)."
  []
  (or (System/getenv "USER_TOKEN") ""))

(defn fetch-github-info
  "STUB: would return the authenticated user's info map."
  [auth-token]
  (throw (ex-info "Network access required: GET api.github.com/user"
                  {:reason :network-required
                   :url authenticated-user-endpoint
                   :headers (auth-headers auth-token)})))
