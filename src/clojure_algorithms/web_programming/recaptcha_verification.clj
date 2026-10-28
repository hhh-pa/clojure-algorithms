(ns clojure-algorithms.web-programming.recaptcha-verification
  "reCAPTCHA verification for a Django-style login view.

  STUB: the Python original is Django glue plus a Google API call; neither
  exists here. The verify payload builder, success check and view decision
  logic are ported as pure functions over plain maps.")

(def verify-url "https://www.google.com/recaptcha/api/siteverify")

(defn verify-payload
  "POST payload for verifying `client-key` with `secret-key`."
  [secret-key client-key]
  {"secret" secret-key "response" client-key})

(defn verified?
  "True when the siteverify response map reports success."
  [response]
  (boolean (get response "success" false)))

(defn login-decision
  "Pure view logic. `request` is {:method :params}. `verify-fn` maps a
  client key to a siteverify response map; `auth-fn` maps [user pass] to
  a user (truthy) or nil. Returns a decision map."
  [request verify-fn auth-fn]
  (if (not= "POST" (:method request))
    {:action :render :template "login.html"}
    (let [params (:params request)
          response (verify-fn (get params "g-recaptcha-response"))]
      (if (and (verified? response)
               (auth-fn [(get params "username") (get params "password")]))
        {:action :redirect :to "/your-webpage"
         :user (get params "username")}
        {:action :render :template "login.html"}))))

(defn login-using-recaptcha
  "STUB: would run the full login flow against Google + Django."
  [request]
  (throw (ex-info "Network/Django required: reCAPTCHA login"
                  {:reason :network-required :method (:method request)})))
