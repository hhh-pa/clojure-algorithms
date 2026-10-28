(ns clojure-algorithms.web-programming.slack-message
  "Slack incoming-webhook messages.

  STUB: posting needs network access. Payload building and the error
  formatting are ported as pure logic.")

(def json-headers {"Content-Type" "application/json"})

(defn message-payload
  [message-body]
  {"text" message-body})

(defn slack-error-message
  [status-code response-text]
  (str "Request to slack returned an error " status-code
       ", the response is:\n" response-text))

(defn send-slack-message
  "STUB: would POST `message-body` to the webhook `slack-url`."
  [message-body slack-url]
  (throw (ex-info "Network access required: POST slack webhook"
                  {:reason :network-required :url slack-url
                   :payload (message-payload message-body)})))
