(ns clojure-algorithms.file-transfer.send-file
  "STUB: network-dependent file sender, not ported.

  The Python original (file_transfer/send_file.py) binds a TCP socket,
  accepts a client, and streams a file over the connection. There is no
  pure algorithmic logic to port, so this namespace only documents the
  omission. `send-file` throws to make that explicit.")

(defn send-file
  "Stub for the socket-based file sender. Always throws."
  [& _args]
  (throw (ex-info "Not ported: requires a live TCP socket connection."
                  {:reason :network-dependent
                   :source "file_transfer/send_file.py"})))
