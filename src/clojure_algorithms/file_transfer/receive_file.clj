(ns clojure-algorithms.file-transfer.receive-file
  "STUB: network-dependent file receiver, not ported.

  The Python original (file_transfer/receive_file.py) connects to a TCP
  socket, sends a greeting, and streams the response into a local file.
  There is no pure algorithmic logic to port, so this namespace only
  documents the omission. `receive-file` throws to make that explicit.")

(defn receive-file
  "Stub for the socket-based file receiver. Always throws."
  [& _args]
  (throw (ex-info "Not ported: requires a live TCP socket connection."
                  {:reason :network-dependent
                   :source "file_transfer/receive_file.py"})))
