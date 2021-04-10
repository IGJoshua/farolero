(ns farolero.proto
  "Protocols necessary for the implementation of control flow operators.")

(defprotocol Jump
  "Internal protocol for jumping to locations for restarts."
  (args [_]
    "Returns an argument list used in the construction of the [[Jump]].")
  (is-target? [_ v]
    "Checks to see if the value is this jump's target."))
