(ns farolero.proto
  "Protocols necessary for the implementation of control flow operators.")

(defprotocol Jump
  "Internal protocol for jumping to locations for restarts."
  (args [_]
    "Returns arguments made to the [[farolero.core/signal]] that triggered the jump.")
  (is-target? [_ v]
    "Checks to see if the value is this jump's target."))
