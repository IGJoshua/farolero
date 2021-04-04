(ns farolero.signal
  "Implementation of the [[farolero.proto.Jump]] protocol for ClojureScript.
  Since JavaScript allows any kind of object to be thrown, this is a
  simple [[defrecord]]."
  (:require
   [farolero.proto :refer [Jump]]))

(defrecord Signal [target args]
  Jump
  (args [_] args)
  (is-target? [_ v] (= target v)))
