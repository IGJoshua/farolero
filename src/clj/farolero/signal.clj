(ns farolero.signal
  "Implementation of the [[farolero.proto.Jump]] protocol for Clojure.
  On the JVM, only classes that extend [[Throwable]] are permitted to be thrown.
  In Clojure the conventional methods of extending types ([[deftype]],
  [[defrecord]]) don't allow extending concrete classes, which just leaves
  [[gen-class]]. Because this relies on [[gen-class]], it must be compiled
  before it is used."
  (:require
   [farolero.proto :refer [Jump]])
  (:gen-class
   :name farolero.signal.Signal
   :extends Error
   :implements [farolero.proto.Jump]
   :init init
   :constructors {[Object Object] []}
   :state state))

(defn -init
  [target args]
  [[] {:target target :args args}])

(defn -args
  [this]
  (:args (.state this)))

(defn -is_target_QMARK_
  [this target]
  (= (:target (.state this)) target))

(defn -fillInStackTrace
  [this]
  this)
