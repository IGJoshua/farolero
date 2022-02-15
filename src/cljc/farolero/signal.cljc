(ns farolero.signal
  (:require
   [clojure.spec.alpha :as s]
   [farolero.protocols :refer [Jump]])
  #?(:clj
     (:import
      (farolero.signal Signal))))

#?(:cljs (defrecord Signal [target args]))

(defn make-signal
  [target args]
  (#?(:clj Signal. :cljs ->Signal) target args))
(s/fdef make-signal
  :args (s/cat :target (s/or :keyword keyword?
                             :internal-integer integer?)
               :args (s/coll-of any?)))

(extend-protocol Jump
  Signal
  (args [signal]
    (.-args signal))
  (is-target? [signal v]
    (= (.-target signal) v)))
