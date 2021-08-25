(ns farolero.extensions.flow
  "Extension for use with `fmnoise/flow`, which ensures that
  the [[farolero.signal.Signal]] exception isn't caught when using
  railway-oriented programming in combination with farolero."
  (:require
   [farolero.signal :refer [#?(:cljs Signal)]]
   [fmnoise.flow :as flow])
  #?(:clj
     (:import
      (farolero.signal Signal))))

(extend-protocol flow/Catch
  Signal
  (caught [s] (throw s)))
