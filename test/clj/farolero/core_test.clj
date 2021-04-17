(ns farolero.core-test
  ""
  (:require
   [clojure.test :as t]
   [farolero.core :as sut :refer [handler-bind handler-case restart-case]]))

(t/deftest test-abort
  (t/is (= :good
           (restart-case (do (sut/abort) :bad)
             (::sut/abort [] :good)))
        "abort restarts unwind the stack and return their result"))

(t/deftest test-assert
  (t/is (nil? (sut/assert true))
        "assert returns nil when the test passes")
  (t/is (nil? (sut/assert true []))
        "assert returns nil when the test passes and no places are specified")
  (t/is (= 17
           (let [x (volatile! nil)]
             (handler-bind [::sut/error (fn [c]
                                          (vreset! x 17)
                                          (let [r (sut/find-restart ::sut/continue c)]
                                            (when r (sut/invoke-restart r))))]
               (sut/assert @x)
               @x)))
        (str "assert signals an error which derives from `:farolero.core/error`"
             " when the test fails, and binds a `:farolero.core/continue`"
             " restart."))
  (t/is (= 17
           (let [x (volatile! nil)]
             (handler-bind [::sut/error (fn [c]
                                          (vreset! x 17)
                                          (sut/continue c))]
               (sut/assert @x)
               @x)))
        (str "assert signals an error when the test fails, and farolero.core/continue"
             " allows execution to continue"))
  (t/is (nil? (let [x (volatile! nil)]
                (sut/assert true [x])))
        "assert does nothing with the places when it passes")
  (t/is (= 17
           (let [x (volatile! nil)]
             (handler-bind [::sut/simple-error (fn [c]
                                                 (vreset! x 17)
                                                 (sut/continue c))]
               (sut/assert @x [] ::sut/simple-error)
               @x)))
        "the user can specify what type of error occurs with the assert")
  (t/is (= 6
           (let [x (volatile! 0)]
             (handler-bind [::sut/type-error (fn [c]
                                               (vswap! x inc)
                                               (sut/continue c))]
               (sut/assert (> @x 5) [] ::sut/type-error)
               @x)))
        "the assertion is retried after continuing")
  (t/is (= 6
           (let [x (volatile! 0)]
             (handler-bind [::sut/type-error (fn [_]
                                               (vswap! x inc)
                                               (sut/continue))]
               (sut/assert (> @x 5) [] ::sut/type-error)
               @x)))
        "the assertion is still retried without passing the related condition"))

(t/deftest test-cerror
  (t/is (= :good
           (handler-case (do (sut/cerror "Keep going.") :bad)
             (::sut/simple-error [c] :good)))
        "cerror signals a simple-error when called with a string")
  (t/is (= 10
           (handler-bind [::sut/simple-error (fn [c] (sut/continue c))]
             (sut/cerror "Wooo")
             10))
        "cerror binds a continue restart")
  (t/is (= :good
           (sut/block test-block
             (binding [sut/*debugger-hook* (fn [& args] (sut/return-from test-block :good))]
               (sut/cerror)
               :bad)))
        "cerror invokes the debugger when no handler is bound"))

(t/deftest test-check-type
  (t/is ))
