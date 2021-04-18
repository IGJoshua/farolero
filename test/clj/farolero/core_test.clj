(ns farolero.core-test
  ""
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.test :as t]
   [farolero.core :as sut :refer [handler-bind handler-case restart-case
                                  block return-from
                                  values]])
  (:import
   (java.io PushbackReader)))

(t/deftest test-abort
  (t/is (= :good
           (restart-case (do (sut/abort) :bad)
             (::sut/abort [] :good)))
        "calls the abort restart"))

(t/deftest test-assert
  (t/is (nil? (sut/assert true))
        "returns nil when the test passes")
  (t/is (nil? (sut/assert true []))
        "returns nil when the test passes and no places are specified")
  (t/is (= 17
           (let [x (volatile! nil)]
             (handler-bind [::sut/error (fn [c]
                                          (vreset! x 17)
                                          (let [r (sut/find-restart ::sut/continue c)]
                                            (when r (sut/invoke-restart r))))]
               (sut/assert @x)
               @x)))
        (str "signals an error which derives from `:farolero.core/error`"
             " when the test fails, and binds a `:farolero.core/continue`"
             " restart."))
  (t/is (= 17
           (let [x (volatile! nil)]
             (handler-bind [::sut/error (fn [c]
                                          (vreset! x 17)
                                          (sut/continue c))]
               (sut/assert @x)
               @x)))
        (str "signals an error when the test fails, and farolero.core/continue"
             " allows execution to continue"))
  (t/is (nil? (let [x (volatile! nil)]
                (sut/assert true [x])))
        "does nothing with the places when it passes")
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
        "the assertion is still retried without passing the related condition")
  (t/is (= 17
           (with-in-str "0\ny\n(vreset! farolero.core/*place* 17)\n"
             (let [x (volatile! nil)]
               (with-out-str
                 (sut/assert @x [x]))
               @x)))
        "the continue restart allows you to set the value interactively from the debugger"))

(t/deftest test-cerror
  (t/is (= :good
           (handler-case (do (sut/cerror "Keep going.") :bad)
             (::sut/simple-error [c & args] :good)))
        "signals a simple-error when called with a string")
  (t/is (= 10
           (handler-bind [::sut/simple-error (fn [c & args] (sut/continue c))]
             (sut/cerror "Wooo")
             10))
        "binds a continue restart")
  (t/is (= :good
           (sut/block test-block
             (binding [sut/*debugger-hook* (fn [& args] (sut/return-from test-block :good))]
               (sut/cerror)
               :bad)))
        "invokes the debugger when no handler is bound"))

(t/deftest test-check-type
  (t/is (nil? (let [x (volatile! 'a)]
                (sut/check-type x symbol?)))
        "returns normally when the check passes")
  (t/is (= :good
           (handler-case (let [x (volatile! 'a)]
                           (sut/check-type x integer?))
             (::sut/type-error [& args]
               :good)))
        "raises a type-error when the check fails")
  (t/is (= 15
           (let [x (volatile! 'a)]
             (handler-bind [::sut/type-error
                            (fn [c desc & {:as opts :keys [spec]}]
                              (sut/assert (not (s/valid? spec @x)))
                              (apply sut/store-value-fn vreset! 15 c desc (mapcat identity opts)))]

               (sut/check-type x number?))
             @x))
        "binds a store-value restart for use with store-value-fn")
  (t/is (nil? (let [x (volatile! 'a)]
                (sut/check-type x symbol? "a symbol")))
        "works fine with a type description")
  (t/is (= "abc"
           (let [x (volatile! 'a)]
             (handler-bind [::sut/type-error
                            (fn [c desc & {:as opts :keys [spec]}]
                              (sut/assert (not (s/valid? spec @x)))
                              (apply sut/store-value-fn vreset! "abc" c desc (mapcat identity opts)))]

               (sut/check-type x string? "a string"))
             @x))
        "binds the store value restart with a type description"))

(t/deftest test-compute-restarts
  (t/is (restart-case
            (let [r (sut/find-restart ::foo)]
              (= r (first (filter (comp #{::foo} ::sut/restart-name) (sut/compute-restarts)))))
          (::foo []))
        "contains bound restarts")
  (t/is (restart-case
            (= (sut/compute-restarts) (sut/compute-restarts))
          (::foo [])
          (::bar [])
          (::foo []))
        "returns the same list each time invoked in the same context")
  (t/is (= :good
           (restart-case
               (let [restarts (sut/compute-restarts)
                     foos (filter (comp #{::foo} ::sut/restart-name)
                                  restarts)
                     r (nth foos 1)]
                 (sut/invoke-restart r))
             (::foo [] :bad)
             (::foo [] :good)
             (::foo [] :bad)))
        "invoking restarts besides the first of a given name correctly finds them"))

(t/deftest test-continue
  (t/is (= :good
           (restart-case (do (sut/continue) :bad)
             (::sut/continue [] :good)))
        "invokes the continue restart"))

(t/deftest test-error
  (t/is (handler-case (sut/error "Error")
          (::sut/simple-error [c fmt & args]
            (= fmt "Error")))
        "signals a simple error with a format string")
  (t/is (handler-case (sut/error "Error %d" 10)
          (::sut/simple-error [c fmt & args]
            (= "Error 10" (apply format fmt args))))
        "passes additional format arguments as rest args")
  (t/is (= :good
           (with-in-str "0\n"
             (restart-case (do (with-out-str
                                 (sut/error "Some error"))
                               :bad)
               (::sut/continue [] :interactive (constantly nil) :good))))
        "invokes the debugger"))

(t/deftest test-handler-bind
  (t/is (nil? (handler-bind []))
        "can take no arguments")
  (t/is (= :good
           (block foo
             (handler-bind [::sut/error (fn [& args] (return-from foo :good))]
               (sut/error "an error")
               :bad)))
        "calls the handlers when they're signaled")
  (t/is (= :good
           (block foo
             (handler-bind [::sut/error (fn [& args] (return-from foo :good))]
               (handler-bind [::sut/error (fn [& args] (apply sut/error args))
                              ::sut/error (fn [& args] (return-from foo :bad))]
                 (sut/error "an error")
                 :bad))))
        "allows re-signaling errors further up the stack")
  (letfn [(handler-fn [& args]
            (return-from :foo :good))]
    (t/is (= :good
             (block :foo
               (handler-bind [::sut/simple-error handler-fn]
                 (sut/error "an error")
                 :bad)))
          "allows binding handler functions made elsewhere"))
  (t/is (= :good
           (block done
             (handler-bind [::sut/error (fn [c & args] c)
                            ::sut/simple-condition (fn [c & args] (return-from done :good))]
               (sut/error "an error")
               :bad)))
        "allows conditions to defer handling conditions to other handlers")
  (t/is (= :good
           (block done
             (handler-bind [::sut/error (fn [c & args] (return-from done :good))]
               (handler-bind [::sut/error (fn [c & args] c)]
                 (sut/error "an error")
                 :bad))))
        "allows conditions to defer handling conditions up the stack")
  (t/is (= :good
           (handler-bind [::sut/error (fn [c & args] (return-from :done :good))]
             (block :done
               (sut/error "an error")
               :bad)))
        "calls the handlers further down the stack than the body")
  (t/is (= :good
           (block done
             (handler-bind [::something identity
                            ::sut/error (fn [c & args] (return-from done :good))]
               (sut/error "an error")
               :bad)))
        "unmatched handlers don't get called")
  (t/is (= :good
           (block done
             (handler-bind [RuntimeException (fn [c & args] (return-from done :good))]
               (sut/error (RuntimeException. "An error!"))
               :bad)))
        "exceptions are valid handler types"))

(t/deftest test-handler-case
  (t/is (= :good
           (handler-case (do (sut/error "an error")
                             :bad)
             (::sut/error [& args] :good)))
        "immediately unwinds on a caught error being signaled")
  (t/is (= :good
           (handler-case (do (sut/error "an error")
                             :bad)
             (::sut/warning [& args] :bad)
             (::sut/error [& args] :good)))
        "calls the correct handler")
  (t/is (= :good
           (handler-case (do (sut/error "an error")
                             :bad)
             (::sut/error [& args] :good)
             (::sut/error [& args] :bad)))
        "calls the first handler first")
  (t/is (= :good
           (handler-case :good
             (::sut/error [& args] :bad)))
        "returns the value from the expression when no error is signaled")
  (t/is (= :good
           (handler-case
               (handler-case (do (sut/error "an error")
                                 :bad)
                 (::blah [& args] :bad))
             (::sut/error [& args] :good)))
        "grabs handlers further up the stack for matches")
  (t/is (= :good
           (handler-case
               (handler-case (do (sut/error "foo")
                                 :bad)
                 (::sut/error [& args] (apply sut/error args)))
             (::sut/error [& args] :good)))
        "allows you to re-raise conditions")
  (t/is (= 13
           (handler-case 10
             (:no-error [x] (+ x 3))))
        "allows a no-error clause, passing the return value")
  (t/is (= :good
           (handler-case :bad
             (:no-error [_] :good)))
        "the no-error clause return value is preferred to the old one")
  (t/is (= [5 4 3 2 1]
           (handler-case (values 1 2 3 4 5)
             (:no-error [a b c d e] [e d c b a])))
        "multiple values are passed to the no-error clause")
  (t/is (= :good
           (handler-case
               (handler-case
                   :bad
                 (::sut/error [& args] :bad)
                 (:no-error [_] (sut/error "foo")))
             (::sut/error [& args] :good)))
        "no-error clause is run outside the handlers for the given case"))

(t/deftest test-ignore-errors
  (t/is (nil? (sut/ignore-errors))
        "ignoring errors returns nil with no body")
  (t/is (= :good (sut/ignore-errors :good))
        "the body value is returned")
  (t/is (= :good (sut/ignore-errors :bad :good))
        "the last form is returned, ignoring previous errors")
  (t/is (= [1 2 3 4 5]
           (sut/multiple-value-list (sut/ignore-errors (sut/values-list [1 2 3 4 5]))))
        "multiple values can be returned")
  (t/is (= ::sut/simple-error
           (second
            (sut/multiple-value-list
             (sut/ignore-errors (sut/error "hello")))))
        "additional values past the first return signaled errors")
  (t/is (= :good
           (handler-case
               (sut/ignore-errors (sut/signal "hello"))
             (::sut/condition [& args] :good)))
        "conditions can be raised past ignore-errors"))

(t/deftest test-invoke-debugger
  (t/is (= :good
           (block done
             (let [f (fn [c hook]
                       (return-from done
                                    (and (nil? sut/*debugger-hook*)
                                         (= (first c) ::blah)
                                         :good)))]
               (binding [sut/*debugger-hook* f]
                 (sut/invoke-debugger ::blah))
               :bad)))
        "the debugger hook is invoked"))

(t/deftest test-muffle-warning
  (t/is (= :good
           (restart-case (do (sut/muffle-warning)
                             :bad)
             (::sut/muffle-warning [] :good)))
        "invokes the correct restart"))