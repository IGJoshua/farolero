(ns farolero.core-test
  "Tests for the new primitives added by farolero.
  Many of these tests have been adapted from ANSI-TEST. See the root of the test
  directory for ANSI-TEST's license."
  (:require
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.test :as t]
   [farolero.core :as sut :refer [handler-bind handler-case restart-case
                                  with-simple-restart wrap-exceptions
                                  block return-from values]])
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

(t/deftest test-block
  (t/is (nil? (block foo))
        "empty body returns nil")
  (t/is (= :good (block foo :good))
        "returns the body value")
  (t/is (= :good
           (block foo
             (return-from foo :good)
             :bad))
        "return-from causes early return")
  (t/is (= :good
           (block :foo
             (return-from :foo :good)
             :bad))
        "non-lexical names work too"))

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

(t/deftest test-restart-bind
  (t/is (nil? (sut/restart-bind []))
        "returns nil when no body is provided")
  (t/is (= :good
           (block done
             (sut/restart-bind []
               (return-from done :good)
               :bad)))
        "doesn't block returning")
  (t/is (= :good
           (block done
             (sut/restart-bind [::foo (fn []
                                        (return-from done :good))]
               (sut/invoke-restart ::foo)
               :bad)))
        "restarts are invoked")
  (t/is (= :good
           (block done
             (sut/restart-bind [::foo (fn []
                                        (return-from done :good))]
               (let [r (sut/find-restart ::foo)]
                 (sut/invoke-restart r))
               :bad)))
        "works with the values returned from find-restart")
  (t/is (= [3 2 1]
           (block done
             (sut/restart-bind [::foo (fn [& args]
                                       (return-from done (reverse args)))]
               (sut/invoke-restart ::foo 1 2 3))))
        "arguments passed to invoke-restart are passed to the restart function")
  (t/is (= [3 2 1]
           (sut/restart-bind [::foo (fn [& args] (reverse args))]
             (sut/invoke-restart ::foo 1 2 3)))
        "the value returned from the restart is returned from invoke-restart")
  (letfn [(f [] (sut/invoke-restart ::foo 1 2 3))]
    (t/is (= [3 2 1]
             (sut/restart-bind [::foo (fn [& args] (reverse args))]
               (f)))
          "the restart doesn't need to be invoked from the lexical scope it's bound in"))
  (t/is (= :good
           (sut/restart-bind [::foo (constantly :bad)]
             (sut/restart-bind [::foo (constantly :good)]
               (sut/invoke-restart ::foo))))
        "only the innermost restart is called")
  (t/is (= [:a :a :a :a :a :a :a :a :a :a]
           (let [x (volatile! 10)
                 y (volatile! '())]
             (sut/restart-bind
                 [::foo (fn []
                          (when (> @x 0)
                            (vswap! y conj :a)
                            (vswap! x dec)
                            (sut/invoke-restart ::foo))
                          @y)]
               (sut/invoke-restart ::foo))))
        "restarts can invoke themselves")
  (t/is (= 1
           (block done
             (let [i (volatile! 0)]
               (sut/restart-bind [::foo (do (vswap! i inc)
                                            (fn [] (return-from done @i)))]
                 (sut/invoke-restart ::foo)
                 :bad))))
        "arbitrary code can be run in the value of the binding")
  (t/is (= "A report"
           (sut/restart-bind [::foo [(constantly nil) :report-function "A report"]]
             (sut/report-restart (sut/find-restart ::foo))))
        "restarts bound can be reported")
  (t/is (= :good
           (sut/restart-bind [::foo (constantly :good)
                              ::foo (constantly :bad)]
             (sut/invoke-restart ::foo)))
        "only the first restart is invoked")
  (t/is (= :good
           (sut/restart-bind [::foo (constantly :bad)
                              ::bar (constantly :good)]
             (sut/invoke-restart ::bar)))
        "the correct restart is invoked")
  (t/is (= :good
           (sut/restart-bind [::foo [(constantly :bad) :test-function (constantly nil)]
                              ::foo [(constantly :good) :test-function (constantly true)]]
             (sut/invoke-restart ::foo)))
        "the test function is used to determine which restarts can be invoked")
  (t/is (= :good
           (sut/restart-bind [::foo [identity :interactive-function (constantly [:good])]]
             (sut/invoke-restart-interactively ::foo)))
        "the interactive function is used to provde the arglist to the restart when invoked interactively"))

(t/deftest test-restart-case
  (t/is (nil? (restart-case nil))
        "without restart clauses, the value is always returned")
  (t/is (= :good
           (restart-case (do (sut/invoke-restart ::foo) :bad)
             (::foo [] :good)))
        "the value from the restart is used as the return value when invoked")
  (t/is (nil? (restart-case (do (sut/invoke-restart ::foo) :bad)
                (::foo [])))
        "the value from the restart, even nil, is returned from the full expression")
  (t/is (= :good
           (restart-case (do (sut/invoke-restart ::foo) :bad)
             (::bar [] :bad)
             (::foo [] :good)
             (::foo [] :bad)))
        "the first restart with the correct name is invoked")
  (t/is (= [3 2 1]
           (restart-case (sut/invoke-restart ::foo 1 2 3)
             (::foo [& args] (reverse args))))
        "additional arguments are passed to the restart function")
  (t/is (= 6
           (restart-case
               (restart-case (sut/invoke-restart ::foo 1)
                 (::foo [v] (sut/invoke-restart ::foo (inc v))))
             (::foo [v] (+ v 4))))
        "restarts can invoke outer restarts")
  (t/is (= :good
           (restart-case (sut/invoke-restart ::foo)
             (::foo [] :test (constantly false)
               :bad)
             (::foo []
               :good)))
        "restarts can have test functions")
  (t/is (= :good
           (restart-case (do (sut/invoke-restart (first (sut/compute-restarts))) :bad)
             (nil [] :good)))
        "unnamed restarts can be bound and invoked")
  (t/is (= :good
           (restart-case (do (sut/invoke-restart-interactively ::foo)
                             :bad)
             (::foo [] :interactive (constantly nil)
               :good)))
        "restarts can be invoked interactively"))

(t/deftest test-store-value
  (t/is (= :good
           (restart-case (do (sut/store-value :good) :bad)
             (::sut/store-value [v] v)))
        "the passed value is the argument to the restart"))

(t/deftest test-use-value
  (t/is (= :good
           (restart-case (do (sut/use-value :good) :bad)
             (::sut/use-value [v] v)))
        "the passed value is the argument to the restart"))

(t/deftest test-warn
  (t/is (let [warned? (volatile! false)]
          (handler-bind [::sut/warning (fn [c & args]
                                         (vreset! warned? true)
                                         (sut/muffle-warning))]
            (sut/warn "this is a warning")
            @warned?))
        "the warning handler is called")
  (t/is (= ""
           (with-out-str
             (binding [*err* *out*]
               (handler-bind [::sut/warning (fn [c & args]
                                              (sut/muffle-warning))]
                 (sut/warn "this is a warning")))))
        "when muffling the warning, no output is produced")
  (t/is (not= ""
              (with-out-str
                (binding [*err* *out*]
                  (handler-bind [::sut/warning (fn [c & args]
                                                 nil)]
                    (sut/warn "this is a warning")))))
        "when not muffling the warning, the output is not empty")
  (t/is (let [warned? (volatile! false)]
          (handler-bind [::sut/simple-condition
                         (fn [c & args]
                           (vreset! warned? true)
                           (sut/muffle-warning))]
            (sut/warn "this is a warning")
            @warned?))
        "warnings made from strings derive simple-condition"))

(t/deftest test-with-simple-restart
  (t/is (nil? (with-simple-restart (::foo "")))
        "empty body returns nil")
  (t/is (= [nil true]
           (sut/multiple-value-list
            (with-simple-restart (::foo "")
              (sut/invoke-restart ::foo))))
        "the second value is set to true when it restarts")
  (t/is (= [nil true]
           (sut/multiple-value-list
            (with-simple-restart (nil "")
              (sut/invoke-restart (first (sut/compute-restarts))))))
        "unnamed restarts can be bound"))

(t/deftest test-wrap-exceptions
  (t/is (nil? (wrap-exceptions))
        "empty body returns nil")
  (t/is (= :good
           (wrap-exceptions :good))
        "the last body value is returned")
  (t/is (= :good
           (handler-case (wrap-exceptions
                           (throw (RuntimeException. "an error")))
             (Exception [c] :good)))
        "thrown exceptions get raised as errors")
  (t/is (= :good
           (handler-bind [Exception (fn [c] (sut/use-value :good))]
             (wrap-exceptions
               (throw (RuntimeException. "an error")))))
        "the use-value restart is bound inside wrap-exceptions")
  (t/is (= :good
           (let [called? (volatile! false)
                 f #(if @called? :good (throw (RuntimeException. "an error")))]
             (handler-bind [Exception (fn [c]
                                        (vreset! called? true)
                                        (sut/continue))]
               (wrap-exceptions
                 (f)))))
        "the continue restart will retry calling the body"))
