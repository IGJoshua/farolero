(ns farolero.core-test
  "Tests for the new primitives added by farolero.
  Many of these tests have been adapted from ANSI-TEST. See the root of the test
  directory for ANSI-TEST's license."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :as t]
   [farolero.core :as sut :refer [handler-bind handler-case restart-case
                                  with-simple-restart wrap-exceptions
                                  block return-from values tagbody go]]
   [net.cgrand.macrovich :as macros]))

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
  #_{:clj-kondo/ignore #?(:clj [] :cljs [:unresolved-symbol])}
  (macros/case :clj
    (with-redefs [sut/*debugger-hook* nil]
      (t/is (= 17
               (with-in-str "0\ny\n(vreset! farolero.core/*place* 17)\n"
                 (let [x (volatile! nil)]
                   (with-out-str
                     (sut/assert @x [x]))
                   @x)))
            "the continue restart allows you to set the value interactively from the debugger")))
  (with-redefs [sut/*debugger-hook* nil]
    (t/is (= 17
             (let [x (volatile! nil)]
               (handler-bind [::sut/interactive-assertion
                              (fn [_c [[x-place]]]
                                (vreset! x-place 17)
                                (sut/continue))]
                 (handler-bind [::sut/assertion-error
                                (fn [& _]
                                  (sut/invoke-restart-interactively ::sut/continue))]
                   (sut/assert @x [x])))
               @x))
          "invoking the continue restart interactively signals the interactive-assertion")))

(t/deftest test-block
  (t/is (nil? (block _foo))
        "empty body returns nil")
  (t/is (= :good (block _foo :good))
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
        "non-lexical names work too")
  (t/is (= :good
           (try
             (handler-case
                 (let [f (block foo
                           (fn []
                             (return-from foo)))]
                   (f))
               (::sut/control-error [_c & _args]
                 :good))
             (catch #?(:clj Throwable :cljs js/Object) _e
               :bad)))
        "return-from outside its calling context signals a control error")
  #?(:clj (t/is (= :good
                   (try
                     (handler-case
                         (let [f (block foo
                                   (bound-fn []
                                     (return-from foo)))]
                           (f))
                       (::sut/control-error [_c & _args]
                         :good))
                     (catch Throwable _e
                       :bad)))
                "return-from outside its calling context with bound-fn signals a control error")))

(t/deftest test-cerror
  (t/is (= :good
           (handler-case (do (sut/cerror "Keep going.") :bad)
             (::sut/simple-error [_c & _args] :good)))
        "signals a simple-error when called with a string")
  (t/is (= 10
           (handler-bind [::sut/simple-error (fn [c & _args] (sut/continue c))]
             (sut/cerror "Wooo")
             10))
        "binds a continue restart")
  (t/is (= :good
           (sut/block test-block
             (binding [sut/*debugger-hook* (fn [& _args] (sut/return-from test-block :good))]
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
             (::sut/type-error [& _args]
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
          (::sut/simple-error [_c fmt & _args]
            (= fmt "Error")))
        "signals a simple error with a format string")
  (t/is (handler-case (sut/error "Error %s" 10)
          (::sut/simple-error [_c fmt & args]
            (= "Error 10" (apply #?(:clj format :cljs goog.string/format) fmt args))))
        "passes additional format arguments as rest args")
  (macros/case :clj
    (with-redefs [sut/*debugger-hook* nil]
      (t/is (= :good
               (with-in-str "0\n"
                 (restart-case (do (with-out-str
                                     (sut/error "Some error"))
                                   :bad)
                   (::sut/continue [] :interactive (constantly nil) :good))))
            "invokes the debugger"))))

(t/deftest test-handler-bind
  (t/is (nil? (handler-bind []))
        "can take no arguments")
  (t/is (= :good
           (block foo
             (handler-bind [::sut/error (fn [& _args] (return-from foo :good))]
               (sut/error "an error")
               :bad)))
        "calls the handlers when they're signaled")
  (t/is (= :good
           (block foo
             (handler-bind [::sut/error (fn [& _args] (return-from foo :good))]
               (handler-bind [::sut/error (fn [& args] (apply sut/error args))
                              ::sut/error (fn [& _args] (return-from foo :bad))]
                 (sut/error "an error")
                 :bad))))
        "allows re-signaling errors further up the stack")
  (letfn [(handler-fn [& _args]
            (return-from :foo :good))]
    (t/is (= :good
             (block :foo
               (handler-bind [::sut/simple-error handler-fn]
                 (sut/error "an error")
                 :bad)))
          "allows binding handler functions made elsewhere"))
  (t/is (= :good
           (block done
             (handler-bind [::sut/error (fn [c & _args] c)
                            ::sut/simple-condition (fn [_c & _args] (return-from done :good))]
               (sut/error "an error")
               :bad)))
        "allows conditions to defer handling conditions to other handlers")
  (t/is (= :good
           (block done
             (handler-bind [::sut/error (fn [_c & _args] (return-from done :good))]
               (handler-bind [::sut/error (fn [c & _args] c)]
                 (sut/error "an error")
                 :bad))))
        "allows conditions to defer handling conditions up the stack")
  (t/is (= :good
           (handler-bind [::sut/error (fn [_c & _args] (return-from :done :good))]
             (block :done
               (sut/error "an error")
               :bad)))
        "calls the handlers further down the stack than the body")
  (t/is (= :good
           (block done
             (handler-bind [::something identity
                            ::sut/error (fn [_c & _args] (return-from done :good))]
               (sut/error "an error")
               :bad)))
        "unmatched handlers don't get called")
  (t/is (= :good
           (block done
             (handler-bind [#?(:clj RuntimeException
                               :cljs js/Error) (fn [_c & _args] (return-from done :good))]
               (sut/error (#?(:clj RuntimeException. :cljs js/Error.) "An error!"))
               :bad)))
        "exceptions are valid handler types")

  #?(:clj
     (t/testing "thread local handlers"
       (t/is (= :good
                (let [ret (atom :good)]
                  (handler-bind [::sut/condition [(fn [_c] (reset! ret :bad))
                                                  :thread-local true]]
                    @(future (sut/signal ::sut/condition)))
                  @ret))
             "thread local handlers aren't run from other threads")
       (t/is (= :good
                (let [ret (atom :bad)]
                  (handler-bind [::sut/condition (fn [_c] (reset! ret :good))]
                    @(future (sut/signal ::sut/condition)))
                  @ret))
             "non-thread-local handlers are run from other threads"))))

(t/deftest test-handler-case
  (t/is (= :good
           (handler-case (do (sut/error "an error")
                             :bad)
             (::sut/error [& _args] :good)))
        "immediately unwinds on a caught error being signaled")
  (t/is (= :good
           (handler-case (do (sut/error "an error")
                             :bad)
             (::sut/warning [& _args] :bad)
             (::sut/error [& _args] :good)))
        "calls the correct handler")
  (t/is (= :good
           (handler-case (do (sut/error "an error")
                             :bad)
             (::sut/error [& _args] :good)
             (::sut/error [& _args] :bad)))
        "calls the first handler first")
  (t/is (= :good
           (handler-case :good
             (::sut/error [& _args] :bad)))
        "returns the value from the expression when no error is signaled")
  (t/is (= :good
           (handler-case
               (handler-case (do (sut/error "an error")
                                 :bad)
                 (::blah [& _args] :bad))
             (::sut/error [& _args] :good)))
        "grabs handlers further up the stack for matches")
  (t/is (= :good
           (handler-case
               (handler-case (do (sut/error "foo")
                                 :bad)
                 (::sut/error [& args] (apply sut/error args)))
             (::sut/error [& _args] :good)))
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
                 (::sut/error [& _args] :bad)
                 (:no-error [_] (sut/error "foo")))
             (::sut/error [& _args] :good)))
        "no-error clause is run outside the handlers for the given case")
  (let [state (volatile! nil)]
    (handler-case
        (sut/error "foo")
      (::sut/error [& _args] (vswap! state conj :found-error))
      (:no-error [& _args] (vswap! state conj :no-error)))
    (t/is (= [:found-error] @state)
          "no-error clause is only run when there is no error"))
  (macros/case :clj
    (t/is (thrown? Exception
                   (macroexpand
                    `(handler-case
                         :no-error-twice
                       (:no-error [& _args] "first")
                       (:no-error [& _args] "second"))))
          "only one no-error clause is allowed"))

  #?(:clj
     (t/is (= :good
              (handler-case (do @(future (sut/signal ::sut/condition))
                                :good)
                (::sut/condition [_c] :bad)))
           "all handler-case handlers are thread-local"))
  (t/is (= :good
           ((fn [n]
              (if (pos? n)
                :good
                (handler-case
                    (sut/error ::foo)
                  (::foo [_c]
                    (recur (inc n))))))
            0))
        "the recur form is permitted from inside the handlers"))

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
             (::sut/condition [& _args] :good)))
        "conditions can be raised past ignore-errors"))

(t/deftest test-invoke-debugger
  (t/is (= :good
           (block done
             (let [f (fn [c _hook]
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

(t/deftest test-multiple-value-bind
  (t/is (= [:a :b]
           (sut/multiple-value-bind [[a b] (values :a :b)]
             [a b]))
        "multiple values are bound")
  (t/is (= [:a :b]
           (sut/multiple-value-list
            (sut/multiple-value-bind [_ nil]
              (values :a :b))))
        "multiple values can be returned from the body"))

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
        "the interactive function is used to provde the arglist to the restart when invoked interactively")
  #?(:clj
     (t/testing "thread local restarts"
       (t/is (= :good
                (let [ret (atom :good)]
                  (sut/restart-bind [::foo [(fn [] (reset! ret :bad))
                                            :thread-local true]]
                    @(future (when-let [foo (sut/find-restart ::foo)]
                               (sut/invoke-restart foo))))
                  @ret))
             "thread local restarts cannot be invoked from other threads")
       (t/is (= :good
                (let [ret (atom :bad)]
                  (sut/restart-bind [::foo (fn [] (reset! ret :good))]
                    @(future (sut/invoke-restart ::foo)))
                  @ret))
             "non-thread-local restarts can be invoked from other threads"))))

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
        "restarts can be invoked interactively")

  #?(:clj
     (t/is (= :good
              (let [ret (atom :good)]
                (restart-case (do @(future
                                     (when-let [foo (sut/find-restart ::foo)]
                                       (sut/invoke-restart foo)))
                                  @ret)
                  (::foo []
                    (reset! ret :bad)))))
           "all restart-case restarts are thread-local"))
  (t/is (= :good
           ((fn [n]
              (if (pos? n)
                :good
                (restart-case
                    (sut/invoke-restart ::foo)
                  (::foo [_c]
                    (recur (inc n))))))
            0))
        "the recur form is permitted from inside the restarts"))

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

(t/deftest test-tagbody
  (t/is (nil? (tagbody)) "Empty returns nil")
  (t/is (nil? (tagbody _a _b _c)) "Only tags returns nil")
  (let [state (atom [])]
    (tagbody
     (swap! state conj 1)
     (swap! state conj 2)
     (swap! state conj 3))
    (t/is (= '(1 2 3) @state) "No tags is still run"))
  (t/testing "if/if-not transforms to when/when-not with fall-through"
    (let [state (atom nil)]
      (tagbody
       (if true (go a) (reset! state true))
       a
       (if-not false (go b) (reset! state true))
       b)
      (t/is (nil? @state) "Neither else branch was chosen"))
    (let [state (atom nil)]
      (tagbody
       (if false (reset! state true) (go a))
       a
       (if-not true (reset! state true) (go b))
       b)
      (t/is (nil? @state) "Neither primary branch was chosen")))
  (t/testing "when/when-not without go isn't transformed"
    (let [state (atom [])]
      (tagbody
       (when true
         (swap! state conj :a)
         (swap! state conj :a2))
       (when-not false
         (swap! state conj :b)
         (swap! state conj :b2)))
      (t/is (= [:a :a2 :b :b2] @state))))
  (t/testing "when/when-not with go in non-final place isn't transformed"
    (let [state (atom [])]
      (tagbody
       (when true
         (swap! state conj :a)
         (go a)
         (swap! state conj :a2))
       a
       (when-not false
         (swap! state conj :b)
         (go b)
         (swap! state conj :b2))
       b)
      (t/is (= [:a :b] @state))))
  (t/testing "Nested tagbodies with shadowed tags"
    (let [state (atom [])]
      (tagbody
       (swap! state conj :entering-outer)
       _a
       (tagbody
        (swap! state conj :entering-inner)
        (go a)
        a
        (swap! state conj :exiting-inner))
       (swap! state conj :exiting-outer))
      (t/is (= [:entering-outer
                :entering-inner
                :exiting-inner
                :exiting-outer]
               @state))))
  (t/testing "Nested tagbodies jumps to outer tags"
    (let [state (atom [])]
      (tagbody
       (swap! state conj :entering-outer)
       _a
       (tagbody
        _a ;; Needed to not short-circuit to tagless branch
        (swap! state conj :entering-inner)
        (go b)
        (swap! state conj :exiting-inner))
       b
       (swap! state conj :exiting-outer))
      (t/is (= [:entering-outer
                :entering-inner
                :exiting-outer]
               @state))))
  (t/testing "CLHS examples"
    (let [state (atom nil)]
      (tagbody
       (reset! state 1)
       (go point-a)
       (swap! state + 16)
       point-c
       (swap! state + 4)
       (go point-b)
       (swap! state + 32)
       point-a
       (swap! state + 2)
       (go point-c)
       (swap! state + 64)
       point-b
       (swap! state + 8))
      (t/is (= @state 15)))
    (t/testing "go can be called in another function"
      (let [f1 (fn f1 [flag escape]
                 (if flag (escape) 2))
            f2 (fn f2 [flag]
                 (let [n (atom 1)]
                   (tagbody
                    (reset! n (f1 flag #(go out)))
                    out
                    (print @n))))]
        (t/is (= "2"
                 (with-out-str
                   (f2 nil))))
        (t/is (= "1"
                 (with-out-str
                   (f2 true)))))))
  (t/testing "Convoluted example"
    (let [state (atom {:a 0 :b 0})]
      (t/is (= ":a:c:a34 positively done"
               (with-out-str
                 (tagbody
                  (go a)
                  (print 1)
                  a
                  (print :a)
                  (if (pos? (:a @state))
                    (go b)
                    (do (swap! state update :a inc)
                        (go c)))
                  b
                  (when (swap! state update :b + 10)
                    (print 3)
                    (print 4)
                    (go d))
                  c
                  (print :c)
                  (swap! state update :b inc)
                  (go a)
                  d
                  (if (pos? (:b @state))
                    (print " positively done")
                    (print " negativey done"))))))
      (t/is (= {:a 1 :b 11} @state))))
  (t/testing "tagbody is reentrant"
    (letfn [(testfn [depth fun]
              (block return
                (tagbody
                 (if (zero? depth)
                   (return-from return
                     (testfn (inc depth) #(go exit)))
                   (fun))
                 exit
                 (return-from return depth))))]
      (t/is (zero? (testfn 0 identity))
            "The function goes to the lexical tag, not the dynamic one"))))

(t/deftest test-warn
  (t/is (let [warned? (volatile! false)]
          (handler-bind [::sut/warning (fn [_c & _args]
                                         (vreset! warned? true)
                                         (sut/muffle-warning))]
            (sut/warn "this is a warning")
            @warned?))
        "the warning handler is called")
  (t/is (= ""
           (with-out-str
             (binding [#?@(:clj (*err* *out*)
                           :cljs (*print-err-fn* *print-fn*))]
               (handler-bind [::sut/warning (fn [_c & _args]
                                              (sut/muffle-warning))]
                 (sut/warn "this is a warning")))))
        "when muffling the warning, no output is produced")
  (t/is (not= ""
              (with-out-str
                (binding [#?@(:clj (*err* *out*)
                              :cljs (*print-err-fn* *print-fn*))]
                  (handler-bind [::sut/warning (fn [_c & _args]
                                                 nil)]
                    (sut/warn "this is a warning")))))
        "when not muffling the warning, the output is not empty")
  (t/is (let [warned? (volatile! false)]
          (handler-bind [::sut/simple-condition
                         (fn [_c & _args]
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
                           (throw (#?(:clj RuntimeException.
                                      :cljs js/Error.) "an error")))
             (#?(:clj Exception
                 :cljs js/Error) [_c] :good)))
        "thrown exceptions get raised as errors")
  (t/is (= :good
           (handler-bind [#?(:clj Exception :cljs js/Error) (fn [_c] (sut/use-value :good))]
             (wrap-exceptions
               (throw (#?(:clj RuntimeException. :cljs js/Error.) "an error")))))
        "the use-value restart is bound inside wrap-exceptions")
  (t/is (= :good
           (let [called? (volatile! false)
                 f #(if @called? :good (throw (#?(:clj RuntimeException. :cljs js/Error.) "an error")))]
             (handler-bind [#?(:clj Exception :cljs js/Error)
                            (fn [_c]
                              (vreset! called? true)
                              (sut/continue))]
               (wrap-exceptions
                 (f)))))
        "the continue restart will retry calling the body"))

(macros/case :cljs
  (t/run-tests))
