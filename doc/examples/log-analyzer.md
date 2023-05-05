This project is a basic introduction to farolero, showing how to use the macros,
what the development process feels like, and how conditions and restarts reduce
the amount of code that needs to be written.


# Motivation & Project Setup

Log files often need to be inspected and analyzed to gain insights about
programs, how they fail, how often certain events occur, etc. This program will
read log files from a directory, parse them using a standard format, and
aggregate statistics about them.

To get started, we need a deps.edn file to start our project.

```clojure
{:paths ["src"]
 :deps {org.clojure/clojure {:mvn/version "1.11.1"}
        org.suskalo/farolero {:mvn/version "1.5.0"}}}
```

All this does is bring in Clojure and farolero.

The project will be split up into two components, the log parser, and the
analyzer. At the end of the project we'll be able to parse logs in multiple
formats and gather statistics about the frequency of certain types of messages.


# Parsing Logs

In order to parse a log entry, we first need to know what the entry is going to
look like. A basic format like the one that follows is what we'll be parsing for
now.

    2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text

This format consists of a date, a thread name between square brackets, the
logging level, the namespace the message was logged from, and the message.

To start with we'll require the `farolero.core` namespace and refer the macros
which will be used the most, along with a few other namespaces.

```clojure
(ns log-analyzer.parse
  (:require
   [clojure.string :as str]
   [farolero.core :as far :refer [restart-case handler-case handler-bind]])
  (:import
   (java.time LocalDateTime)
   (java.time.format DateTimeFormatter)))
```

Now that we have a namespace defined, we can make a regex which will match it.

```clojure
(def log-line-regex #"^(\S+) +\[(.*?)\] +(\S+) +(\S+) +- +(.*)$")
```

This regex looks a little complex, but it's quite simple in what it does. It
just checks for a sequence of non-whitespace text to match the date, some text
inside square brackets for the thread name, and so on.

With this regex, we can now make a very basic log parsing function.

```clojure
(defn parse-log-line
  [s]
  (when-let [[date thread level ns message] (next (re-matches log-line-regex s))]
    (try
      {:date (LocalDateTime/parse date DateTimeFormatter/ISO_DATE_TIME)
       :thread thread
       :level (keyword (str/lower-case level))
       :ns (symbol ns)
       :message message}
      (catch Exception _))))
```

This function parses an individual log line into a map.

```clojure
(parse-log-line "2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text")
;; => {:date #object(java.time.LocalDateTime 0x3c051b9c "2021-05-21T21:46:30.321411")
;;     :thread "main"
;;     :level :debug
;;     :ns some.namespace
;;     :message "some text"}
(parse-log-line "Some random invalid log line")
;; => nil
```

There is one problem with this implementation however: if it encounters a log
line that doesn't fit the regex or with an invalid date format, it always
returns `nil`, regardless of what the problem was. Initially this doesn't seem
like a problem, and it's how a lot of code is written, but this means that in
circumstances where you would like to know about the errors, and potentially do
something about them, you have no option but to write another function, or
change callers of this function to handle errors.

In cases where there are few functions and a shallow stack, this may not be a
large burden, but if it was buried many layers deep behind many functions you
don't want to rewrite, this may prove a significant burden.

Instead, let us define these functions like this:

```clojure
(defn parse-log-line
  [s]
  (restart-case
      (if-let [[date thread level ns message] (next (re-matches log-line-regex s))]
        (try
          {:date (LocalDateTime/parse date DateTimeFormatter/ISO_DATE_TIME)
           :thread thread
           :level (keyword (str/lower-case level))
           :ns (symbol ns)
           :message message}
          (catch Exception e
            (far/error ::invalid-log-entry
                       :log-line s
                       :cause e)))
        (far/error ::invalid-log-entry
                   :log-line s
                   :cause ::failed-regex-match))
    (::far/continue [])
    (::far/use-value [v] v)))

(defn parse-log-file
  [lines]
  (keep parse-log-line lines))
```

This change gives us a lot of power. Let's walk through this. Instead of using a
`when-let` and simply returning `nil` when a log line fails to match, and
catching exceptions and returning `nil` on date formatting parsers, we call
`farolero.core/error` and pass it the keyword `::invalid-log-entry`. You can
think of this almost like throwing an exception of a custom type. It will look
up the stack to try to find something that "catches" (or in farolero parlance,
"handles") the error.

```clojure
(parse-log-line "2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text")
;; => {:date #object[java.time.LocalDateTime 0x7b79b9c "2021-05-21T21:46:30.321411"],
;;     :thread "main",
;;     :level :debug,
;;     :ns some.namespace,
;;     :message "some text"}

(parse-log-line "Some random invalid log line")
;; => #clojure.lang.ExceptionInfo {:message "Unhandled Condition",
;;                                 :data {:condition ::invalid-log-entry,
;;                                        :args '(:log-line "Some random invalid log line"
;;                                                :cause :log-analyzer.parse/failed-regex-match)}}
```

If we wanted to catch the condition, we can use `handler-case`.

```clojure
(handler-case (parse-log-line "Some random invalid log line")
  (::invalid-log-entry [_ & {:keys [log-line]}]
    log-line))
;; => "Some random invalid log line"
```

Where this differs from exceptions however is in what you can do when you
respond to the error. In this case, the call is wrapped in a `restart-case`,
binding two restarts, `::far/continue`, and `::far/use-value`. If we use
`handler-bind`, we can register a handler that will invoke one of those restarts.

For example, if we use the `::far/continue` restart, which we can invoke with
`farolero.core/continue`, it will return nil from parse-log-line, which will in
turn be removed by `keep`.

```clojure
(handler-bind [::invalid-log-entry (fn [_ & {:keys [log-line]}]
                                     (far/continue))]
  (parse-log-file
   ["Some random invalid log line"
    "2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text"]))
;; => (nil
;;     {:date #object(java.time.LocalDateTime 0x27fb80cf "2021-05-21T21:46:30.321411")
;;      :thread "main"
;;      :level :debug
;;      :ns some.namespace
;;      :message "some text"})
```

If we invoke the `::far/use-value` restart, we can substitute our own value for
the log entry.

```clojure
(handler-bind [::invalid-log-entry (fn [_ & _]
                                     (far/use-value :invalid-entry))]
  (parse-log-line "Some random invalid log line"))
;; => :invalid-entry
```

One of the uses this could have is to allow multiple log formats to be parsed.
In the old implementation, if we wanted to support multiple log formats, we'd
have to change the regex, and the construction of the map, basically requiring
an entirely new function. With this way of handling logs, we can extend the
existing behavior to add new valid log entries. This may be useful if for
example we have our application's process sending its logs to stdout, the same
place where a process monitor is also sinking its logs.

```clojure
(def alternate-log-format #"\[(\S+)\] (\S+) - (.*)")

(defn alt-middleware
  [f]
  (fn [& args]
    (handler-bind [::invalid-log-entry
                   (fn [_ & {:keys [log-line]}]
                     (when-let [[level ns message] (next (re-matches alternate-log-format log-line))]
                       (far/use-value {:level (keyword (str/lower-case level))
                                       :ns (symbol ns)
                                       :message message})))]
      (apply f args))))

(handler-bind [::invalid-log-entry (fn [_ & _] (far/continue))]
  ((alt-middleware (comp doall parse-log-file))
   ["Some random invalid log line"
    "2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text"
    "[DEBUG] other.namespace - More text"]))
;; => ({:date #object[java.time.LocalDateTime 0xc5cee50 "2021-05-21T21:46:30.321411"],
;;      :thread "main",
;;      :level :debug,
;;      :ns some.namespace,
;;      :message "some text"}
;;     {:level :debug,
;;      :ns other.namespace,
;;      :message "More text"})
```

This `alt-middleware` function takes the function it wants to wrap, in this case
an eager-ized `parse-log-file`, and then returns a function that calls it in the
context of a handler for `::invalid-log-entry` that will attempt to parse it
using a second log format. Notice however that if it fails to parse the log
line, it simply returns normally instead of invoking a restart. This is
intentional. If a handler returns normally without invoking a restart, then it
has decided not to handle the condition, and handlers further up the stack will
be used, in this case the one which calls the `far/continue` restart.

As a footnote here, farolero includes a macro for converting exceptions to
conditions, `wrap-exceptions`, which catches all exceptions and signals them as
conditions:

```clojure
(handler-case (far/wrap-exceptions
                (throw (ex-info "An error!" {})))
  (Exception [e]
    (ex-message e)))
;; => #clojure.lang.ExceptionInfo {:message "An error!",
;;                                 :data {}}
```


# Aggregating Statistics

Now that we can turn lines of strings into log structures, we should add ways to
generate statistics about the logs we parse. First let's declare a new namespace
to work in.

```clojure
(ns log-analyzer.stats
  (:require
    [clojure.string :as str]
    [farolero.core :as far :refer [restart-bind restart-case handler-bind]]))
```

Say we want to generate a basic statistic about how many errors occurred. We
might write a function like this:

```clojure
(defn count-errors
  [logs]
  (count (filter (comp #{:error} :level) logs)))
```

This function does exactly what we want, but it poses a problem. It's entirely
specific to exactly the one statistic we're generating, and there's no way to
extend it to be more general.

If we think about this more carefully, then we have two components for each
statistic. The first is one that generates data points from the sequence of
logs, and the second takes those data points and aggregates them into a
structure to be returned.

There's two main ways we could go about making a statistics abstraction which
would cover these needs. The first is the functional way, where we design higher
order functions to allow both the generation of data points and to aggregate the
data points into a structure. The second way is to use handlers to generate data
points, and restarts to aggregate them. We will investigate both to see their
respective advantages and disadvantages.

First, let's do this in a functional way. To build a count errors statistic, we
need a way to generate data points, and a way to collect them. This is already
encoded in Clojure in the form of transducers, so let's write some code that
takes advantage of this.

We'll write a transducing context which stores each transducer stack's value in
a key in an accumulator map. First it calls the reducing functions with no
arguments to generate an initial value for the statistic and stores them in a
map, next it calls each reducing function with each log entry, using the value
generated in the first step as the accumulator, and finally is completes all the
statistics by calling each reducing function with its accumulator and no log
entries.

```clojure
(defn aggregate-statistics
  [rfs logs]
  (let [init-stats (reduce-kv (fn [m k rf]
                                (assoc m k (rf)))
                              {} rfs)
        stats (reduce
               (fn [stats log-entry]
                 (reduce-kv (fn [m k rf]
                             (update m k rf log-entry))
                           stats rfs))
               init-stats
               logs)]
    (reduce-kv (fn [m k rf] (update m k rf)) stats rfs)))
```

This produces a function which is relatively usable, and to get back our error
count metric, we could call it like so:

```clojure
(aggregate-statistics
 {:error-count ((filter (comp #{:error} :level))
                (fn
                  ([] 0)
                  ([acc] acc)
                  ([acc next] (inc acc))))}
 [{:level :info
   :message "Hello"}
  {:level :error
    :message "Bad!"}
  {:level :error
    :message "Crash"}])
;; => #:log-analyzer.stats{:error-count 2}
```

This isn't a bad solution, but it has problems once we try to extend this
behavior to something more complex.

Suppose for example that we wish to generate a single statistic, but where there
are two distinct ways to generate a data point for that statistic. Maybe our
error count metric needs to be triggered by both the level of the log message,
but also if the word "error" occurs in the message, regardless of the logging
level.

In this case, we have two options. On the one hand, we could generate two
statistics and combine them after they're both constructed. For certain types of
statistics this may be feasible, but if you need total ordering of data points,
it won't work for you. On the other hand, we could rewrite the aggregate
statistics function to not deal with a map, but instead have a sequence of
reducing functions that all operate on the same data structure. This however has
the problem that two statistics that update the same value become coupled
because they require that they both update the same place in the data.

With some understanding of how we might construct this with pure functions,
let's now consider how we might decouple construction of data points from
aggregation of statistics using farolero.

To begin with, we'll write a function which will be used to iterate through the
log structure and serve up the entries, as well as collect the results of the
statistics.

```clojure
(defn aggregate-statistics
  [logs]
  (restart-case
      (do (run! #(far/signal ::log-entry %) logs)
          (far/signal ::collect-statistics {})
          {})
    (::far/use-value [v] v)))
```

That's it. Let's walk through this line by line to see what this will be used
for.

To start out, this function only takes the log entries, it doesn't take anything
to aggregate statistics with. Instead of taking a function or similar, we let
the dynamic context that this function is called in determine what statistics
will be collected.

Next, we have a restart case with a do expression, and one restart bound, called
`:farolero.core/use-value` that just returns the value it's passed. This is how
we'll return statistics, as you'll see in a minute.

Next we call `run!` to signal each log entry. Any handlers for `::log-entry` up
on the stack will see this log entry, and can optionally emit a data point for
the statistic they're generating for.

After that, we signal `::collect-statistics` with an empty map. Handlers for
this condition will add the statistic they're collecting into the map and
re-signal it, and eventually invoke the `::far/use-value` restart to return the
statistics.

If none of the handlers invoke the `::far/use-value` restart though, then
\`far/signal\` will return nil, but we'd prefer to return an empty map.

Since we'll be defining things in this way, let's also define a utility function
for outputting data points.

```clojure
(defn signal-data-point
  [name & args]
  (when-let [r (far/find-restart name)]
    (apply far/invoke-restart r args))
  nil)
```

All this does is invoke a restart with the arguments only if it exists. This
way, we can create pieces of code that generate several different kinds of data
points, and nothing will go wrong if only one of those kinds of data points is
being consumed.

Let's see how to use this to create our error count statistic.

Like how we added a new format to the log parser, we'll define the statistics in
terms of middleware. To start with, we'll make a middleware which will call the
statistics function in the context of a restart which will add one to our error
count, a locally bound atom, as well as a handler for the `::collect-statistics`
condition, which will add the count to the statistics map.

```clojure
(def with-error-count
  (fn [f]
    (fn [& args]
      (let [ct (atom 0)]
        (restart-bind [::add-error (fn [] (swap! ct inc))]
          (handler-bind [::collect-statistics
                         (fn [_ acc]
                           (let [acc (assoc acc ::error-count @ct)]
                             (far/signal ::collect-statistics acc)
                             (far/use-value acc)))]
            (apply f args)))))))
```

The handler for collecting statistics will first re-signal the updated value
just in case another statistic is higher on the stack which wants to add its own
value. If there isn't though, then `signal` will return, and we invoke the
`use-value` restart with our updated map.

Next, we'll make a middleware that will generate an error data point if a
predicate returns true when called with a log entry.

```clojure
(defn error-on
  [pred]
  (fn [f]
    (fn [& args]
      (handler-bind [::log-entry
                     (fn [_ entry]
                       (when (pred entry)
                         (signal-data-point ::add-error)))]
        (apply f args)))))
```

This handles the `::log-entry` condition, adding data points by using the
`signal-data-point` call, which then returns normally, allowing additional
handlers higher up the stack to also react to the log entry.

Now let's compose our middleware into a complete `error-count` middleware.

```clojure
(def error-count
  (comp with-error-count
        (error-on (comp #{:error} :level))
        (error-on (comp (partial str/index-of "error") str/lower-case :message))))
```

This will add the error count statistic to the collected map, and it will
generate error data points either when there is an `:error` level log entry, or
a log entry with the text `error` somewhere in the message, case-insensitively.

Now we can use this middleware together with the statistics function to gather
our stats.

```clojure
((error-count aggregate-statistics)
 [{:level :info
   :message "error"}
  {:level :info
   :message "hello"}
  {:level :error
   :message "Bad!"}
  {:level :error
   :message "Crash"}])
;; => #:log-analyzer.stats{:error-count 3}
```

Note however that because of how we defined our middleware, we don't have to
apply it directly to `aggregate-statistics`. We could put this anywhere higher
up on the stack surrounding a function which directly or indirectly calls
`aggregate-statistics`.

Now let's consider the advantages and disadvantages of this approach.

On the positive side, it is very easy to extend, with multiple data-point
generators per statistic, as many statistics as desired, and with the ability to
inject them at any point in the stack. Additionally, because the `::log-entry`
handlers and the restarts used to aggregate the data all operate on data in a
thread-safe manner, we could update our `aggregate-statistics` function to use a
thread pool instead of `run!` to generate all the statistics, and then signal
the `::collect-statistics` back on the original thread when it's complete
(although if we were to do this, the `::collect-statistics` handlers should be
marked as thread-local, see `handler-bind`'s docstring for details).

On the negative side, the extension method makes no requirements about where on
the stack statistics collectors may be bound. This means that without
discipline, bindings may occur over many parts of the stack, making it hard to
keep track of where to add additional handlers, or find the ones that have
already been added. While this usually isn't a problem for error handling, it
can sometimes become a problem with other kinds of situations, like this one.
This same kind of flexibility can prove useful though, for example by allowing
the conditional adding of a statistic being tracked from an entrypoint, long
before the handlers are normally bound.


# Conclusion

At this stage of the project we have everything that's needed to parse a log
file and report any desired analytics about it. All that's left is to design an
interface to it, as well as to add extra statistics and formats. This is left as
an exercise to the reader.

Conditions provide a powerful method to extend the toolbox for error handling.
In any case where multiple ways to solve a problem present themselves that fit
with a common interface, bind restarts to provide ways to do them, and signal a
condition to allow that decision to be deferred. Handlers are bound higher up
the stack allow choices to be made about how these actions are taken.
Additionally restarts bound with \`restart-bind\` provide a system to signal side
effects without affecting the current execution path. All of these together
provide a flexible system for changing how the code runs based on its context.

