This project is a basic introduction to farolero, showing how to use the macros,
what the development process feels like, and how conditions and restarts reduce
the amount of code that needs to be written.


# Project Setup

Log files often need to be inspected and analyzed to gain insights about
programs, how they fail, how often certain events occur, etc. This program will
read log files from a directory, parse them using a standard format, and
aggregate statistics about them.

To get started, we need a deps.edn file to start our project.

```clojure
{:paths ["src"]
 :deps {org.clojure/clojure {:mvn/version "1.10.3"}
        org.suskalo/farolero {:mvn/version "1.1.1"}}}
```

All this does is bring in Clojure and farolero.

The project will be split up into three components, the log parser, the
analyzer, and the cli. At the end of the project we'll be able to parse logs in
multiple formats from multiple files and gather statistics about the frequency
of certain types of messages, as well as report errors to the user.


# Parsing Logs

In order to parse a log entry, we first need to know what the entry is going to
look like. A basic format like the one that follows is what we'll be parsing for
now.

`2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text`

This format consists of a date, a thread name between square brackets, the
logging level, the namespace the message was logged from, and the message.

To start with we'll require the `farolero.core` namespace and refer the macros
which will be used the most, along with a few other namespaces.

```clojure
(ns log-analyzer.parse
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [farolero.core :as far :refer [handler-bind handler-case restart-case
                                  wrap-exceptions translate-exceptions values]])
  (:import
   (java.time format.DateTimeFormatter LocalDateTime)))
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
    {:date (LocalDateTime/parse date DateTimeFormatter/ISO_DATE_TIME)
     :thread thread
     :level (keyword (str/lower-case level))
     :ns (symbol ns)
     :message message}))

(defn parse-log-file
  [lines]
  (keep parse-log-line lines))
```

This function parses an individual log line into a map, and parses a file into a
sequence of these maps.

```clojure
(parse-log-line "2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text")
(parse-log-line "Some random invalid log line")
```

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">{:date #object[java.time.LocalDateTime 0x26b97e8c "2021-05-21T21:46:30.321411"], :thread "main", :level :debug, :ns some.namespace, :message "some text"}</td>
</tr>


<tr>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>

There is one problem with this implementation however: if it encounters a log
line that doesn't fit the regex, it always returns `nil`, and `parse-log-file`
always removes it. Initially this doesn't seem like a problem, and it's likely
how you've written a lot of code before, but this means that in circumstances
where you would like to know about the errors, and potentially do something
about them, you have no option but to write another function.

In cases like this where there are few functions and a shallow stack, this may
not be a large burden, but if it was buried many layers deep behind many
functions you don't want to rewrite, this may prove a significant burden.

Instead, let us define these functions like this:

```clojure
(defn parse-log-line
  [s]
  (restart-case
      (if-let [[date thread level ns message] (next (re-matches log-line-regex s))]
        {:date (LocalDateTime/parse date DateTimeFormatter/ISO_DATE_TIME)
         :thread thread
         :level (keyword (str/lower-case level))
         :ns (symbol ns)
         :message message}
        (far/error ::invalid-log-entry
                   :log-line s))
    (::far/continue [])
    (::far/use-value [v] v)))

(defn parse-log-file
  [lines]
  (keep parse-log-line lines))
```

This change gives us a lot of power. Let's walk through this. Instead of using a
`when-let` and simply returning `nil` when a log line fails to match, we call
`farolero.core/error` and pass it the keyword `::invalid-log-entry`. You can
think of this almost like throwing an exception of a custom type. It will look
up the stack to try to find something that "catches" (or in farolero parlance,
"handles") the error.

```clojure
(parse-log-line "2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text")
(parse-log-line "Some random invalid log line")
```

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">{:date #object[java.time.LocalDateTime 0xd57ad53 "2021-05-21T21:46:30.321411"], :thread "main", :level :debug, :ns some.namespace, :message "some text"}</td>
</tr>


<tr>
<td class="org-left">#clojure.lang.ExceptionInfo {:message "Unhandled Condition", :data {:condition ::invalid-log-entry, :args '(:log-line "Some random invalid log line")}}</td>
</tr>
</tbody>
</table>

If we wanted to catch the condition, we can use `handler-case`.

```clojure
(handler-case (parse-log-line "Some random invalid log line")
  (::invalid-log-entry [_ & {:keys [log-line]}]
    log-line))
```

    "Some random invalid log line"

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
  (doall
   (parse-log-file ["Some random invalid log line"
                    "2021-05-21T21:46:30.321411Z [main] DEBUG some.namespace - some text"])))
```

    '({:date #object(java.time.LocalDateTime 0x77a64bc8 "2021-05-21T21:46:30.321411")  :thread "main"  :level :debug  :ns some.namespace  :message "some text"})

Note that in this case we must use `doall` because `parse-log-file` returns a
lazy sequence, and if the elements of the sequence are realized outside the
context of the `handler-bind` (for example when printing at the repl), the
condition will be unhandled.

If we invoke the `::far/use-value` restart, we can substitute our own value for
the log entry.

```clojure
(handler-bind [::invalid-log-entry (fn [_ & _]
                                     (far/use-value :invalid-entry))]
  (parse-log-line "Some random invalid log line"))
```

    :invalid-entry

One of the uses this could have is to allow multiple log formats to be used. In
the old implementation, if we wanted to support multiple log formats, we'd have
to change the regex. With this way of handling logs, we can extend the existing
behavior to add new valid log entries. This may be useful if for example we have
our application's process sending its logs to stdout, the same place where a
process monitor is also sinking its logs.

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
```

    ({:date
      #object[java.time.LocalDateTime 0x6d8ab953 "2021-05-21T21:46:30.321411"],
      :thread "main",
      :level :debug,
      :ns some.namespace,
      :message "some text"}
     {:level :debug, :ns other.namespace, :message "More text"})

This `alt-middleware` function takes the function it wants to wrap, in this case
an eager-ized `parse-log-file`, and then returns a function that calls it in the
context of a handler for `::invalid-log-entry` that will attempt to parse it
using a second log format. Notice however that if it fails to parse the log
line, it simply returns normally instead of invoking a restart. This is
intentional. If a handler returns normally without invoking a restart, then it
has decided not to handle the condition, and handlers further up the stack will
be used, in this case the one which calls the `far/continue` restart.

There's one case we haven't covered yet though, and it's best illustrated by
this example.

```clojure
(parse-log-line "invalid-date [main] DEBUG some.namespace - some text")
```

    class java.time.format.DateTimeParseException

This throws a date format exception! We don't want exceptions here, instead we'd
prefer if it were signaled as a condition. In order to do that, we'll use
`translate-exceptions`.

```clojure
(defn parse-log-line
  [s]
  (restart-case
      (if-let [[date thread level ns message] (next (re-matches log-line-regex s))]
        (translate-exceptions [Exception (fn [_] (values ::invalid-log-entry :log-line s))]
          {:date (LocalDateTime/parse date DateTimeFormatter/ISO_DATE_TIME)
           :thread thread
           :level (keyword (str/lower-case level))
           :ns (symbol ns)
           :message message})
        (far/error ::invalid-log-entry
                   :log-line s))
    (::far/continue [])
    (::far/use-value [v] v)))

(defn parse-log-file
  [lines]
  (keep parse-log-line lines))
```

Java code, as well as Clojure library code, will usually not be using farolero
for its error handling. The only method of error handling likely to be used by
either that would interfere with our code is if an exception were to be thrown
from inside some code using conditions, just like the date format exception.
This is where `translate-exceptions` comes in. When an exception is thrown from
inside the body of `translate-exceptions`, it checks to see if it's one of the
types of exceptions there's a binding for. If it is, then it'll call the
function that's bound with the exception as its argument, and use its return
values to construct a condition. For now, just ignore the `values` call, and
treat it like it's returning a list.

```clojure
(parse-log-line "invalid-date [main] DEBUG some.namespace - some text")
```

    #clojure.lang.ExceptionInfo {:message "Unhandled condition" :data {:condition ::invalid-log-entry :args '(:log-line "invalid-date [main] DEBUG some.namespace - some text")}}

In some cases, we'd like to preserve the original exception, but still want to
turn them into conditions. In these cases, we use `wrap-exceptions`, and then
use the exception classes as the handlers to bind.

```clojure
(handler-case (wrap-exceptions (throw (RuntimeException. "an error")))
  (Exception [e] :caught))
```

    :caught

# TODO Aggregating Statistics


# TODO Creating a CLI
