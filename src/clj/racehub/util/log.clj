(ns racehub.util.log
  (:require [cemerick.friend :as friend]
            [cheshire.core :refer [generate-string]]
            [clj-stacktrace.core :as st]
            [clj-stacktrace.repl :as sr]
            [noir.request :refer [*request*]]
            [schema.core :as s]
            [taoensso.timbre :as log])
  (:import [java.io StringWriter]))

;; ## Schemas

(def RequestID
  (s/named s/Str "Unique ID the current request coming in from Heroku."))

(def StructuredLog
  {:action s/Str
   :authenticated? boolean
   (s/optional-key :current-user) (s/named s/Str "Username")
   (s/optional-key :request-id) RequestID
   s/Keyword s/Any})

;; ## Functions

(s/defn structured-log :- StructuredLog
  "Generates a structured log output."
  [req :- {s/Any s/Any}
   action :- s/Str
   attrs :- {s/Keyword s/Any}]
  (let [auth-m (if-let [id (friend/current-authentication req)]
                 {:authenticated? true
                  :current-user (:username id)}
                 {:authenticated? false})]
    (merge {:action action}
           (when-let [uuid (:uuid req)]
             {:request-id uuid})
           (when-let [uri (:uri req)]
             {:uri uri})
           auth-m
           attrs)))

(defn log-stacktrace
  "Logs the stacktrace of the current exception with ansi terminal
  encoding on."
  [& [e]]
  (let [sw (StringWriter.)]
    (sr/pst-on sw true (or e *e))
    (log/error (str sw))))

(defn request []
  (if (bound? #'*request*)
    *request*
    {}))

(s/defn spying? :- s/Bool
  "Returns true if the supplied request is harboring a spy, false
  otherwise."
  [req]
  (boolean
   (:spy (friend/identity req))))

(s/defn exception :- (s/eq nil)
  "Logs the supplied exception as a JSON string."
  [e :- Throwable]
  (-> (st/parse-exception e)
      (update-in [:class] (fn [^Class c] (.getName c)))
      (generate-string)
      (log/error)))

(defmacro ^:private def-logger [sym]
  (let [timbre-sym (symbol "taoensso.timbre" (name sym))]
    `(defn ~sym
       ([action# attrs#]
          (let [req# (request)]
            (~sym req# action# attrs#)))
       ([req# action# attrs#]
          (when-not (spying? req#)
            (~timbre-sym
             (generate-string (structured-log req# action# attrs#))))))))

(defmacro ^:private def-loggers
  "Defines a mirrored logger for every level provided by Timbre."
  []
  `(do ~@(map (fn [level]
                (let [level (symbol (name level))]
                  `(def-logger ~level)))
              log/levels-ordered)))

;; ## API
;;
;; This macro defines loggers that mirror the info, error, etc
;;functions in Timbre. One function's defined for each entry in
;;`taoensso.timbre/levels-ordered`.
;;
;; Each function takes
;;
;; - an optional first argument of request (The request provides the
;;   Req UUID and the currently authenticated user)
;;
;; - an "action", just a string to identify this when searching later
;;
;; - A map of keyword -> anything. These extra attributes all get
;;   merged in when sending data off to Kibana.

(def-loggers)

(comment
  "Example:"
  (defn tickle! [request]
    (log/info request
              "tickle"
              {:ticker "tim"
               :ticklee "dave"})
    (perform-tickle! "tim" "dave"))

  "Response:"
  ;; 2014-Feb-12 12:59:57 -0700 RitchieMacBook.local INFO [racehub.util.log] - {:ticklee dave, :ticker tim, :authenticated? false, :action tickle}
  )
