(ns racehub.session
  "Functions for managing the Redis session store."
  (:require [clj-redis-session.core :refer [redis-store]]
            [racehub.util :refer [days->secs]]
            [racehub.util.config :as c]
            [schema.core :as s]
            [taoensso.carmine :as car]
            [taoensso.timbre :as log]))

;; ## Config Code

(s/defschema SessionStore
  (s/enum :memory :redis))

(s/defn redis-config :- {:spec {(s/optional-key :uri) s/Str}}
  "Map of Redis settings. Note that every group of settings has to
  have a spec key to work with carmine."
  []
  {:spec (if-let [uri (c/env :redis-url)]
           {:uri uri}
           {})})

(s/defn session-store :- SessionStore
  "Returns the mode the system should use for the session store. Redis
  by default."
  []
  (c/kenv :session-store :redis))

(def cache-expiration-days
  "Number of days before a session cached in redis expires."
  20)

(defn session-config []
  {:store (session-store)
   :expire-secs (days->secs cache-expiration-days)
   :reset-on-read true
   :prefix "session"})

;; ## API

(defn ping [spec]
  (car/wcar spec (car/ping)))

(defmacro wcar [& body]
  `(car/wcar (redis-config)
             ~@body))

(defn try-redis []
  (let [spec (redis-config)]
    (try (when (ping spec) spec)
         (catch Exception e
           (log/error e "Couldn't connect to Redis!")))))

(defn setup-redis-store
  "Returns a local or remote redis store. Defaults to local."
  []
  (log/info "Setting up Redis.")
  (when-let [redis (try-redis)]
    (redis-store redis (session-config))))
