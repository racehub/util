(ns racehub.session
  "Functions for managing the Redis session store."
  (:require [taoensso.carmine :as car]
            [taoensso.timbre :as log]
            [clj-redis-session.core :refer [redis-store]]
            [racehub.util.config :refer [get-config]]
            [schema.core :as s]))

(s/defn redis-config :- {:spec {(s/optional-key :uri) s/Str}}
  "Map of Redis settings. Note that every group of settings has to
  have a spec key to work with carmine."
  []
  {:spec (if-let [uri (c/env :redis-url)]
           {:uri uri}
           {})})

(defn ping [spec]
  (car/wcar spec (car/ping)))

(defmacro wcar [& body]
  `(car/wcar (get-config :redis)
             ~@body))

(defn try-redis []
  (let [spec (get-config :redis)]
    (try (when (ping spec) spec)
         (catch Exception e
           (log/error e "Couldn't connect to Redis! "
                      "Falling back to Noir's session store.")))))

(defn setup-redis-store
  "Returns a local or remote redis store. Defaults to local."
  []
  (log/info "Setting up Redis.")
  (when-let [redis (try-redis)]
    (redis-store redis (get-config :session {}))))
