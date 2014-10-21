(ns racehub.middleware.bare
  (:require [cemerick.friend :as friend]
            [clojure.string :as s]
            [racehub.util.ring :refer [scheme-redirect]]))

(defn is-bare [s]
  (or (not (string? s))
      (not (.startsWith ^String s "www"))))

(defn strip-www
  "Ring middleware similar to friend/requires-scheme that should be
  able to handle things like load balancers in Amazon's elastic
  beanstalk and heroku in addition to other load balancers and reverse
  proxies that use x-forwarded-proto and thus don't set :scheme in the
  request map properly. Do not use if your application server is
  directly facing the internet as these headers are *very* easy to
  forge."
  ([handler scheme]
     (strip-www handler scheme friend/*default-scheme-ports*))
  ([handler scheme scheme-mapping]
     (fn [request]
       (if (is-bare (:server-name request))
         (handler request)
         (-> request
             (update-in [:server-name] s/replace-first "www." "")
             (scheme-redirect scheme scheme-mapping))))))
