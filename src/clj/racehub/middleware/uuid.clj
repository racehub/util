(ns racehub.middleware.uuid
  "Middleware to inject a UUID into a ring request."
  (:import (java.util UUID)))

(defn wrap-uuid
  "Injects a unique identifier into each request. Useful for logging
  if you want breakdowns of certain actions per request."
  [handler]
  (fn [req]
    (let [uuid (or (-> req :headers (get "heroku-request-id"))
                   (str (UUID/randomUUID)))]
      (handler (assoc req :uuid uuid)))))
