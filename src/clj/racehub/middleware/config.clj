(ns racehub.middleware.config)

(defn wrap-config
  "Injects configuration data into the stream."
  [handler conf]
  (fn [request]
    (handler
     (assoc request :config
            (select-keys conf [:mode :dev? :cdn])))))
