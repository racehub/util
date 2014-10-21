(ns racehub.middleware.request-method
  (:require [clojure.string :as st]))

(defn ensure-request-method
  "Middleware to ensure that the request method is properly processed
  out of the form params. Without this, any ':delete' post that hits a
  liberator endpoint will be processed as a :post.

  (Compojure only processes this form-params field when you explicitly
  list a route as (DELETE theroute [] handler).)"
  [handler]
  (fn [request]
    (handler
     (if-let [method (get-in request [:form-params "_method"])]
       (assoc request :request-method (keyword (st/lower-case method)))
       request))))
