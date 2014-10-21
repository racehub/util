(ns racehub.middleware.bare-test
  (:use midje.sweet
        racehub.middleware.bare)
  (:require [clojure.test :refer [deftest]]
            [ring.mock.request :as r]))

(defn make-req
  "Creates a get request."
  [s]
  (r/request :get s))

(defn process
  "Processes the supplied request through the bare/strip-www
  middleware."
  [req]
  ((strip-www identity :https) req))

(deftest strip-www-test
  (fact
    (let [response (process (make-req "http://www.paddleguru.com/cake"))]
      "redirect to the www-less version."
      (-> response :headers (get "Location")) => "https://paddleguru.com/cake"

      "permanent redirect"
      (-> response :status) => 301)

    (let [req (make-req "http://paddleguru.com")]
      (fact "Without www, the middleware passes through."
        (process req) => req))

    (let [req (dissoc (make-req "http://paddleguru.com")
                      :server-name)]
      (fact "Even without the server name, no NPE!"
        (process req) => req))))
