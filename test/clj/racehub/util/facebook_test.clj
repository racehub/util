(ns racehub.util.facebook-test
  (:use clojure.test
        racehub.util.facebook)
  (:require [racehub.util.oauth :as o]))

(defn with-user*
  "Takes a function of one argument and passes in a facebook test
  user."
  [f]
  (f (first (:data (test-users)))))

(defmacro with-user
  "Accepts a binding form (destructuring allowed) and evaluates the
  supplied body with a facebook test user bound to that form."
  [[binding] & forms]
  `(with-user* (fn [~binding] ~@forms)))

(deftest me-test
  (with-user [{id :id token :access_token}]
    (let [fetched (me token)]
      (is (= id (:id fetched))
          "Fetching the user with the me endpoint returns a
                     user with matching ID."))))
