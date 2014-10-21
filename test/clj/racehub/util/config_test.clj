(ns racehub.util.config-test
  (:use clojure.test
        racehub.util.config))

(deftest with-env-test
  "with-env merges items into the main environment."
  (with-env {:ring-env "prod"
             :test-key "true"}
    (is (= "prod" (env :ring-env)))
    (is (= "true" (env :test-key)))))
