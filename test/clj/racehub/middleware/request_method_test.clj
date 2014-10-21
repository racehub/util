(ns racehub.middleware.request-method-test
  (:use clojure.test
        racehub.middleware.request-method))

(deftest request-method-test
  (let [handle (ensure-request-method identity)]
    (is (= {:form-params {"_method" "DELETE"}
            :request-method :delete
            :a "b"}
           (handle
            {:request-method :post
             :form-params {"_method" "DELETE"}
             :a "b"}))
        "If a _method parameter's in the form params, it overrides the
        supplied request-method.")

    (testing "If _method' not present, keys make it through untouched."
      (is (= {:request-method :post}
             (handle {:request-method :post})))
      (is (= {:foo "bar"} (handle {:foo "bar"}))))))
