(ns racehub.util.validation-test
  (:use clojure.test
        racehub.util.validation))

(deftest validator-conversion-test
  (let [v (new->old (constantly {:title #{"a" "b"}}))]
    (is (= {:a "value"
            :_validation-errors {:title ["a" "b"]}}
           (v {:a "value"}))
        "Validations get converted over.")
    (is (= {:a "b"} ((new->old (constantly {}))  {:a "b"}))
        "No errors get added when the new validations are empty.")))
