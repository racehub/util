(ns racehub.util.validation-test
  (:use clojure.test
        racehub.util.validation)
  (:require [validateur.validation :as v]))

(deftest validator-conversion-test
  (let [v (new->old (constantly {:title #{"a" "b"}}))]
    (is (= {:a "value"
            :_validation-errors {:title ["a" "b"]}}
           (v {:a "value"}))
        "Validations get converted over.")
    (is (= {:a "b"} ((new->old (constantly {}))  {:a "b"}))
        "No errors get added when the new validations are empty.")))

(deftest validate-some-test
  "Tests for the validate-some helper."
  (let [v (validate-some
           (v/presence-of :cake-count :message "missing_cake")
           (v/validate-by :cake-count odd? :message "even_cake"))]
    (is (= [true #{}] (v {:cake-count 1}))
        "Odd cake counts are valid.")

    (is (= [false {:cake-count #{"even_cake"}}] (v {:cake-count 2}))
        "Even cake counts only throw the second error, since the first
      validation passed.")

    (is (= [false {:cake-count #{"missing_cake"}}] (v {}))
        "The second validation never gets called and never throws a
        NPE, as it would if we just composed them up.")))

(deftest errors-test
  (let [v (v/validation-set
           (v/presence-of :cake-count))
        compound-v (v/validation-set
                    (v/presence-of [:a :b])
                    (v/presence-of :c))]
    (is (errors? :cake-count (v {}))
        "Missing key triggers an error.")

    (is (errors? :cake-count {[:cake-count] #{"something"}})
        "It works if the error map has a nested, single keyword (as
        happens when we use unnest)")

    (is (not (errors? :cake-count (v {:cake-count "hi!"})))
        "No errors, since cake-count is present.")

    (testing "errors? Works for nested keywords too"
      (is (errors? [:a :b] (compound-v {})))
      (is (not (errors? [:a :b] (compound-v {:a {:b "something"}})))))))
