(ns racehub.util.liberator-test
  "Tests for the liberator helper namespace."
  (:use midje.sweet
        racehub.util.liberator)
  (:require [clojure.test :refer [deftest]]))

(defn sans-merge [m]
  (chatty-checker [l] (= m (dissoc l :merge-with))))

(deftest flatten-testing
  (fact
    "Merging maps with flatten-resource works:"
    (flatten-resource
     {:base {:a "a"} :b "b"}) => {:a "a" :b "b"}

    "Clashing keys are either knocked out:"
    (flatten-resource
     {:base {:a "a"} :a "b"}) => {:a "b"}

    "Or merged with the :merge-with binary function:"
    (flatten-resource
     {:base {:a "a"}
      :merge-with (fn [a b] a)
      :a "b"}) => (sans-merge {:a "a"})

    (let [base {:a "a"
                :base {:key "value"
                       :a "deepest"}
                :merge-with (fn [a b] a)}
          nested {:base base
                  :a "b"
                  :new "old"}]
      "This works recursively, with the lowest merge-with function
  sticking around for some time:"
      (flatten-resource nested)
      => (sans-merge {:a "deepest"
                      :key "value"
                      :new "old"})

      "Combat this by nuking it with a nil, or a new map:"
      (let [merger (fn [a b] a)]
        (flatten-resource
         {:base base
          :a "b"
          :merge-with nil
          :new "old"})
        => (sans-merge {:a "b"
                        :key "value"
                        :new "old"})))))

(defn with-type [m type]
  (-> m
      (assoc-in [:resource :available-media-types]
                (constantly
                 ["text/plain"
                  "application/json"
                  "cake"]))
      (assoc-in [:representation :media-type] type)))

(defn has-body [b]
  (chatty-checker [m] (= b (-> m :response :body))))

(deftest media-typed-test
  (let [base (media-typed
              {"text/plain" "hi"
               "application/json" "json"
               "cake" (fn [req]
                        (:entry req))
               :default "default"})]
    (fact "media-typed delegates properly"
      ;; Unspecified accept type gets the first response by default
      ;; (text/plain in this case, because of the list above).
      (base (-> {:entry "an entry"} (with-type nil))) => (has-body "hi")
      (base (-> {:entry "an entry"} (with-type "cake"))) => "an entry"
      (base (-> {:entry "an entry"} (with-type "text/plain"))) => (has-body "hi")
      (base (-> {:entry "hum"} (with-type "application/json"))) => (has-body "json")
      (base (-> {:entry "hum"} (with-type "newtype"))) => (has-body "default"))))

(defn with-content [s]
  {:headers {"content-type" s}})

(deftest content-type-test
  (fact "Content type only returns the base."
    (content-type (with-content "application/json; charset=utf8"))
    => "application/json"

    "But works still without params."
    (content-type (with-content "application/json")) => "application/json"))

(defn accepts
  "Returns a checker that checks a request to see if it's accepted
  types vector is the same as the supplied type-seq."
  [type-seq]
  (chatty-checker [req]
                  (= (accepted-types req)
                     type-seq)))

(defn with-accept [d]
  {:headers {"accept" d}})

(deftest weighted-type-test
  (fact "accepted-types returns headers sorted by weight."
    (with-accept
      "application/json;q=0.9,application/edn")
    => (accepts ["application/edn" "application/json"])

    "Given no particular preference, the items get returned in
     alphabetical order."
    (with-accept "a/c,a/b,a/a") => (accepts ["a/a" "a/b" "a/c"])

    "returns an empty seq if no acceptance is possible."
    (with-accept "") => (accepts [])))

(defn matches
  "Returns a checker that checks that the request negotiates media
  type properly."
  ([t]
     (chatty-checker [req] (= t (get-media req))))
  ([t accepted]
     (chatty-checker [req] (= t (get-media req accepted)))))

(deftest accept-ranking-test
  (fact
    "If no preferred item is specified, the system returns the highest
  ranked."
    (with-accept "application/json;q=0.9,application/edn")
    => (matches "application/edn")

    "If we only accept a lower priority, returns the best option."
    (with-accept "application/json;q=0.9,application/edn,text/html;q=0.5")
    => (matches "application/json" ["application/json" "text/html"])

    "Again, we pick whatever we accept. Multiple equally weighted
   matches sorts alphabetically."
    (with-accept "a/c,a/b,a/a") => (matches "a/b" ["a/c" "a/b"])

    "If they can't agree, get-media returns nil."
    (with-accept "a/c,a/b,a/a") => (matches nil ["a/z"])))
