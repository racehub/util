(ns racehub.schema
  "Common schema elements."
  (:require [racehub.util :as u]
            [schema.core :as s :include-macros true])
  #+clj (:import [clojure.core.async.impl.protocols ReadPort]))

(def Channel
  "core.async channel."
  #+cljs s/Any
  #+clj ReadPort)

(def UnixTimestamp
  (s/named s/Int "Unix timestamp. Seconds since epoch."))

(def Timestamp
  (s/named s/Str "timestamp"))

(def EnliveNode
  {s/Keyword s/Any})

(s/defschema Function
  (s/=> s/Any s/Any))

(s/defschema Percent
  (s/both s/Int (s/pred (u/between 1 101))))

(s/defschema PositiveInt
  (s/both s/Int (s/pred pos?)))

(s/defschema PositiveNumber
  (s/both s/Num (s/pred pos?)))

(defn non-negative [schema]
  (s/both schema (s/pred (complement neg?))))

(s/defschema NonNegativeInt
  (non-negative s/Int))

(s/defschema NonNegativeNumber
  (non-negative s/Num))

(def Currency NonNegativeInt)

(s/defschema Pennies
  "Pennies can be negative for refunds."
  s/Int)

(s/defschema Millis
  "non-negative integer representing milliseconds."
  NonNegativeInt)

(s/defn percent-str :- s/Str
  [percent :- Percent]
  (str percent "%"))

(def sorted-coll?
  (s/pred (fn [items]
            (= (sort items) items))))

(defn sorted [schema]
  (s/both schema sorted-coll?))

(s/defn toggle-optional :- {s/Any s/Any}
  "Takes in a Schema, and a keyword to toggle."
  [schema :- {s/Any s/Any}
   k :- (s/named s/Any "Key to toggle")]
  (if-let [optional-v (get schema (s/optional-key k))]
    ;;change to required:
    (-> (assoc schema k optional-v)
        (dissoc (s/optional-key k)))
    ;;change to optional:
    (-> (assoc schema (s/optional-key k) (get schema k))
        (dissoc schema k))))
