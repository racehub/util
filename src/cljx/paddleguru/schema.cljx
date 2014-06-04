(ns paddleguru.schema
  "Common schema elements."
  (:require [schema.core :as s]
            [paddleguru.util :as u]
            #+clj [potemkin :refer [import-vars]])
  (#+clj :require #+cljs :require-macros [schema.macros :as sm])
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

(sm/defschema Function
  (sm/=> s/Any s/Any))

(sm/defschema Percent
  (s/both s/Int (s/pred (u/between 1 101))))

(sm/defschema PositiveInt
  (s/both s/Int (s/pred pos?)))

(sm/defschema PositiveNumber
  (s/both s/Num (s/pred pos?)))

(defn non-negative [schema]
  (s/both schema (s/pred (complement neg?))))

(sm/defschema NonNegativeInt
  (non-negative s/Int))

(sm/defschema NonNegativeNumber
  (non-negative s/Num))

(def Currency NonNegativeInt)

(sm/defschema Millis
  "non-negative integer representing milliseconds."
  NonNegativeInt)

(sm/defn percent-str :- s/Str
  [percent :- Percent]
  (str percent "%"))

(def sorted-coll?
  (s/pred (fn [items]
            (= (sort items) items))))

(defn sorted [schema]
  (s/both schema sorted-coll?))

(sm/defn toggle-optional :- {s/Any s/Any}
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
