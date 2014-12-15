(ns racehub.util.validation
  "Shared validation utilities."
  (:require [racehub.util :as u]
            [schema.core :as s :include-macros true]
            [validateur.validation :as v]
            #+clj [forms-bootstrap.validation :as fbv]))

;; ## Schema

(def ValidationSet
  {(s/either s/Keyword [s/Keyword]) #{s/Str}})

;; ## Code

(s/defn lookup-k :- s/Keyword
  [k :- s/Keyword]
  (keyword (str (name k) "-lookup")))

(s/defn lookup-field
  "Returns a lookup-field schema"
  [k :- s/Keyword schema]
  (let [lookup (lookup-k k)]
    {k schema
     lookup {:state (s/enum :success :pending :failed)
             (s/optional-key :value) schema}}))

(s/defschema LookupFieldValidatorOpts
  {(s/optional-key :missing-message) s/Str
   (s/optional-key :invalid-message) s/Str})

(s/defn validate-lookup
  "Returns the validator that handles the lookup field."
  [k opts :- LookupFieldValidatorOpts]
  (v/validate-with-predicate
   k
   (comp #{:success} :state (lookup-k k))
   :message (:invalid-message opts)))

(s/defn lookup-validator
  "Returns a validator that acts on pairs of the schema returned by
  lookup-field. Checks that the key is present and that the lookup
  field has a successful value."
  [k :- s/Keyword
   opts :- LookupFieldValidatorOpts]
  (v/validation-set
   (v/validate-some
    (v/presence-of k :message (:missing-message opts))
    (validate-lookup k opts))))

(defn validate-set-when
  "Returns a validator that only acts on the map if (predicate m)
  passes."
  [predicate validation-set]
  (fn [m]
    (if (predicate m)
      (validation-set m)
      {})))

(defn validate-when
  "Macro that only provides the supplied validator if the predicate
  evaluates to true. Lazy."
  [pred v]
  (if pred v (constantly [true {}])))

(defn valid-amount?
  "Returns a validator that takes a map and checks the value at the
  supplied location is an integer."
  [korks]
  (v/numericality-of korks :only-integer true))

(defn merge-errors
  "Accepts a sequence of error maps (returned by the application of a
  `validation-set` call) and merged them all together."
  [errs]
  (apply merge-with into errs))

(defn validate-schema
  "Returns a function that, when given a map, will validate that the
  value of the attribute in that map passes the supplied schema.

  The message here isn't great, and of course the schema should just
  generate the entire damned validator, but this gets us started."
  [attr & {:keys [schema message]
           :or {message "Schema validation failed."}}]
  (let [f (if (vector? attr) get-in get)]
    (if-not schema
      (constantly [true {}])
      (fn [m]
        (let [v (f m attr)]
          (if (s/check schema v)
            [false {attr #{message}}]
            [true {}]))))))

(defn valid-email?
  "Accepts an email keyword and an optional :message argument, and
  returns a validator that checks if the item in the supplied "
  [k & {:keys [message] :or {message "Please enter a valid email."}}]
  (letfn [(validate-email [email]
            (let [length (count email)]
              (and (> length 2)
                   (< length 60)
                   (re-matches #"\S+@\S+\.\S+" email)
                   (nil? (re-seq #"[\s]" email)))))]
    (v/validate-by k validate-email :message message)))

(def birthdate-validator
  (v/validation-set
   (v/validate-by :day #(and (number? %) (> % 0) (< % 32))
                  :message "Birthdate day must be a valid number.")
   (v/validate-by :month #(and (number? %) (> % 0) (< % 13))
                  :message "Birthdate month must be a valid number.")
   (v/validate-by :year #(and (number? %) (> % 1899) (< % 2014))
                  :message "Birthdate year must be a valid number.")))

#+clj
(defn new->old
  "Accepts the results of a validation-set in the new style and
  returns an old-style validator that uses forms-bootstrap."
  [validator]
  (fn [m]
    (if-let [errors (not-empty (validator m))]
      (reduce (fn [acc [k v]]
                (fbv/add-validation-error acc k v))
              m
              (for [[k vs] errors, v vs]
                [k v]))
      m)))
