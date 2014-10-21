(ns racehub.util.config
  (:require [environ.core :as e]
            [schema.core :as s]))

(def ^:dynamic *env* {})

(defn env
  ([] (merge e/env *env*))
  ([k] ((env) k))
  ([k default] (get (env) k default)))

(defn kenv
  "Returns the value for the supplied key as a keyword, fallback
  otherwise."
  ([key]
     (kenv key nil))
  ([key fallback]
     (keyword (let [v ((env) key)]
                (if (empty? v)
                  fallback
                  v)))))

(letfn [(string->boolean [s]
          (= s "true"))]
  (defn benv
    "Returns the value for the supplied key as a boolean, fallback
  otherwise."
    ([key]
       (benv key nil))
    ([key fallback]
       (string->boolean ((env) key fallback)))))

(defn with-env*
  "Executes the supplied function in an environment where conf's kv
  pairs override the default config."
  [conf f]
  {:pre [(map? conf)]}
  (binding [*env* (merge *env* conf)]
    (f)))

(defmacro with-env
  "Executes the supplied body in an environment where conf's bindings
  override the default env."
  [conf & body]
  `(with-env* ~conf (fn [] ~@body)))

(s/defn prefixed* :- s/Any
  "Executes the supplied function in an environment where each of the
  keys is overridden by the same key, only prefixed."
  [prefix :- s/Keyword
   ks :- [s/Keyword]
   f :- (s/=> s/Any s/Any)]
  (let [kvs (->> (for [k ks]
                   [k (env (keyword (str (name prefix) "-" (name k))))])
                 (into {}))]
    (with-env* kvs f)))

(defmacro prefixed
  [prefix ks & forms]
  `(prefixed* ~prefix [~@ks] (fn [] ~@forms)))
