(ns racehub.util.crypto
  "RaceHub's crypto helpers."
  (:require [lock-key.core :as l]
            [racehub.util.config :as conf]
            [schema.core :as s])
  (:import [org.apache.commons.codec.binary Base64]))

(def ^:dynamic *key*
  "Dynamic variable for custom bindings of a cryptographic key. Mainly
  used for testing."
  nil)

(defmacro with-key
  "Performs the encryption below with a custom key in the scope of
  this thread."
  [[k] & body]
  `(binding [*key* ~k]
     ~@body))

(s/defn crypto-key :- s/Str
  []
  {:post [(string? %)]}
  (or *key* (conf/env :crypto-key)))

(s/defn decrypt :- s/Str
  "Decrypts the incoming base64 encoded message using the crypto key."
  [input :- s/Str]
  (-> (Base64/decodeBase64 input)
      (l/decrypt (crypto-key))
      (String.)))

(s/defn encrypt :- s/Str
  "Returns an encrypted, Base 64 encoded version of the supplied
  message, encrypted using the crypto key."
  [s :- s/Str]
  (Base64/encodeBase64String
   (l/encrypt s (crypto-key))))

(s/defn base-64? :- s/Bool
  "Returns true if the supplied string is Base64 encoded, false
  otherwise."
  [s :- s/Str]
  (Base64/isBase64 s))
