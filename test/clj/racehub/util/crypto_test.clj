(ns racehub.util.crypto-test
  (:use racehub.util.crypto)
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :refer [for-all]])
  (:import [org.apache.commons.codec.binary Base64]))

(defspec with-key-test 100
  (for-all [k gen/string]
           (with-key [k]
             (= k (crypto-key)))))

;; Encrypting and decrypting a custom message with a custom key works.
(defspec round-trip-encryption 100
  (for-all [k gen/string
            message gen/string]
           (with-key [k]
             (let [encrypted (encrypt message)]
               "The encoded message is base 64 encoded, and decrypting
                it yields the original message."
               (and (base-64? encrypted)
                    (= message (decrypt encrypted)))))))
