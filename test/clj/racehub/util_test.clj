(ns racehub.util-test
  (:use midje.sweet
        racehub.util)
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [schema.core :as s]))

(deftest format-currency-test
  (is (= 1024.00
         (format-currency "1,024.00")
         (format-currency 1024))
      "Numbers gets parsed."))

;; TODO: Add test.check to round trip amounts through the currency
;; string functions.
(deftest currency-string-test
  (is (= 100 (currency-str->pennies "$1"))
      "No pennies is interpreted as dollars.")
  (is (= 200 (currency-str->pennies "2")))
  (is (= 543 (currency-str->pennies "$5.43"))
      "Dollar signs get stripped.")
  (is (= 600 (currency-str->pennies "6.00"))
      "Dollar sign doesn't need to be present.")
  (is (= 600000 (currency-str->pennies "6,000.00"))
      "Commas are okay."))

(deftest pennies->currency-test
  (is (= "$12.04" (pennies->currency 1204)))
  (is (= "-$12.37" (pennies->currency -1237))))

(deftest deep-merge-test
  (is (= {} (deep-merge))
      "Deep merging nothing results in nothing.")
  (is (= {:x "y"} (deep-merge {:x "y"}))
      "Deep merging a single map returns that map.")
  (let [base {:name {:first {:first-letter "s" :rest "am"}
                     :last "ritchie"}}]
    (is (= (assoc base :a "b") (deep-merge base {:a "b"}))
        "deep merging when a key's missing just adds that key.")

    (is (= {:name {:first {:first-letter "s" :rest "ucker"}
                   :last "ritchie"}}
           (deep-merge base {:name {:first {:rest "ucker"}}}))
        "deep-merging will knock out keys way in, but leave other keys
        alone.")

    (is (= {:name "Sam"} (deep-merge base {:name "Sam"}))
        "You can knock maps out too.")))

(deftest merge-with-map-test
  (fact
    "merge-with-map: If the supplied function map has a function to
   merge with, use that. Otherwise, right wins."
    (merge-with-map {:a +
                     :b -}
                    {:a 1 :b 10}
                    {:a 2 :c "hi!"}
                    {:a 3 :b 5 :c "ho!"})
    => {:a 6, :b 5, :c "ho!"}))

(deftest mapk-test
  (is (= {1 1 4 2 9 3} (mapk #(* % %) {1 1 2 2 3 3}))
      "mapk maps across keys and returns another map."))

;; Test of the create, update, delete cycle for customers.
(deftest collectify-test
  (tabular
   (fact (collectify ?coll) => ?result)
   ?coll       ?result
   '(1 2 3)    '(1 2 3)
   [5 5]       [5 5]
   "aaa"       ["aaa"]
   {:a 1 :b 2} [{:a 1 :b 2}]
   #{1 2 3}    #{1 2 3}))

(deftest ensure-defaults-test
  (is (= {:genders ["All"]} (ensure-defaults {:genders []} :genders ["All"]))
      "Empty seqs get bumped by the default.")
  (is (= {:a "b" :b "c"} (ensure-defaults {:a nil} :a "b" :b "c"))
      "Defaults always beat nil.")
  (is (= {:genders "default!"} (ensure-defaults {:genders []} :genders "default!"))
      "Default can be anything...")
  (is (= {:genders nil} (ensure-defaults {:genders []} :genders nil))
      "Default can be nil.")
  (is (= {:a "b" :b ["z"]} (ensure-defaults {:a "b"} :a ["All"] :b ["z"]))
      "Defaults make their way in even if the key wasn't originally
      present."))

(deftest to-currency-test
  (is (= "$0.00" (to-currency nil))
      "Nil handling works.")
  (is (= "$0.00" (to-currency "busted!"))
      "Busted strings parse to 0.")
  (is (= "$12.43" (to-currency 12.43))
      "Floats do the right thing.")
  (is (= "$100,123.00" (to-currency 100123))
      "large ints are printed in a sexy way."))

(deftest currency-amt-test
  (s/without-fn-validation
   (is (= "0.00" (currency-amt nil))
       "Nil handling works."))
  (is (= "0.00" (currency-amt "asdf"))
      "Busted strings parse to 0.")
  (is (= "12.43" (currency-amt 12.43))
      "Floats do the right thing.")
  (is (= "12.54" (currency-amt "12.54"))
      "Numeric strings are OK")
  (is (= "12.44" (currency-amt 12.43523423))
      "Rounds of to nearest hundredth.")
  (is (= "12.44" (currency-amt "12.43523423"))
      "Strings rounds too.")
  (is (= "100,123.00" (currency-amt 100123))
      "large ints are printed in a sexy way.")
  (is (= "100,123.00" (currency-amt "100,123"))
      "String input with commas succeeds")
  (is (= "-12.00" (currency-amt "-12.00"))
      "Negative numbers yield negative currencies."))

(deftest number-string-test
  (is (= true (number-string? "123"))
      "Strings with just numbers return true")
  (is (= false (number-string? nil))
      "Nil input returns false")
  (is (= true (number-string? "123,342.34"))
      "Nicelly formatted (commas and periods) OK.")
  (is (= false (number-string? "12s23"))
      "Letters not OK.")
  (is (= false (number-string? "$123"))
      "Special symbols not OK.")
  (is (= true (number-string? "-123"))
      "Negative numbers OK."))

(deftest valid-currency-test
  (is (= true (valid-currency-amt? "123.00"))
      "String input with exactly two decimal points.")
  (is (= false (valid-currency-amt? "123.0"))
      "One decimal not enough")
  (is (= false (valid-currency-amt? "123"))
      "No decimals not OK")
  (is (= false (valid-currency-amt? "-123.00"))
      "Negative amount not allowed.")
  (is (= false (valid-currency-amt? "s123.00"))
      "Non-numbers or periods not allowed")
  (is (= true (valid-currency-amt? "1,234.00"))
      "Commas OK")
  (is (= false (valid-currency-amt? nil))
      "Handles nil")
  (is (= false (valid-currency-amt? "0"))
      "0 is not a valid currency (must be 0.00)"))

(deftest clean-currency-test
  (is (= "12.00" (clean-currency "12"))
      "Numbers are valid, return a string with two decimal places")
  (is (= "12.87" (clean-currency "12.86723423"))
      "Rounds to nearest penny.")
  (is (= "12,342.03" (clean-currency "12,342.03"))
      "Well placed commas are valid.")
  (is (= "0.00" (clean-currency "1234,2.03"))
      "Improperly placed commas are invalid.")
  (is (= "1,000.00" (clean-currency "1000"))
      "Inserts commas if appropriate.")
  (is (= "0.00" (clean-currency "-34"))
      "Negative amounts are invalid, return zero.")
  (is (= "0.00" (clean-currency "asdc"))
      "Letters are invalid currency.")
  (is (= "0.00" (clean-currency "12d23"))
      "Letters are invalid currency."))

(deftest clear-specials-test
  (is (= "RaceHub" (clear-specials "Race Hub"))
      "Letters are valid")
  (is (= "123456" (clear-specials "123 456"))
      "Numbers are valid")
  (is (= "Race123Hub456" (clear-specials "Race 123 Hub 456"))
      "Letters/Numbers are valid")
  (is (= "NoParens") (clear-specials "N((o)) (Parens)"))
  (is (= "NoSpecialCharacters"
         (clear-specials "No !@#$ Special %^&* Characters+_)(**&!@#$^#^?<>:"))
      "No Special Characters."))

(def select-gen
  (gen/one-of [(gen/return nil) gen/int (gen/return {}) (gen/return []) (gen/return #{})
               (gen/vector gen/int)]))

(def select-non-empty-idempotence
  (for-all [m (gen/map select-gen select-gen)]
           (= (select-non-empty m (keys m))
              (select-non-empty (select-non-empty m (keys m)) (keys m)))))

(def select-non-empty-all-empty
  (for-all [m (gen/map select-gen select-gen)]
           (->> m
                (map (some-fn nil? #{""} (every-pred coll? empty?)))
                (every? false?))))

(sc/quick-check 100 select-non-empty-idempotence)
(sc/quick-check 100 select-non-empty-all-empty)

(deftest underscore->dash-test
  (tabular
   (fact (underscore->dash ?m) => ?result)
   ?m            ?result
   {:_a 1}       {:-a 1}
   {:a 1}        {:a 1}
   {:a_b 1}      {:a-b 1}
   {:hi-foo 1}   {:hi-foo 1}
   {:mo_fo-bo 1} {:mo-fo-bo 1}
   {:foo_bar 1 :hi 2 :bar_foo_moo 3}    {:foo-bar 1 :hi 2 :bar-foo-moo 3}))

(deftest build-kw-map-test
  (let [a "Hi"
        b 23
        c ["A" 43]
        d {:foo "bar" :big {:dawg "33"}}
        e #{:hi :there}
        f :yomama]
    (is (= (build-kw-map a) {:a "Hi"}))
    (is (= (build-kw-map b) {:b 23}))
    (is (= (build-kw-map d) {:d {:foo "bar" :big {:dawg "33"}}}))
    (is (= (build-kw-map b f) {:b 23 :f :yomama}))
    (is (= (build-kw-map a b c e) {:a "Hi" :b 23 :c ["A" 43] :e #{:hi :there}}))))
