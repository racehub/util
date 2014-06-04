(ns paddleguru.algebra-test
  (:refer-clojure :exclude [associative?])
  (:require [clojure.test :refer :all]
            [paddleguru.algebra :as a]
            [clojure.test.check :as sc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :refer (for-all)]))

(defn test-property
  ([p] (test-property 100 p))
  ([num-tests p]
     (is (:result (sc/quick-check num-tests p)))))

(defn associative?
  "Returns a property that tests that the supplied function is
  associative for values produced by the supplied generator."
  [generator f]
  (prop/for-all [x generator
                 y generator
                 z generator]
                (= (f (f x y) z)
                   (f x (f y z)))))

(defn semigroup-law
  "Tests that the supplied generator produces values that can add
  together with paddleguru.algebra/plus."
  [type-name generator]
  (testing (str type-name " forms a semigroup.")
    (test-property
     (associative? generator a/plus))))

(defn identity?
  "Returns a property that checks that the supplied zero is an
  identity for the supplied generator, when passed in to the supplied
  function."
  [generator f zero]
  (prop/for-all [x generator]
                (and (= (f x zero) x)
                     (= (f zero x) x))))

(defn monoid-laws
  "Tests that the supplied monoid is both associative (in the 2-arg
  case) and produces an identity in the no-arg case."
  [type-name generator monoid]
  (let [zero (monoid)]
    (semigroup-law type-name generator)
    (testing (format "%s and %s form a monoid." type-name zero)
      (test-property
       (identity? generator a/plus zero)))))

(deftest numeric-monoid-laws
  ;; Checks that Clojure's numeric types work monoid style.
  (monoid-laws "int" gen/int a/numeric-monoid)
  (monoid-laws "ratio" gen/ratio a/numeric-monoid)
  (monoid-laws "map-monoid" (gen/map gen/int gen/int) a/map-monoid))

(deftest lazycat
  ;;Checks to make sure we can add two lazy sequences.
  (is (= (a/plus (take 2 (range 10)) (take 3 (range 4 10)))
         '(0 1 4 5 6))))

(deftest lazysum
  (is (= (a/sum a/map-monoid
                (for [a (range 10)]
                  {:foo [a]}))
         {:foo '(0 1 2 3 4 5 6 7 8 9)})
      "Make sure we can add sum over lazy sequences.")
  (is (and (= (a/sum a/map-monoid []) {})
           (= (a/sum a/vector-monoid []) []))
      "Summing an empty collection returns the zero of the monoid."))
