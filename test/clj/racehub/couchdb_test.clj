(ns racehub.couchdb-test
  (:require [clojure.string :refer [upper-case]]
            [clojure.test :refer :all]
            [com.ashafa.clutch :as c]
            [com.ashafa.clutch.utils :as utils]
            [racehub.couchdb :as db]
            [racehub.couchdb.testing :refer :all]))

;; ## CouchDB Tests

(deftest check-couchdb-connection
  (println "Testing using Clojure" *clojure-version*
           "on Java" (System/getProperty "java.version")
           "=>>" (-> test-host utils/url (assoc :username nil :password nil) str))
  (println "CouchDB server info:" (c/couchdb-info (-> test-host utils/url str)))
  (is (= "Welcome" (:couchdb (c/couchdb-info (test-database-url))))))

(deftest get-list-check-and-delete-database
  (let [name "paddleguru_test_db"
        url (test-database-url name)
        *test-database* (c/get-database url)]
    (is (= name (:db_name *test-database*)))
    (is ((set (c/all-databases url)) name))
    (is (= name (:db_name (c/database-info url))))
    (is (:ok (c/delete-database url)))
    (is (nil? ((set (c/all-databases url)) name)))))

(defdbtest view-existence-test
  {:design-docs "regatta"}
  (is (= "regatta" (:name (db/design-doc-info "regatta")))
      "The design doc specified in the metadata should be created.")

  (is (nil? (db/design-doc-info "unwritten_doc"))
      "design-doc-info returns nothing for a missing design doc."))

(defdbtest investigation-test
  "Some basic tests of CouchDB functionality."
  (let [doc       (db/create! {:foo "bar" :type "honey"})
        updated   (db/update! doc {:baz "cake"})
        ts-doc    (db/create! {:foo "bar" :type "honey" :created-at "today"})
        doc2      (db/create! {:fun "salad" :type "badger"})
        doc3      (db/create! {:acey "ducey"})
        nil-types (db/get-nil-types)]
    ;; CRU(D) operations.
    (is (= "bar" (:foo doc))
        "Creating a doc works!")

    (is (= "today" (:created-at ts-doc))
        "You can override :created-at to create a custom timestamp.")

    (is (and (= doc2 (db/get (db/get-id doc2)))
             (= updated (db/get (db/get-id updated))))
        "db/create! and db/update! return the created or updated
        document. Fetching with db/get! returns the exact same
        document. (So, when you create something, you don't need to go
        get it afterward.)")

    (is (= {:foo "bar"
            :baz "cake"}
           (select-keys updated [:foo :baz])))
    (is (= #{:_id :_rev :created-at :updated-at :type :foo} (set (keys doc)))
        "Creating a document adds a timestamp, plus some CouchDB fields.")
    (is (= #{:_id :_rev :created-at :updated-at :type :baz :foo}
           (set (keys updated)))
        "Updating a doc adds an :updated-at key.")

    ;; Investigations
    (is (= {nil 1, "honey" 2, "badger" 1} (db/count-all-types)))
    (is (and (= 1 (count nil-types))
             (= doc3 (:doc (first nil-types))))
        "get-nil-types returns a seq of documents")

    (let [id-seq ["fuck" (db/get-id doc2) (db/get-id updated)]
          zipped (db/zip (comp upper-case :type) id-seq)]
      (is (= {(db/get-id doc2) "BADGER"
              (db/get-id updated) "HONEY"}
             zipped)
          "Zipping ignores missing documents and returns a map of
          present doc ID -> function return."))

    (db/delete! updated)
    (db/delete! doc3)

    (is (= {"badger" 1 "honey" 1} (db/count-all-types))
        "Nuking docs removes them from the frequency counts.")))
