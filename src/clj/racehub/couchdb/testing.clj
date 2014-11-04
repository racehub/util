(ns racehub.couchdb.testing
  (:require [clojure.java.shell :as shell]
            [clojure.string :as s]
            [clojure.test :refer [deftest]]
            [com.ashafa.clutch :as c]
            [racehub.couchdb :as db]
            [racehub.util :as u]
            [racehub.util.config :as conf]
            [schema.core :as sm]
            [taoensso.timbre :as log]
            [clojure.tools.macro :refer [name-with-attributes]])
  (:import [clojure.lang Symbol]
           [java.lang.reflect Array]))

;; ## Schemas

(sm/defn array-of :- Class
  "Returns an array instance of the specified class."
  [t :- Class]
  (.getClass (Array/newInstance t 0)))

(def ExitCode
  (sm/named sm/Int "sub-process's exit code"))

(def StdOut
  (-> (sm/either sm/Str (array-of Byte))
      (sm/named "sub-process's stdout")))

(def StdErr
  (sm/named sm/Str "sub-process's stderr"))

(def Shell
  (-> {:exit ExitCode
       :out StdOut
       :err StdErr}
      (sm/named "Return of a clojure.shell/sh call.")))

(def ViewName (sm/named String "CouchDB view name"))
(def Database
  {:uri String
   :database String})

;; ## Shell Functions

(sm/defn sh :- Shell
  [& args]
  (log/debug "Running command: " (s/join " " args))
  (apply shell/sh args))

(sm/defn success? :- Boolean
  "Returns true if the supplied command returned with a successful
  code, false otherwise."
  [command :- Shell]
  (zero? (:exit command)))

(sm/defn command-exists? :- Boolean
  "returns true if the supplied shell command exists on the classpath,
  false otherwise."
  [command :- String]
  (success?
   (sh "command" "-v" command)))

(sm/defn ensure-success :- Shell
  [command :- Shell]
  (assert (success? command) (str "Command failed: " command))
  command)

(sm/defn erica-exists? :- Boolean
  []
  (command-exists? "./erica"))

(defn erica-command
  "Returns the command string for erica."
  []
  (-> (sh "pwd")
      :out
      (s/trim)
      (str "/erica")))

(defn view-directory []
  (conf/env :couch-view-directory "couchviews"))

(sm/defn view-path :- String
  [view-name :- ViewName]
  (when-let [view-dir (view-directory)]
    (str view-dir "/" view-name)))

(sm/defn view-exists? :- Boolean
  "Returns true if the supplied view name exists on the filesystem,
  false otherwise."
  [view-name :- ViewName]
  (when-let [p (view-path view-name)]
    (success?
     (sh "test" "-d" p))))

(sm/defn load-view! :- Shell
  [db :- Database view-name :- ViewName]
  {:pre [(view-exists? view-name)]}
  (let [{:keys [uri database]} db
        db-string (str uri "/" database)]
    (let [erica (erica-command)]
      (sh erica "push" "--is-ddoc" db-string
          :dir (view-path view-name)))))

;; ## Database Functions

(def test-host
  "Returns the currently configured local testing URI for
  CouchDB (with username and password included for admin goodness)"
  (:uri (db/couchdb-config)))

(def test-database-url
  "Takes a database name, returns a Chas Emerick URL."
  (partial db/database-url test-host))

(declare ^{:dynamic true} *test-database*)
(declare ^{:dynamic true} *database-map*)

(sm/defn test-database-name :- String
  [test-name :- (sm/either String Symbol)]
  (str "test-db-" (s/replace (str test-name) #"[^$]+\$([^@]+)@.*" "$1")))

(sm/defn test-database :- Database
  [db-name :- String]
  {:uri test-host
   :database db-name})

(defmacro defdbtest
  "Defines a test with a custom database bound to *test-database* (and
  subbed in to the global atom in couchdb.clj) for the duration of the
  body. Also, a map containing :uri and :database are bound to
  *database-map*, in case you need those bits.

  Optionally, you can supply a :design-docs tag in the metadata; if
  supplied, defdbtest will load those views from the paddleguru repo
  into the test database before running any tests."
  [name & body]
  (let [[name body]    (name-with-attributes name body)
        required-views (-> name meta :design-docs u/collectify)]
    `(deftest ~name
       (let [db-name# (test-database-name ~(clojure.core/name name))]
         (binding [*database-map* (test-database db-name#)
                   *test-database* (c/get-database
                                    (test-database-url db-name#))]
           (let [current-db# @db/my-db]
             (try (db/reset-db! *test-database*)
                  (c/with-db *test-database*
                    (doseq [v# ~required-views]
                      (ensure-success
                       (load-view! *database-map* v#)))
                    ~@body)
                  (finally
                    (db/reset-db! current-db#)
                    (c/delete-database *test-database*)))))))))
