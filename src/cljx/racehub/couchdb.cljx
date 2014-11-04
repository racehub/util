(ns racehub.couchdb
  "CouchDB integration."
  #+clj (:refer-clojure :exclude (get))
  (:require [racehub.schema :as ps]
            [schema.core :as s :include-macros true])
  #+clj
  (:require [clojure.string :as string]
            [com.ashafa.clutch :as c]
            [com.ashafa.clutch.utils :as utils]
            [com.ashafa.clutch.http-client :as http]
            [racehub.util.config :as conf]
            [racehub.util.log :as log]
            [racehub.util.time :as time]
            [taoensso.timbre :refer [error]]))

;; ## Schemas

(s/defschema Stub
  "CouchDB attachment stub."
  {s/Any s/Any})

(def ID s/Str)

(s/defn id [schema-name :- s/Str]
  s/Str)

(def CouchFields
  {:_id ID
   :_rev s/Str
   (s/optional-key :created-at) ps/Timestamp
   (s/optional-key :updated-at) ps/Timestamp
   (s/optional-key :*) (s/named s/Str "Kill this!")
   (s/optional-key :_conflicts) (s/named [s/Str] "Array of revision IDs")
   (s/optional-key :_deleted_conflicts) (s/named [s/Str] "Array of revision IDs")
   (s/optional-key :_attachments) {s/Keyword Stub}})

(defn couchify [m type]
  (-> (merge CouchFields m)
      (assoc :type (s/eq type))))

(defn uncouchify
  [m]
  (dissoc m :_id :_rev :created-at :updated-at :* :_conflicts :_deleted_conflicts
          :_attachments :type))

(def CouchDoc
  (-> CouchFields
      (assoc (s/either s/Keyword s/Str) s/Any)
      (s/named "CouchDB Document.")))

(def UpdateFailure
  {:id ID
   :error s/Str
   :reason s/Str})

(def UpdateSuccess
  {(s/optional-key :ok) (s/named boolean "Doesn't show up on Cloudant, only locally.")
   :id ID
   :rev s/Str})

(def UpdateResult
  (s/either UpdateFailure UpdateSuccess))

(def ViewName
  (s/named s/Str "Name of a view in a CouchDB design doc."))

(def ViewEntry
  {(s/optional-key :id) ID
   :key s/Any
   :value s/Any
   :doc CouchDoc})

(def DesignDoc
  (s/named s/Str "Design document name."))

(def SlimView
  "Schema for a CouchDB view result with :include-docs set to false."
  [(dissoc ViewEntry :doc)])

(def FatView
  "Schema for a CouchDB view result with :include-docs set to true."
  [ViewEntry])

(def View
  (s/either SlimView FatView))

(def CasedViewName
  (s/both s/Str (s/pred (fn [^String s] (.startsWith s "downcased-")))))

#+clj
(do
  (defonce my-db (atom nil))

  ;; ## CouchDB Schema Factories

  ;; TODO: Destroy this when we release a new clutch and make this
  ;; method public.
  (defmacro defdbop
    "Same as defn, but wraps the defined function in another that transparently
   allows for dynamic or explicit application of database configuration as well
   as implicit coercion of the first `db` argument to a URL instance."
    [name & body]
    `(do (let [ret# (defn ~name ~@body)]
           (alter-var-root (var ~name) @#'c/with-db*)
           (alter-meta!
            (var ~name)
            update-in [:doc] str
            "\n\n  When used within the dynamic scope of `with-db`, the initial `db`"
            "\n  argument is automatically provided.")
           ret#)))

  (defdbop design-doc-info
    [db design-document]
    (let [url (apply utils/url db ["_design" design-document "_info"])]
      (http/couchdb-request :get url)))

  (defn reset-db! [db]
    (reset! my-db db))

  (defn database-url
    ([host]
       (database-url host nil))
    ([host db-name]
       (-> (utils/url host)
           (utils/url db-name))))

  (defn couch-connect [host db-name]
    (try (c/get-database (database-url host db-name))
         (catch Exception e
           (error e "on CouchDB connection:" (.getMessage e)))))

  (defn view-suffix [] (conf/env :couch-view-suffix ""))

  (defn couchdb-config
    "Map of CouchDB settings."
    []
    {:uri (conf/env :couch-url "http://localhost:5984")
     :database (conf/env :database-name)
     :view-suffix (view-suffix)})

  (defn setup-couchdb!
    ([] (setup-couchdb! (couchdb-config)))
    ([conf]
       (let [{:keys [uri database]} conf]
         (reset-db! (couch-connect uri database)))))

  ;;opts will be a map ie {:group true :key some-id}
  (s/defn get-view :- View
    [design-name :- DesignDoc
     map-name :- ViewName
     & [query-params-map post-data-map]]
    ;;we need to encode the slash in key so the couchdb path doesnt get
    ;;messed up
    (let [design-name (str design-name (view-suffix))]
      (log/debug "couchdb-get-view" {:design-doc design-name
                                     :view-name map-name})
      (c/get-view @my-db design-name map-name query-params-map post-data-map)))

  (defn get-all
    "Get all objects from CouchDB."
    [& {:keys [include-docs]
        :or {include-docs true}}]
    (-> @my-db
        (c/all-documents {:include_docs (boolean include-docs)})))

  (s/defn get-id :- (s/maybe ID)
    [doc :- (s/either ID CouchDoc)]
    (if (map? doc)
      (:_id doc)
      doc))

  (s/defn get :- (s/maybe CouchDoc)
    "Query CouchDB for the supplied key."
    [id :- ID]
    (log/debug "couchdb-get" {:document-id id})
    (c/get-document @my-db id))

  (s/defn resolve-match
    "Accepts the result of a get-view with include_docs set to true and
  some lookup key. If the matches only has only entry, returns the
  corresponding document. Otherwise, returns the entry where the VALUE
  equals the input key.

  It's the value because the backing CouchDB view had to group by the
  lowercased version of the input key. So, the lowercased version is
  the :key, the original is the :value."
    ([matches :- View
      k :- s/Str]
       (resolve-match matches k :doc))
    ([matches :- View
      k :- s/Str
      result-key :- (s/enum :key :value :id :doc)]
       (if (= 1 (count matches))
         (result-key (first matches))
         (some (fn [m]
                 (when (= k (:value m))
                   (result-key m)))
               matches))))

  (s/defn cased-get :- (s/maybe CouchDoc)
    [design-name :- DesignDoc
     view-name :- CasedViewName
     k :- s/Str]
    (let [matches (get-view design-name
                            view-name
                            {:key (string/lower-case k)
                             :include_docs true})]
      (resolve-match matches k)))

  (s/defn mark-creation :- {:created-at ps/Timestamp
                            s/Any s/Any}
    [document :- {s/Any s/Any}]
    (-> document
        (update-in [:created-at] (fn [t] (or t (time/timestamp))))))

  (s/defn mark-update :- {:updated-at ps/Timestamp
                          s/Any s/Any}
    [document :- {s/Any s/Any}]
    (assoc document :updated-at (time/timestamp)))

  (s/defn put! :- CouchDoc
    "Accepts a CouchDB document (with a valid revision and id) and
  updates the document in the database WITHOUT merging in existing kv
  pairs."
    [old-doc & opts]
    (apply c/put-document @my-db (mark-update old-doc)
           opts))

  (defn create!
    "Add the supplied object to the database; no validations are
  necessary. Call with any of the following:
   - map of data
   - data and an id (as :_id)
   - data, vector of attachments
   - data, id, vector of attachments"
    [document & opts]
    (let [metadoc (mark-creation document)]
      (apply put! metadoc opts)))

  (defn delete!
    "Takes a document and deletes it from the database."
    [document]
    (c/delete-document @my-db document))

  (defn update!
    "Takes any of the following sets of arguments:

   - A single document
   - A document and another map to merge into it

  update! acts on the CouchDB document with the key of the first map
  provided."
    [old-doc new-doc]
    (c/update-document @my-db old-doc
                       (assoc new-doc :updated-at (time/timestamp))))

  ;;ATTACHMENTS

  (defn get-attachment
    "Gets the attchment from the document in the db"
    [document file]
    (c/get-attachment @my-db document file))

  (defn add-attachment!
    "Add the attachment to the document database; no validations are
  necessary. Call with the following:
   - regatta document
   - file for attachment ie 'resources/public/img/guru.png')
   - optional :filename filename :mime-type 'image/jpeg' "
    [document file & opts]
    (let [new-doc (update! document {})]
      (apply c/put-attachment @my-db new-doc file opts)))

  (defn delete-attachment!
    "Deletes the attachment from the document in the database; Call with the following:
   - regatta document
   - file for attachment ie 'logo.png')"
    [document file]
    (let [new-doc (update-in document [:_attachments] dissoc file)]
      (update! document new-doc)))

  ;;BULK-DOCUMENT FUNCTIONS
  (defn get-bulk
    "Takes in a collection of couchdb ids and fetches the corresponding
  documents in one request"
    [ids]
    (log/debug "couchdb-get-bulk" {:ids ids})
    (if (not-empty ids)
      (c/all-documents @my-db
                       {:include_docs true}
                       {:keys (seq ids)})
      []))

  (s/defn bulk-update :- [UpdateResult]
    "Takes in a collection of maps; if a doc contains :_id and :_rev,
  that doc gets updated. If not, a new doc's created."
    [docs]
    (let [docs-w-created (map (fn [doc]
                                (-> doc
                                    (mark-creation)
                                    (mark-update)))
                              docs)]
      (c/bulk-update @my-db docs-w-created)))

  (s/defn mark-for-deletion :- CouchDoc
    [doc :- CouchDoc]
    (assoc doc :_deleted true))

  (defn delete-bulk!
    "Takes in a collection of documents, each of which must have
  and :_id and :_rev field, and deletes them from couchdb in one
  request"
    [docs]
    (c/bulk-update @my-db (map mark-for-deletion docs)))

  ;; ## Transformations

  (s/defn zip :- {ID s/Any}
    [f :- (s/either s/Keyword (s/=> s/Any CouchDoc))
     ids :- (s/either [ID] #{ID})]
    (->> (get-bulk (if (set? ids)
                     (seq ids)
                     (distinct ids)))
         (remove :error)
         (map (comp (juxt get-id f) :doc))
         (into {})))

  ;; ## Database Investigation
  ;;
  ;; Let's face it, we rewrite these in the repl every damned day.

  (s/defn count-type :- {(s/maybe s/Str) s/Int}
    "Returns a frequency map of doc types."
    [doc-seq :- FatView]
    (frequencies (map (comp :type :doc) doc-seq)))

  (s/defn count-all-types :- {(s/maybe s/Str) s/Int}
    []
    (count-type
     (get-all :include-docs true)))

  (s/defn get-nil-types :- FatView
    []
    (filter (comp nil? :type :doc)
            (get-all :include-docs true))))
