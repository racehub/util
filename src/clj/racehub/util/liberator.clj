(ns racehub.util.liberator
  "Helpful extensions to liberator."
  (:require [clojure.string :refer [split join]]
            [cheshire.core :as json]
            [compojure.route :as route]
            [forms-bootstrap.core :as fb]
            [forms-bootstrap.validation :as v]
            [noir.session :as session]
            [liberator.conneg :as conneg]
            [liberator.core :as l]
            [liberator.representation :as rep]
            [racehub.util :as util]
            [racehub.util.log :as log]
            [ring.util.response :as r]
            [schema.core :as s])
  (:import [liberator.representation RingResponse]))

;; ## Basic Helpers
;;
;; These could easily make their way back into liberator.

(defn flatten-resource
  "Accepts a map (or a sequence, which gets turned into a map) of
  resources; if the map contains the key :base, the kv pairs from THAT
  map are merged in to the current map. If there are clashes, the new
  replaces the old by default.

  Combat this by supplying a :merge-with function in the map. This key
  should point to a map from keyword -> binary function; this function
  will be used to resolve clashes."
  [kvs]
  (let [m (if (map? kvs)
            kvs
            (apply hash-map kvs))
        trim #(dissoc % :base)]
    (if-let [base (:base m)]
      (let [combined (flatten-resource base)
            trimmed (trim m)]
        (if-let [merger (if (contains? m :merge-with)
                          (:merge-with m)
                          (:merge-with combined))]
          (if (fn? merger)
            (merge-with merger combined trimmed)
            (util/merge-with-map merger combined trimmed))
          (merge combined trimmed)))
      (trim m))))

(defn resource
  "Functional version of defresource. Takes any number of kv pairs,
  returns a resource function."
  [& kvs]
  (fn [request]
    (l/run-resource request
                    (flatten-resource kvs))))

(defmacro defresource
  "The same as liberator's defresource, except it allows for a base
  resource and a merge-with function."
  [name & kvs]
  (if (vector? (first kvs))
    (let [[args & kvs] kvs]
      `(defn ~name [~@args]
         (resource ~@kvs)))
    `(defn ~name [req#]
       (l/run-resource req# (flatten-resource [~@kvs])))))

(defn content-type
  "Returns ONLY the type/subtype, NO parameters."
  [req]
  (when-let [content-type (get-in req [:headers "content-type"])]
    (first (split content-type #";"))))

(defn accepted-types
  "Returns a sequence of content types accepted by the supplied
  request. If no accept header is present, returns nil."
  [req]
  (when-let [accepts-header (get-in req [:headers "accept"])]
    (->> (conneg/sorted-accept accepts-header ["*/*"])
         (map (comp conneg/stringify :type))
         (filter not-empty))))

(defn get-media
  "Pulls the media type out of the request, or parses it from the
  content headers.

  allowed-types is a set containing pairs (e.g., [\"text\" \"*\"])
  or strings (e.g., \"text/plain\").

  If no allowed-types is present, returns the type most favored by the
  client."
  ([req]
     (first (accepted-types req)))
  ([req allowed-types]
     {:pre [(contains? (:headers req) "accept")
            (sequential? allowed-types)]}
     (l/try-header "Accept"
                   (when-let [accept-header (get-in req [:headers "accept"])]
                     (let [type (conneg/best-allowed-content-type
                                 accept-header
                                 allowed-types)]
                       (not-empty (conneg/stringify type)))))))

(def ring-response rep/ring-response)
(def ringify
  (comp rep/ring-response r/response))

(defn to-response
  "to-response does more intelligent response parsing on your
  liberator ring responses.

  Liberator tries to coerce your returned value into the proper
  content type; maps get turned into json or clojure as required, etc.

  The problem with this, naively, is that ring's responses are ALSO
  just bare maps. If you return a bare map to a text/html request,
  liberator tries to coerce the map into HTML.

  The liberator solution is a special wrapper type called RingResponse
  that gets passed through without diddling. This function handles the
  most common response type cases in one spot

  If you pass in an instance of RingResponse, to-response passes it
  through untouched.

  If you pass in a ring response map, it's wrapped in an instance of
  RingResponse and passed on (and ignored by liberator).

  else, liberator tries to coerce as before."
  [t req]
  (cond (instance? RingResponse t) t
        (r/response? t) (rep/ring-response t)
        :else (rep/ring-response (rep/as-response t req))))

(defn generic
  "If you pass a response back to liberator before it's parsed the
  content type it freaks out and says that it can't dispatch on
  null. This generic method calls the proper multimethod rendering on
  json, clojure, etc, all of that business, before passing the result
  back up the chain through liberator."
  [data req media-type]
  (to-response data (assoc-in req [:representation :media-type] media-type)))

(s/defn ensure-media-type :- (s/maybe {:representation {:media-type s/Str}})
  [ctx]
  (-> ctx
      (update-in [:request :headers "accept"]
                 (fn [h] (or h "*/*")))
      (l/negotiate-media-type)))

(defn media-typed
  "Accepts a map of encoding -> handler (which can be a constant or a
  function) and returns a NEW handler that delegates properly based on
  the request's encoding. If no encoding is found, calls the handler
  under the :default key."
  [& ms]
  (let [m (apply merge ms)]
    (fn [req]
      (let [get-media #(get-in % [:representation :media-type])
            parsed-type (get-media req)
            req (merge req (ensure-media-type req))
            media-type (or parsed-type (get-media req))]
        (when-let [handler (get m media-type (:default m))]
          (if (fn? handler)
            (handler req)
            (if-not (= parsed-type media-type)
              (generic handler req media-type)
              (to-response handler req))))))))

(defn with-default
  "Duplicates the entry under the supplied media-type as the default
  in the supplied response map."
  [default-type m]
  (if-let [response (m default-type)]
    (assoc m :default response)
    m))

(comment
  "Due to the way liberator's resources merge, a fun pattern is
  something like this:"
  {:merge-with
   {:handle-not-acceptable (fn [l r]
                             (some-fn r l))}
   :handle-not-acceptable (media-typed
                           {"application/json" {:success false
                                                :message "No acceptable resource available"}})}

  "If handle-not-acceptable is called, the resource will first try all
  of the specified pairs, then the merge-with function will cause the
  system to fall back on the superclass's declared pairs. Use this to
  override certain content types, but still fall back on nicely
  defined base content types (for unauthorized resources, for
  example).

  Coding this was almost certainly overkill.")

(defn with-base
  "Merges the supplied base map into the supplied resource
  map (knocking out any existing base)"
  [base resource]
  (assoc resource :base base))

;; ## Response Helpers

(defn response
  "Accepts a function of req -> response and returns a wrapping
  function that does the proper ring checks."
   [f]
  (comp ringify f))

(defn json-response
  "Formats the supplied data as json and returns a response."
  [data & [status]]
  "return a ring json response with data serialized to json and
  status (200 by default)"
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defn with-session
  "The same as `to-response`, except that you get to supply a session
  that gets pegged onto the response."
  [resp ctx session]
  (-> (rep/as-response resp ctx)
      (assoc :session session)
      (rep/ring-response)))

;; ## Form Submission Resource

(s/defschema FormHandler
  {:post! (s/=> s/Any s/Any)
   :handle-created (s/=> s/Any s/Any)
   :handle-malformed (s/=> s/Any s/Any)})

(s/defn validate! :- s/Bool
  "General helper for validation in our apps. TODO: Move this guy into
  forms-bootstrap."
  [params validator action-name]
  (v/if-valid validator params
              (constantly true)
              (fn [form-data errors]
                (fb/move-errors-to-flash form-data errors)
                (log/info (str action-name "-failure") {:errors errors
                                                        :form-data form-data})
                false)))

(s/defn form-base :- {:malformed? (s/=> s/Any s/Any)}
  "Returns a base resource that handles form validation. If everything
  works out, you'll need to also supply the fields in FormHandler.  We
  put ::validation-error in the context -- a string used in malformed?
  to display an error in red at the top of the form. If there are
  form-level errors (ie 'Pick at least one event') then it will pick
  the first one of those. Otherwise it assumes there are only form
  field level errors (highlighted in red) and returns a generic
  message."
  [validator :- (s/=> s/Any s/Any)
   action-name :- s/Str]
  (letfn [(validate-form [ctx]
            (let [params (-> ctx :request :params)
                  post? (= :post (-> ctx :request :request-method))]
              (when post?
                (let [valid? (not (validate! params validator action-name))
                      form-errors (-> :form-data session/flash-get :_sandbar-errors :form)
                      error-str (or (not-empty (join ", " form-errors))
                                    "Please fix errors highlighted in red below!")
                      ctx (-> ctx
                              ensure-media-type
                              (assoc ::validation-error error-str))]
                  [valid? ctx]))))]
    {:malformed? validate-form}))
