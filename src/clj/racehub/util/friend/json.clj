(ns racehub.util.friend.json
  "JSON authentication workflow for Friend."
  (:require [cemerick.friend :as friend]
            [cemerick.friend.workflows :as workflows]
            [racehub.util.liberator :as l]))

;; ## Notes for Improvement
;;
;; To really improve this flow, we need to adopt a liberator-style
;; switching on content type. This is really the same as the
;; interactive login form, and not really doing a great job of
;; respecting the proper liberator decision tree that would occur
;; above authentication. (For example, checking if the media type is
;; even supported, and properly negotiating content type and accepted
;; returns.)

;; ## Handlers
;;
;; There are three cases - login failed, already logged in, and
;; successful login.

(def available-media-types
  ["text/html" "application/json"])

(defn login-failed
  "called when JSON authentication fails."
  []
  (-> {:ok false
       :reason "authentication failed"}
      (l/json-response 401)))

(defn already-logged-in []
  (l/json-response
   {:ok true
    :reason "User already authenticated."}))

(defn login-succeeded
  "called when JSON authentication succeeds."
  [request]
  (let [sesh (:session request)]
    (-> {:ok true
         :reason
         "Authentication succeeded! Save the cookie returned in the cookie header."}
        (l/json-response 201)
        (assoc :session sesh))))

;; ## Workflow

(defn json-login-route?
  [{:keys [uri request-method] :as req}]
  (and (= uri (:login-uri (::friend/auth-config req)))
       (= :post request-method)
       (= "application/json" (l/content-type req))))

;; ## Public Methods

(defn extract-creds [req]
  (let [creds (:json-params req)]
    {:username (creds "username")
     :password (creds "password")}))

(defn should-redirect? [req]
  (= "text/html" (l/get-media req available-media-types)))

(defn json-login
  "json auth workflow implementation for friend. It's required that
  you're using wrap-json-params if you decide to use this bad boy."
  [req]
  (when (json-login-route? req)
    (let [{:keys [username password] :as creds} (extract-creds req)
          creds (with-meta creds {::friend/workflow :json-login})
          cred-fn (:credential-fn (::friend/auth-config req))]
      (if (boolean (friend/identity req))
        (already-logged-in)
        (if-let [user-record (and username password (cred-fn creds))]
          (workflows/make-auth user-record
                               {::friend/workflow :json-login
                                ::friend/redirect-on-auth?
                                (should-redirect? req)})
          (login-failed))))))
