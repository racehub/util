(ns racehub.util.friend.json
  "JSON authentication workflow for Friend."
  (:require [cemerick.friend :as friend]
            [cemerick.friend.workflows :as workflows]
            [racehub.util.liberator :as l]
            [schema.core :as s]))

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
  ([] (login-failed nil))
  ([reason]
     (-> {:ok false :reason (or reason "authentication failed")}
         (l/json-response 401))))

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

(s/defschema Credentials
  "Credentials supplied by the user for json login."
  {:username s/Str
   :password s/Str})

(s/defschema AuthReturn
  "Return value of the supplied authorization function. Useful if you
  want to provide a custom failure message."
  (s/either {:ok (s/pred true?)
             :user s/Any}
            {:ok (s/pred false?)
             (s/optional-key :reason) s/Str}))

(s/defn extract-creds :- Credentials
  [req]
  (let [creds (:json-params req)]
    {:username (creds "username")
     :password (creds "password")}))

(defn should-redirect? [req]
  (= "text/html" (l/get-media req available-media-types)))

(s/defn json-login
  "json auth workflow implementation for friend. It's required that
  you're using wrap-json-params if you decide to use this bad boy.

  Accepts a function of credentials and the ring request. The function
  must return an instance of AuthReturn.

  If "
  ([] (json-login (fn [creds req]
                    (let [f (:credential-fn (::friend/auth-config req))]
                      (if-let [user (f creds)]
                        {:ok true :user user}
                        {:ok false})))))
  ([cred-fn :- (s/=> AuthReturn Credentials {s/Any s/Any})]
     (fn [req]
       (when (json-login-route? req)
         (let [{:keys [username password] :as creds} (extract-creds req)
               creds (with-meta creds {::friend/workflow :json-login})]
           (if (boolean (friend/identity req))
             (already-logged-in)
             (if-let [{:keys [ok user reason]}
                      (and username password (cred-fn creds req))]
               (if ok
                 (workflows/make-auth user
                                      {::friend/workflow :json-login
                                       ::friend/redirect-on-auth?
                                       (should-redirect? req)})
                 (login-failed reason))
               (login-failed))))))))
