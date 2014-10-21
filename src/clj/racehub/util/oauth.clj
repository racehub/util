(ns racehub.util.oauth
  "Helpers for OAuth registration."
  (:require [cheshire.core :refer [parse-string]]
            [compojure.core :refer [ANY routes context]]
            [crypto.random :as random]
            [org.httpkit.client :as http]
            [racehub.util.liberator :as l]
            [racehub.util.ring :as ring]
            [ring.util.codec :as ring-codec]
            [ring.util.response :as response]
            [schema.core :as s]))

;; ## Schema

(s/defschema CallBack
  {:path s/Str
   :domain s/Str})

(s/defschema TokenLocation
  (s/enum :params :body))

(def OAuthProvider s/Keyword)

(s/defschema OAuthConfig
  {:token-location TokenLocation
   :auth-url s/Str
   :token-url s/Str
   :client-id s/Str
   :client-secret s/Str
   (s/optional-key :auth-query) {s/Keyword s/Str}})

(s/defschema URIConfig
  {:token-location TokenLocation
   :authentication-uri {:url s/Str
                        :query {s/Keyword s/Str
                                :client_id s/Str
                                :redirect_uri s/Str}}
   :access-token-uri {:url s/Str
                      :query {:client_id s/Str
                              :client_secret s/Str
                              :redirect_uri s/Str}}})

;; ## Code

(defn replace-authz-code
  "Formats the token uri with the authorization code"
  [{:keys [query]} code]
  (assoc-in query [:code] code))

;; http://tools.ietf.org/html/draft-ietf-oauth-v2-31#section-5.1
(defn extract-access-token
  "Returns the access token from a JSON response body"
  [{body :body}]
  (-> body (parse-string true) :access_token))

(defn get-access-token-from-params
  "Alternate function to allow retrieve
   access_token when passed in as form params."
  [{body :body}]
  (-> body ring-codec/form-decode (get "access_token")))

(defn format-config-uri
  "Formats URI from domain and path pairs in a map"
  [{{:keys [domain path]} :callback}]
  (str domain path))

(defn format-authn-uri
  "Formats the client authentication uri"
  [{{:keys [query url]} :authentication-uri} anti-forgery-token]
  (->> (assoc query :state anti-forgery-token)
       ring-codec/form-encode
       (str url "?")))

(s/defn uri-config :- URIConfig
  "Builds an OAuth config suitable for use with the friend oauth
  middleware."
  [{:keys [client-id client-secret auth-url token-url auth-query token-location]
    :as conf} :- OAuthConfig]
  (let [formatted (format-config-uri conf)]
    {:token-location token-location
     :authentication-uri {:url auth-url
                          :query (merge auth-query
                                        {:client_id client-id
                                         :redirect_uri formatted})}

     :access-token-uri {:url token-url
                        :query {:client_id client-id
                                :client_secret client-secret
                                :redirect_uri formatted}}}))

(s/defn callback :- CallBack
  [domain :- s/Str provider :- s/Keyword]
  {:path (format "/oauth/%s/callback" (name provider))
   :domain domain})

(s/defn get-config :- (s/maybe URIConfig)
  "Returns the URI config associated with the supplied provider, if
  said provider exists."
  [config :- {s/Keyword OAuthConfig}
   domain :- s/Str
   provider :- s/Keyword]
  (if-let [m (config provider)]
    (-> m
        (assoc :callback (callback domain provider))
        (uri-config))))

;; ## Anti-Forgery Token

(s/defn generate-anti-forgery-token :- s/Str
  "Generates random string for anti-forgery-token."
  []
  (random/url-part 60))

(defn add-anti-forgery [m token]
  (assoc m ::state token))

(defn get-anti-forgery [m]
  (-> m ::state))

(defn remove-anti-forgery [m]
  (dissoc m ::state))

;; ## Handshake Resource

(s/defn append-redirect
  "Takes a ring response and registers a redirect key into the user's
  session. When the callback comes back, the user will get redirected
  to this key."
  [response uri :- s/Str]
  (if uri
    (assoc-in response [:session ::redirect-uri] uri)
    response))

(s/defn redirect-after-auth
  "If the user's session contains a redirect URI, returns a response
  that redirects the user to that original URI and destroys the link
  in the session. If not, sends the user to the supplied default URI.

  Returns a ring response."
  [request default :- s/Str]
  (let [uri (-> request :session (::redirect-uri default))
        response (ring/transfer-session (response/redirect uri) request)]
    (update response :session dissoc ::redirect-uri)))

(defn redirect-to-provider!
  "Redirects user to OAuth2 provider. Code should be in response."
  [uri-config request]
  (let [anti-forgery-token (generate-anti-forgery-token)
        session-with-af-token (add-anti-forgery (:session request)
                                                anti-forgery-token)]
    (-> uri-config
        (format-authn-uri anti-forgery-token)
        response/redirect
        (assoc :session session-with-af-token))))

(s/defn oauth-base
  [base :- {s/Keyword s/Any}
   config :- {OAuthProvider OAuthConfig}
   provider :- OAuthProvider]
  {:base base
   :exists?
   (fn [{req :request}]
     (let [domain (:server-name req)]
       (if-let [config (get-config config domain provider)]
         {::config config})))})

(l/defresource handshake [base]
  :base base
  :allowed-methods [:get]
  :available-media-types ["text/html"]
  :handle-ok (fn [{req :request config ::config}]
               ;; Switch in here. If they already have a token for the
               ;; provider, check if it's still valid. If so, then
               ;; just say you're already authenticated. Otherwise
               ;; kill it and redirect.
               (-> (redirect-to-provider! config req)
                   (append-redirect (ring/referrer req))
                   (l/ring-response))))

;; ## Token Requests

(s/defn request-token
  "POSTs request to OAauth2 provider for authorization token."
  [config :- OAuthConfig code :- s/Str]
  (let [token-location (:token-location config)
        access-token-uri (:access-token-uri config)
        query-map (merge {:grant_type "authorization_code"}
                         (replace-authz-code access-token-uri code))
        token-url (assoc access-token-uri :query query-map)
        token-response @(http/post (:url token-url)
                                   {:form-params (:query token-url)
                                    :throw-entire-message? true})]
    (if (= :params token-location)
      (get-access-token-from-params token-response)
      (extract-access-token token-response))))

;; Resource that manages OAuth token fetching from providers.

(l/defresource token [base provider register-fn]
  :base base
  :allowed-methods [:get]
  :available-media-types ["text/html"]
  :handle-ok (let [config (get-config provider)]
               (fn [{req :request}]
                 (let [{:keys [state code]} (:params req)
                       session-state (-> req :session get-anti-forgery)
                       response (l/ring-response
                                 (redirect-after-auth req "/settings/profile"))]
                   (if (and code (= state session-state))
                     (let [access-token (request-token config code)]
                       (register-fn provider access-token)
                       response)
                     ;; Redirect back home and note that an exception
                     ;; occurred with login. We need to properly
                     ;; handle the failed auth case.
                     response)))))

(l/defresource unlink [base provider unregister-fn]
  :base base
  :allowed-methods [:get]
  :available-media-types ["text/html"]
  :handle-ok (fn [{req :request}]
               (unregister-fn provider)
               (l/ring-response
                (response/redirect (ring/referrer req)))))

(def OAuthEndpoint
  {:config {OAuthProvider OAuthConfig}
   :endpoint-base (-> {s/Keyword s/Any}
                      (s/named "Base for the resources."))
   :register-fn (-> (s/=> s/Any s/Keyword s/Str)
                    (s/named "Function of provider name and access
                  token. Should store the token in the database."))
   :unregister-fn (-> (s/=> s/Any s/Str)
                      (s/named "Function of provider name. Performs the
                  side effect of killing the provider."))})

(s/defn oauth-api
  "Returns a bundled set of routes for oauth interaction."
  [endpoint :- OAuthEndpoint]
  (let [{:keys [register-fn unregister-fn config endpoint-base]} endpoint]
    (context "/oauth/:provider" [provider]
             (let [provider (keyword provider)
                   base (oauth-base (or endpoint-base {}) config provider)]
               (routes
                (ANY "/" [] (handshake base))
                (ANY "/callback" [] (token base provider register-fn))
                (ANY "/unlink" [] (unlink base provider unregister-fn)))))))
