(ns racehub.util.facebook
  "Facebook API helpers."
  (:require [clojure.string :as str]
            [cheshire.core :refer [parse-string generate-string]]
            [org.httpkit.client :as http]
            [racehub.util.config :as c]
            [racehub.util.oauth :as oauth]
            [racehub.util.ring :as ring]
            [schema.core :as s]))

;; This namespace requires FB_APP_SECRET, FB_CLIENT_ID, and
;; FB_CLIENT_SECRET environment variables.

;; ## Schema

(s/defschema APIError
  "Error response from the Facebook API."
  {:error {:message s/Str :type s/Str :code s/Int}})

(s/defschema Paging
  "Not really sure how this is used."
  {:cursors {:before s/Str
             :after s/Str}})

(s/defschema FacebookConfig
  {:oauth oauth/OAuthConfig
   :app-secret s/Str})

;; ## Security

(def api-version "2.2")
(def root (str "https://graph.facebook.com/v" api-version "/"))

(s/defn facebook-config :- FacebookConfig
  "Token location specifies that the token is going to come back in
  the params, not the body. We also make sure to ask for email
  privileges, to beef up a particular user's profile.

  The returned map contains oauth information AND the app-specific
  secret key for facebook."
  []
  {:app-secret (c/env :fb-app-secret)
   :oauth {:token-location :params
           :auth-url "https://www.facebook.com/dialog/oauth"
           :token-url "https://graph.facebook.com/oauth/access_token"
           :auth-query {:scope "email"
                        :response_type "code"}
           :client-id (c/env :fb-client-id)
           :client-secret (c/env :fb-client-secret)}})

(def app-id (comp :client-id :oauth facebook-config))
(def client-secret (comp :client-secret :oauth facebook-config))
(def server-secret (comp :app-secret facebook-config))

(s/defn body
  "returns a json-parsed representation of the body of the response."
  [req]
  (parse-string (:body @req) keyword))

(s/defn access-token :- (s/maybe s/Str)
  "Generates a server-side access token. This is used to make calls
  about the app to the server.

  More info:
  https://developers.facebook.com/docs/facebook-login/access-tokens"
  []
  (let [params {:client_id (app-id)
                :client_secret (client-secret)
                :grant_type "client_credentials"}]
    (ring/get-access-token-from-params
     @(http/get (str root "oauth/access_token")
                {:query-params params}))))

(s/defn api-get :- {s/Any s/Any}
  "Makes a call to Facebook's /user/id endpoint for the user linked to
  the supplied token. Gives you a map with a bunch of information
  about the user.

  See the documentation for more details on what the map holds:
  https://developers.facebook.com/docs/graph-api/reference/v2.1/user"
  [endpoint :- s/Str
   {:keys [query-params params]}
   :- {:query-params {:token s/Str, s/Any s/Any}
       :params {s/Any s/Any}}]
  (let [opts (assoc params :query-params query-params)]
    (body (http/get (str root endpoint) opts))))

;; ## Api Calls

;; ### Token Exchange
;;
;; This section deals with exchanging short-lived client tokens for
;;long-lived server tokens.

(s/defn exchange-token :- (s/either APIError {:token s/Str})
  [token :- s/Str]
  (let [{:keys [client-id client-secret]} (:oauth (facebook-config))
        resp @(http/get (str root "oauth/access_token")
                        {:query-params
                         {:grant_type "fb_exchange_token"
                          :fb_exchange_token token
                          :client_id client-id
                          :client_secret client-secret}})]
    (or (when-let [token (oauth/get-access-token-from-params resp)]
          {:token token})
        (parse-string (:body resp) keyword))))

;; ### Profile Info

(s/defschema UserData
  "Response from Facebook's me endpoint."
  {:email s/Str
   :first_name s/Str
   :timezone (s/named s/Int "UTC offset.")
   :locale s/Str
   :name s/Str
   :updated_time s/Str
   :link s/Str
   :id s/Str
   :last_name s/Str
   :gender s/Str
   :verified s/Bool
   s/Any s/Any})

(s/defn me :- UserData
  "Makes a call to Facebook's /user/id endpoint for the user linked to
  the supplied token. Gives you a map with a bunch of information
  about the user.

  Optionall you can give a list of fields

  See the documentation for more details on what the map holds:
  https://developers.facebook.com/docs/graph-api/reference/v2.1/user"
  ([token :- s/Str]
     (me token nil))
  ([token :- s/Str fields :- [s/Str]]
     (api-get "me" {:query-params {:access_token token
                                   :fields (str/join "," fields)}})))

(s/defschema CoverPhoto
  {:id s/Str
   (s/optional-key :offset_y) s/Int
   (s/optional-key :source) s/Str})

(s/defschema ProfilePhoto
  {:is_silhouette s/Bool
   :url s/Str
   (s/optional-key :width) s/Int
   (s/optional-key :height) s/Int})

(s/defn cover-photo :- CoverPhoto
  "Returns information for the supplied user's cover photo. If the
  user's cover photo isn't set, only the :id is present."
  [token :- s/Str]
  (:cover (me token ["cover"])))

(s/defn profile-photo :- (s/maybe ProfilePhoto)
  "If the supplied user exists, returns information for the supplied
  user's cover photo. If the user's cover photo isn't set, only
  the :id is present.

  The actual API can take width and height parameters. These aren't
  implemented yet."
  [facebook-id :- s/Str]
  (:data
   (api-get (str facebook-id "/picture")
            {:query-params {:type "large" :redirect false}
             :params {:oauth-token (server-secret)}})))

(comment
  "Example of a batch request."
  (s/defn photos
    [facebook-id]
    (body
     (http/post "https://graph.facebook.com"
                {:query-params
                 {:batch (generate-string
                          [{:method "GET" :relative_url (str facebook-id "?fields=cover")}
                           {:method "GET" :relative_url (str facebook-id "/picture?type=large&redirect=false")}])}
                 :oauth-token (server-secret)}))))

;; ## Test Users

(s/defschema TestUser
  "Fields for the test user endpoint."
  {:id s/Str
   :login_url s/Str
   :access_token s/Str})

(s/defn test-users :- {:data [TestUser], :paging Paging}
  "Queries the /accounts/test-users endpoint:
  https://developers.facebook.com/docs/graph-api/reference/v2.1/app/accounts/test-users"
  []
  (api-get (str (app-id) "/accounts/test-users")
           {:params {:oauth-token (server-secret)}}))
