(ns racehub.util.facebook
  "Facebook API helpers."
  (:require [cheshire.core :refer [parse-string]]
            [org.httpkit.client :as http]
            [racehub.util.config :as c]
            [racehub.util.oauth :as oauth]
            [racehub.util.ring :as ring]
            [schema.core :as s]))

;; This namespace requires FB_APP_SECRET, FB_CLIENT_ID, and
;; FB_CLIENT_SECRET environment variables.

;; ## Schema

(s/defschema Paging
  "Not really sure how this is used."
  {:cursors {:before s/Str
             :after s/Str}})

(s/defschema FacebookConfig
  {:oauth oauth/OAuthConfig
   :app-secret s/Str})

;; ## Security

(def api-version "2.1")
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

;; ## Api Calls

(s/defn me :- {s/Any s/Any}
  "Makes a call to Facebook's /user/id endpoint for the user linked to
  the supplied token. Gives you a map with a bunch of information
  about the user.

  See the documentation for more details on what the map holds:
  https://developers.facebook.com/docs/graph-api/reference/v2.1/user"
  [token :- s/Str]
  (let [opts {:query-params {"access_token" token}}]
    (body (http/get (str root "me") opts))))

(s/defschema TestUser
  "Fields for the test user endpoint."
  {:id s/Str
   :login_url s/Str
   :access_token s/Str})

(s/defn test-users :- {:data [TestUser], :paging Paging}
  "Queries the /accounts/test-users endpoint:
  https://developers.facebook.com/docs/graph-api/reference/v2.1/app/accounts/test-users"
  []
  (body
   (http/get (str root (app-id) "/accounts/test-users")
             {:oauth-token (server-secret)})))
