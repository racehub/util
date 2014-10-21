(ns racehub.util.ring
  (:require [cemerick.friend :as friend]
            [cemerick.friend.util :as util]
            [noir.response :as response]
            [ring.util.codec :as ring-codec]
            [schema.core :as s]))

(defn get-access-token-from-params
  "Alternate function to allow retrieve
   access_token when passed in as form params."
  [{body :body}]
  (-> body ring-codec/form-decode (get "access_token")))

(defn referrer
  "Returns the referring page of the current request, or the root if
  the referer's not present."
  [req]
  (-> (:headers req)
      (get "referer")
      (or "/")))

(s/defschema RedirectType
  (s/enum :permanent     ;; -- A 301 permanent redirect.
          :found         ;; -- A 302 found redirect (default).
          :see-other     ;; -- A 303 see other redirect.
          :not-modified  ;; -- A 304 not modified redirect.
          :proxy         ;; -- A 305 proxy redirect.
          :temporary     ;; -- A 307 temporary redirect.
          ))

(defn scheme-redirect
  "Forces a permanent redirection to the supplied scheme (:https, for
   example)"
  ([request scheme]
     (scheme-redirect request scheme friend/*default-scheme-ports*))
  ([request scheme scheme-mapping]
     (-> (util/original-url
          (-> request
              (dissoc :headers)
              (assoc :scheme scheme
                     :server-port (scheme-mapping scheme))))
         (response/redirect :permanent))))

(s/defn bounce
  "Returns a response that sends the user back to the URI of this
  request. Useful in redirecting POSTs back to their forms."
  ([req] (response/redirect (:uri req)))
  ([req type :- RedirectType]
     (response/redirect (:uri req) type)))

(defn transfer-session
  "Moves the request's session over to the reponse."
  [resp req]
  (assoc resp :session (:session req)))
