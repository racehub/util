(ns racehub.util.friend
  "Helper utilities for the friend authorization and authentication
   library, by cemerick."
  (:require [cemerick.friend :as f]
            [ring.util.response :as r]
            [schema.core :as s]))

(def UserName s/Str)

;; ## Control Flow
;;
;; Helpful supplements to Chas's stuff.

(letfn [(auth? [id]
          (boolean
           (f/current-authentication id)))]
  (defn authenticated?
    "Returns true if the current session is authenticated, false
     otherwise. The no-arg checks the *identity* dynamic var bound by
     friend in middleware; the one-arg versions takes a request
     explicitly."
    ([] (auth? f/*identity*))
    ([req]
       (auth? (f/identity req)))))

;; ## Route Building Helpers
;;
;; The following functions make it easy to build routes with custom
;; friend validations.

(defn roles
  "Returns an authorization function that checks if the authenticated
  user has the specified roles. (This is the usual friend behavior.)"
  [roles]
  (fn [id]
    (f/authorized? roles id)))

(defn unauthorized!
  "Throws the proper unauthorized! slingshot error if authentication
  fails. This error is picked up upstream by friend's middleware."
  [handler req]
  (f/throw-unauthorized (f/identity req)
                        {::wrapped-handler handler}))

(defn wrap-authorize
  "Custom ring middleware to help with friend authorization. Takes a
  handler and a predicate of one argument (the request), and only
  allows access through to the handler if the predicate returns
  true. If the predicate fails, Friend will throw a validation error
  internally.

  the authorized? function takes the request as its argument. If you
  want to get the identity out, use the `identity` function inside of
  cemerick.friend, or current-authentication. Both are helpful."
  [handler authorized?]
  (fn [request]
    (if (authorized? request)
      (handler request)
      (unauthorized! handler request))))

(defn wrap-authenticated
  "Takes a handler and wraps it with a check that the current user is
  authenticated."
  [handler]
  (wrap-authorize handler f/identity))

(defmacro if-authenticated
  "Returns a request handler function. if the request is
  authenticated, calls the `then` branch; else, calls the `else`
  branch. The called branch can be a function of 1 argument (the
  request), or a response.

  Optionally, you can bind the passed-in request by providing a vector
  of 1 arg, like this:

  (if-authenticated [req]
     ,,,then,,,
     ,,,else,,,)

  This gives the branches access to the passed-in request."
  ([then else]
     `(if-authenticated [~(gensym "req")] ~then ~else))
  ([[sym] then else]
     `(fn [~sym]
        (if (authenticated? ~sym)
          (let [then# ~then]
            (if (fn? then#)
              (then# ~sym)
              then#))
          (let [else# ~else]
            (if (fn? else#)
              (else# ~sym)
              else#))))))

;; ## Workflow Helpers
;;
;; Here are some nice functions for developing custom authentication
;; workflows using friend.

(defn on-success
  "When a workflow is run, it returns either an authorized user
  document OR a ring response noting that the auth failed. If a login
  isn't actually happening, Friend authenticates the user via their
  session.

  `on-success` returns a new workflow that runs the old workflow, then
  calls the supplied function if the workflow is applied
  successfully."
  [workflow f]
  (fn [& args]
    (when-let [res (apply workflow args)]
      (when (f/auth? res)
        (f))
      res)))

;; ## Response Helpers

(defn original-url
  "Returns the original URL the user was trying to access, with query
  parameters intact. Note that this will return a relative URL."
  [{:keys [uri query-string]}]
  (str uri
       (when (seq query-string)
         (str \? query-string))))

(defn force-redirect-on-auth
  "Takes a request and result and returns a NEW result that will force
  a redirect to the original URI in the req after the next
  login. (Paddleguru uses this on the custom `you must register!` page
  to show user info before presenting links to the login or signup
  pages.)"
  [req result]
  (let [uri (original-url req)
        result (r/response result)]
    (assoc-in result [:session ::f/unauthorized-uri] uri)))

;; ## Spy Mode Related Items

(defn authentications
  "Returns the current batch of Friend authentications."
  [req]
  (let [id (or (f/identity req) req)]
    (:authentications id)))

(s/defn remove-authentication
  "Removes the authentication for the supplied user from the
  response. If the user is the :current user, nukes that key as well."
  [response username :- UserName]
  (-> response
      (update-in [:session ::f/identity]
                 (fn [m]
                   (let [m (update-in m [:authentications] dissoc username)
                         current-user (:current m)]
                     (if (= username current-user)
                       (dissoc m :current)
                       m))))))

(defn merge-authentication
  "Adds the supplied authentication (user map) into the session."
  [response auth]
  (let [auth (if (:identity auth)
               auth
               (assoc auth :identity (:username auth)))]
    (-> response
        (update-in [:session ::f/identity]
                   #(assoc-in % [:authentications (:identity auth)] auth)))))

(s/defn current-user :- (s/maybe UserName)
  "Returns the current user, if there is one."
  [req]
  (:current (f/identity req)))

(s/defn switch-user
  "If the supplied user is authenticated, switches to that user, else
  returns the response untouched."
  [response username :- UserName]
  (if-let [auth (-> (authentications response)
                    (get username))]
    (assoc-in response [:session ::f/identity :current]
              (:identity auth))
    response))

(s/defn rename-auth
  "If the `from` user exists in the auth map, renames that user and
  switches to the new name."
  [response
   from :- UserName
   to :- UserName]
  (if-let [auth (-> (authentications response)
                    (get from))]
    (-> response
        (merge-authentication (assoc auth
                                :identity to
                                :username to))
        (remove-authentication from)
        (switch-user to))
    response))

(s/defn current-spy :- (s/maybe UserName)
  "Returns the current spy, if there is one."
  [req]
  (:spy (f/identity req)))

(def spying? (comp boolean current-spy))

(s/defn set-spy [req username :- UserName]
  (assoc-in req [:session ::f/identity :spy] username))

(s/defn remove-spy [req]
  (update-in req [:session ::f/identity] dissoc :spy))

;; TODO: Only promote super admin spies.
(s/defn enable-spying
  "Spies on the supplied username, either with the same spy or by
  promoting the current logged in user."
  [req username :- UserName user-fn]
  (if-let [spy (or (current-spy req)
                   (current-user req))]
    (let [user (user-fn username)]
      (-> (merge-authentication req user)
          (switch-user (:username user))
          (set-spy spy)))))

(defn disable-spying [req]
  (if-let [spy (current-spy req)]
    (-> req
        (remove-authentication (current-user req))
        (switch-user spy)
        (remove-spy))
    req))
