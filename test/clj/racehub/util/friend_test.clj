(ns racehub.util.friend-test
  (:refer-clojure :exclude (identity))
  (:use clojure.test
        racehub.util.friend)
  (:require [cemerick.friend :as friend]))

(defn authify [user]
  (assoc user :identity (:username user)))

(def sam-doc (authify {:username "sam"}))
(def sam (:username sam-doc))
(def dave-doc (authify {:username "dave"}))
(def dave (:username dave-doc))

(def example-response
  {:session {::friend/identity
             {:current sam
              :authentications {sam sam-doc
                                dave dave-doc}}}})

(deftest authentication-test
  (let [auths (authentications example-response)
        without-dave (-> example-response (remove-authentication dave))
        without-sam (-> example-response (remove-authentication sam))
        without-either (-> without-sam
                           (remove-authentication dave))]
    (is (= auths {sam sam-doc dave dave-doc})
        "Authentications returns the auths.")

    (is (nil? (:current (friend/identity without-sam)))
        "Removing the :current authentication kills that key")

    (is (= sam (:current (friend/identity without-dave)))
        "Removing a different auth keeps it intact.")

    (testing "user switching"
      (is (= sam (current-user example-response))
          "The current user is sam.")
      (is (= sam (-> example-response
                     (switch-user "missing_user")
                     current-user))
          "The current user doesn't change if you switch to someone
          not present in the auth map.")

      (is (= dave (-> example-response
                      (switch-user dave)
                      current-user))
          "Switching to a present user works."))

    (testing "Removing an auth nukes it."
      (is (= (authentications without-dave)
             {sam sam-doc}))
      (is (= (authentications without-sam)
             {dave dave-doc})))

    (is (= auths (authentications
                  (merge-authentication without-sam
                                        (dissoc sam-doc :identity))))
        "merging the authentication back in recovers it. (Also, it's
        fine to remove the :identity key... the code subs in the
        username for identity if its gone.)")

    (is (= auths
           (authentications
            (merge-authentication example-response sam-doc)))
        "merging over an existing authorization is fine.")

    (is (= {} (authentications without-either))
        "Removing all auths returns the empty map.")))


(def get-user
  {"timmy" {:username "timmy"}
   sam sam-doc
   dave dave-doc})

(deftest spy-mode-test
  (testing "spy mode transitions"
    (is (nil? (current-spy example-response)))
    (is (= sam (current-user example-response)))
    (let [spying (enable-spying example-response "timmy" get-user)]
      (is (= sam (current-spy spying)))
      (is (= "timmy" (current-user spying)))))

  (is (= example-response
         (disable-spying
          (enable-spying example-response "timmy" get-user)))
      "Enabling and disabling spying gets back the original map.")

  (= example-response
     (disable-spying
      (disable-spying (enable-spying example-response "timmy" get-user)))
     "Disabling twice is a no-op."))
