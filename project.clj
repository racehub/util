(defproject racehub/util "0.3.16-SNAPSHOT"
  :description "Utility functions from RaceHub."
  :url "https://github.com.com/racehub/util"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 [com.andrewmcveigh/cljs-time "0.2.4"]
                 [com.cemerick/friend "0.2.0"
                  :exclusions [org.openid4java/openid4java-nodeps
                               com.google.inject/guice
                               net.sourceforge.nekohtml/nekohtml
                               org.apache.httpcomponents/httpclient
                               org.clojure/core.cache]]
                 [com.cemerick/url "0.1.1"
                  :exclusions [com.cemerick/clojurescript.test]]
                 [com.googlecode.libphonenumber/libphonenumber "5.0"]
                 [com.novemberain/validateur "2.4.2"]
                 [com.taoensso/carmine "2.6.2"]
                 [com.taoensso/timbre "3.2.1"]
                 [optimus "0.15.1" :exclusions [clj-time]]
                 [org.apache.httpcomponents/httpclient "4.3.5"]
                 [paddleguru/clutch "0.5.0"]
                 [paddleguru/forms-bootstrap "0.9.1" :exclusions [ring/ring]]
                 [paddleguru/geonames "0.7.0"]
                 [prismatic/schema "0.3.0"]
                 [amazonica "0.1.30"]
                 [cheshire "5.3.1"]
                 [clj-redis-session "2.1.0"]
                 [clj-stacktrace "0.2.8"]
                 [clj-time "0.11.0"]
                 [crypto-random "1.2.0"]
                 [environ "0.5.0"]
                 [http-kit "2.1.19"]
                 [liberator "0.12.2"]
                 [lib-noir "0.8.4" :exclusions [ring/ring]]
                 [lock-key "1.0.0"]]
  :jar-exclusions [#"\.swp|\.swo|\.DS_Store"]
  :source-paths ["src/clj"]
  :profiles {:dev {:injections [(require 'schema.core)
                                (schema.core/set-fn-validation! true)]
                   :dependencies [[org.clojure/clojurescript "0.0-2356"] ;;"1.7.122"
                                  [org.clojure/test.check "0.5.7"]
                                  [midje "1.6.3"]
                                  [ring-mock "0.1.5"]]
                   :test-paths ["test/clj"]
                   :plugins [[paddleguru/lein-gitflow "0.1.2"]]}})
