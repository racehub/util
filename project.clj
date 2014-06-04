(defproject paddleguru/util "0.1.1"
  :description "Utility functions from PaddleGuru."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [com.cemerick/url "0.1.1"]
                 [com.googlecode.libphonenumber/libphonenumber "5.0"]
                 [commons-validator/commons-validator "1.4.0"]
                 [enlive "1.1.4"]
                 [potemkin "0.3.4"]
                 [prismatic/schema "0.2.2"]]
  :hooks [cljx.hooks]
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :source-paths ["src/clj" "target/generated/clj"]
  :resource-paths ["target/generated/cljs"]
  :profiles {:dev {:injections [(require 'schema.core)
                                (schema.core/set-fn-validation! true)]
                   :dependencies [[org.clojure/test.check "0.5.7"]
                                  [midje "1.6.3"]]
                   :test-paths ["test/clj"]
                   :plugins [[com.keminglabs/cljx "0.4.0"]]}}
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/generated/clj"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/cljs"
                   :rules :cljs}]})
