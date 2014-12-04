(ns racehub.middleware.optimus
  "Frontend optimization via Optimus."
  (:require [clojure.string :refer [lower-case]]
            [optimus.prime :as optimus]
            [optimus.assets :as assets]
            [optimus.optimizations :as optimizations]
            [optimus.strategies :as strategies]
            [racehub.schema :refer [Function]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.util.mime-type :as mt]
            [ring.util.response :as resp]
            [schema.core :as s]))

(defn filename-ext
  "Returns the file extension of a filename or filepath."
  [filename]
  (if-let [ext (second (re-find #"\.([^./\\]+)$" filename))]
    (lower-case ext)))

(defn ext-mime-type
  "Get the mimetype from the filename extension. Takes an optional map
  of extensions to mimetypes that overrides values in the
  default-mime-types map."
  [file-extension & [mime-types]]
  (let [mime-types (merge mt/default-mime-types mime-types)]
    (mime-types file-extension)))

(defn content-type-response
  "If the request has a file extension AND the content type header
  doesn't already exist, plugs in an appropriate content header to the
  request."
  [response req & [opts]]
  (if (resp/get-header response "Content-Type")
    response
    (if-let [extension (filename-ext (:uri req))]
      (let [mime-type (or (ext-mime-type extension (:mime-types opts))
                          "application/octet-stream")]
        (resp/content-type response mime-type))
      response)))

(defn wrap-content-type
  "Middleware that adds a content-type header to the response if one is not
  set by the handler. Uses the ring.util.mime-type/ext-mime-type function to
  guess the content-type from the file extension in the URI. If no
  content-type can be found, it defaults to 'application/octet-stream'.

  Accepts the following options:
    :mime-types - a map of filename extensions to mime-types that will be
                  used in addition to the ones defined in
                  ring.util.mime-types/default-mime-types

  Note that this logic will ONLY apply to responses to requests with a
  file extension that don't have a preset content type."
  [handler & [opts]]
  (fn [req]
    (if-let [resp (handler req)]
      (content-type-response resp req opts))))

(defn add-cdn-base-url-to-assets
  "Tacks the supplied prefix on to every bundle. Used for CDN."
  [assets cdn]
  (map #(assoc % :base-url cdn)
       assets))

(defn public-cache-control [assets]
  (map (fn [asset]
         (if-let [control (get-in asset [:headers "Cache-Control"])]
           (update-in asset [:headers "Cache-Control"] (partial str "public, "))
           asset))
       assets))

(defn optimize-all
  "Applies RaceHub-specific content optimizations to Optimus
  resources."
  [conf]
  (fn [assets options]
    (-> assets
        (optimizations/minify-css-assets options)
        (optimizations/concatenate-bundles)
        (optimizations/add-cache-busted-expires-headers)
        (optimizations/add-last-modified-headers)
        (public-cache-control)
        (add-cdn-base-url-to-assets conf))))

(s/defn wrap-optimus :- Function
  "Ring Middleware that applies Optimus front-end
  optimizations. Get-assets is a function of no args that should
  return a list of assets."
  [handler :- Function
   get-assets :- Function
   conf :- {:dev? s/Bool
            :cdn s/Str
            s/Any s/Any}]
  (-> handler
      (optimus/wrap get-assets
                    (if (:dev? conf)
                      optimizations/none
                      (optimize-all (:cdn conf)))
                    (if (:dev? conf)
                      strategies/serve-live-assets
                      strategies/serve-frozen-assets))
      (wrap-content-type)
      (wrap-not-modified)))
