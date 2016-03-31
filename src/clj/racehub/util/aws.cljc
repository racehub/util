(ns racehub.util.aws
  "AWS Helpers for image uploading and friends."
  (:require [racehub.schema :as ps]
            [schema.core :as s :include-macros true]
            #?@(:clj [[amazonica.aws.s3transfer :as s3t]
                      [amazonica.core :as amazon]
                      [clojure.core.async :as a]
                      [clojure.java.io :refer [file]]]))
  #?(:clj
     (:import [java.util UUID]
              [java.io FileInputStream File InputStream]
              [com.amazonaws.services.s3.transfer TransferManager]
              [com.amazonaws.services.s3.model ObjectMetadata])))

;; This is required to register the proper runtime reflection
;; coercions.
#?(:clj
   (require 'amazonica.aws.s3))

;; Map of content type, so we can recover a proper uri.
(def content-types
  "Map of content-type to image extension."
  {"image/jpeg" "jpg"
   "image/bmp" "bmp"
   "image/gif" "gif"
   "image/png" "png"})

(def crazy-map
  "Map of IE-style, annoying prefixes over to proper image types."
  {"image/pjpeg" "image/jpeg"
   "image/x-png" "image/png"})

(s/defschema CrazyInput
  (apply s/enum (keys crazy-map)))

(s/defschema ImageContent
  (s/enum "image/jpeg" "image/png" "image/bmp" "image/gif"))

(def AWSKey
  (s/named s/Str "AWS bucket key"))

(def Bucket
  "Supported buckets. We only have one for now."
  (s/enum :photos))

(def BucketConf
  {:bucket-name s/Str
   :cloudfront s/Str})

(def ImageName
  (-> (s/either s/Keyword s/Str)
      (s/named "image name")))

(def CloudFrontURL
  (s/named s/Str "URL representing a path to a blob served by CloudFront."))

(def Credentials
  {:access-key s/Str
   :secret-key s/Str
   (s/optional-key :region) s/Str})

(s/defschema AWSConfig
  {:credentials Credentials
   :buckets {Bucket BucketConf}})

;; ## Methods

(s/defn massage-content-type :- ImageContent
  "Fixes up some of the crazier input types that IE responds to."
  [t :- (s/either ImageContent CrazyInput)]
  (or (crazy-map t) t))

#?(:clj
   (do
     (def Image
       "Schema for images that come in via the temporary store, from ring's
  multipart-params middleware."
       {:size (s/named s/Int "number of  bytes in the supplied image")
        :tempfile (-> (s/either String File InputStream)
                      (s/named "temporary on-disk location of the image"))
        :content-type (s/either ImageContent CrazyInput)
        (s/optional-key :filename) (s/named s/Str "original uploaded filename")})

     (s/defn generate-key :- AWSKey
       [image-name :- ImageName content-type :- ImageContent]
       (let [suffix (content-types content-type)
             path (str (name image-name) "/original/" (UUID/randomUUID))]
         (if suffix
           (str path "." suffix)
           path)))

     (s/defn current-manager :- TransferManager
       "Returns the current active TransferManager."
       [cred :- Credentials]
       (@#'amazon/amazon-client TransferManager cred {}))

     (s/defn bucket-config :- BucketConf
       "Returns a BucketConf from the supplied AWS config."
       [conf :- AWSConfig bucket :- Bucket]
       (bucket (:buckets conf)))

     (def bucket-name
       (comp :bucket-name bucket-config))

     (def cloudfront-name
       (comp :cloudfront bucket-config))

     (s/defn upload! :- {s/Any s/Any}
       "Triggers an upload via the TransferManager to s3. Returns a clojure
  map that represents an AWS Transfer instance. The map goes from
  method name (in keyword form) -> function with input types as
  described here:
  http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/s3/transfer/Transfer.html"
       ([conf :- AWSConfig
         man :- TransferManager
         bucket :- Bucket
         key :- AWSKey
         filename :- String]
        (amazon/marshall
         (.upload man (bucket-name conf bucket)
                  key
                  (file filename))))
       ([conf :- AWSConfig
         man :- TransferManager
         bucket :- Bucket
         key :- AWSKey
         input :- InputStream
         params :- {s/Any s/Any}]
        (let [upload (.upload man
                              (bucket-name conf bucket)
                              key
                              input
                              (amazon/coerce-value params ObjectMetadata))]
          (amazon/marshall upload))))

     (s/defn trim-channel :- ps/Channel
       "Modifies a file upload channel to only return success or failure
  events.

  NOTE: Down the road, we could use the other channel elements (bytes
  transferred so far) to build a client-side upload progress bar."
       [chan :- ps/Channel url :- AWSKey]
       (->> chan
            (a/filter< (comp #{:completed :failed} :event))
            (a/map< (fn [item]
                      (if (= :completed (:event item))
                        {:success true, :url url}
                        {:success false})))))

     (s/defn to-stream :- InputStream
       [tempfile :- (:tempfile Image)]
       (if (instance? InputStream tempfile)
         tempfile
         (FileInputStream. (file tempfile))))

     (s/defn upload-image :- ps/Channel
       "Takes an image that'd show up directly from the upload dialogue and
  starts an asynchronous upload to S3. Returns a channel that reports
  the success status and the new url on a successful upload.

  Uploaded images are cached, by default, for 10 years (see the
  cache-control header in here)"
       [aws-config :- AWSConfig
        bucket :- Bucket
        image-name :- ImageName
        {:keys [tempfile content-type size]} :- Image]
       (let [{:keys [credentials]} aws-config
             results (a/chan)
             new-key (generate-key image-name content-type)
             man (current-manager credentials)
             ret (upload! aws-config
                          man
                          bucket
                          new-key
                          (to-stream tempfile)
                          {:content-length size
                           :content-type (massage-content-type content-type)
                           :cache-control "max-age=315360000"})]
         ((:add-progress-listener ret) #(a/put! results %))
         (trim-channel results new-key)))

     ;; ## Public

     (s/defn add-to-doc :- {s/Any s/Any}
       [doc :- {s/Any s/Any}
        bucket :- Bucket
        image-name :- ImageName
        photo-url :- s/Str]
       (assoc-in doc [bucket (keyword image-name)] photo-url))

     (s/defn cloudfront-url :- (s/maybe CloudFrontURL)
       "Returns a cloudfront URL for the supplied image suitable for
     splicing into HTML. Example args a user-doc, :photos,
     and :main-photo."
       [conf :- AWSConfig
        doc :- {s/Any s/Any}
        bucket-key :- Bucket
        image-name :- ImageName]
       (if-let [suffix (-> doc :photos (get (keyword image-name)))]
         (if-let [cloudfront (cloudfront-name conf bucket-key)]
           (str cloudfront "/" suffix))))))
