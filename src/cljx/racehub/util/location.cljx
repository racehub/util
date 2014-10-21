(ns racehub.util.location
  "Geonames API wrappers."
  (:require [schema.core :as s :include-macros true])
  #+clj
  (:require [clojure.core.async :as a]
            [racehub.schema :as ps]
            [racehub.util :as u]
            [racehub.util.config :as conf]
            [geonames.geocoder :as geo]
            [schema.core :as s]))

(s/defschema Reference
  (s/named s/Str "Unique reference for this place in Google's Places
  API. Can be used to query their servers for more information about
  the place (if we want more detailed address information, for
  example)."))

(s/defschema PlaceID
  (s/named s/Str "Stable identifier for a place in the Google Places
  API. Can't be used to query, but the same place will have the same
  ID, apparently."))

(s/defschema ZoomLevel
  "More info here: https://developers.google.com/maps/documentation/staticmaps/#Zoomlevels. Changed to maybe because sometimes they come in as nil."
  (s/maybe (s/named s/Int "Zoom level of the google map. Ranges from 0 -> 21+")))

(def Timezone s/Str)

(s/defschema GoogleData
  {:formatted-address s/Str
   :reference Reference
   :id PlaceID})

(s/defschema Point
  {:latitude s/Num
   :longitude s/Num})

(s/defschema MarkerPosition
  (assoc Point :zoom-level ZoomLevel))

(s/defschema Address
  {(s/optional-key :street-number) s/Str
   (s/optional-key :street-address) s/Str
   (s/optional-key :city) s/Str
   (s/optional-key :zip) s/Str
   (s/optional-key :state) s/Str
   (s/optional-key :country) s/Str})

(s/defschema Location
  {(s/optional-key :google) GoogleData
   (s/optional-key :position) MarkerPosition
   (s/optional-key :address) Address
   (s/optional-key :timezone) Timezone})

#+clj
(do
  (comment
    "Example overage message:"
    {:status
     {:message "the hourly limit of 2000 credits for demo has
  been exceeded. Please use an application specific account. Do not
  use the demo account for your application."
      :value 19}})

  (s/defn get-point :- Point
    [loc :- Location]
    (-> loc :position (select-keys [:latitude :longitude])))

  (s/defn timezone :- ps/Channel
    [loc :- Location]
    (geo/with-key (conf/env :geonames-key)
      (->> (geo/timezone (get-point loc))
           (a/map< :timezoneId))))

  (s/defn assoc-timezone
    "Updates the supplied location by adding in the timezone with a call
   to the Geonames API. Takes an optional timeout. If the timeout
   fails, the functions returns the unchanged location."
    ([loc :- Location]
       (if-let [tz (a/<!! (timezone loc))]
         (assoc loc :timezone tz)
         loc))
    ([loc :- Location millis :- ps/Millis]
       (if-let [tz (first (a/alts!! [(timezone loc) (a/timeout millis)]))]
         (assoc loc :timezone tz)
         loc))))
