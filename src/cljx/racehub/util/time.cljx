(ns racehub.util.time
  (:require [#+clj clj-time.core #+cljs cljs-time.core :as time]
            [#+clj clj-time.format #+cljs cljs-time.format :as format]
            #+cljs [cljs-time.coerce :as coerce]
            [clojure.string :as string]
            [racehub.schema :as ps]
            #+clj [racehub.util.config :as conf]
            [schema.core :as s :include-macros true])
  #+clj
  (:import [org.joda.time DateTime DateMidnight DateTimeZone DateTimeUtils]))

;; ## Schemas

(s/defschema TimeZone
  "Timezone string."
  #+cljs s/Str
  #+clj (apply s/enum (DateTimeZone/getAvailableIDs)))

(def EST "America/New_York")
(def PST "America/Los_Angeles")

(s/def UnixTime
  (s/named s/Int "Instant in time defined as the number of seconds
  since midnight UTC Jan 1 1970."))

(def valid-calendar-formats
    [#"^[0-1][0-9]/[0-3][0-9]/[0-9][0-9][0-9][0-9]$"
     #"^[0-9]/[0-3][0-9]/[0-9][0-9][0-9][0-9]$"])

;; ## Code

(s/defn valid-calendar-str? :- s/Bool
  "Checks to see if the provided calendar string (ie Date) is valid."
  [s :- (s/maybe s/Str)]
  (boolean
   (when s
     (some (fn [regex] (re-find regex s))
           valid-calendar-formats))))

(s/defn timestamp :- s/Str
  "Returns the current timestamp, formatted using the supplied
  formatter. Call `(clj-time.format/show-formatters) to see all
  options; formatter defaults to :base-date-time-no-ms."
  ([] (timestamp :basic-date-time-no-ms))
  ([format-kwd :- s/Keyword]
     (format/unparse (format/formatters format-kwd)
                     (time/now))))

(s/defn convert-to-rfc822 :- s/Str
    "Takes in a date-time string and converts it to rfc822 format."
    [s :- s/Str]
    (let [newstring (format/unparse
                     (format/formatters :rfc822)
                     (format/parse s))]
      (string/replace newstring " +0000" "")))

(s/defn timestamp-to-datetime
  [s :- s/Str]
  (-> (format/formatters :basic-date-time-no-ms)
      (format/parse s)))

(s/defn timestamp-to-millis :- s/Int
  [s :- s/Str]
  (let [^DateTime date (timestamp-to-datetime s)]
    #+clj (.getMillis date)
    #+cljs (coerce/to-long date)))

(s/defn code-expired? :- s/Bool
  "Takes in the activation-code-created-at string stored in the db,
  and returns true if the code has expired (over 1 hr, or limit, old)."
  [code-time-str :- s/Str & limit]
  (let [time-hrs (-> (timestamp-to-datetime code-time-str)
                     (time/interval (time/now))
                     (time/in-hours))]
    (not (< time-hrs (or (first limit) 1)))))

(def display-format
  (format/formatter "MMM d"))

#+clj
(s/defn db-format [tz :- TimeZone]
  (let [timezone ^DateTimeZone (time/time-zone-for-id tz)]
    (format/formatter "MM/dd/yyyy" timezone)))

(s/defn default-timezone :- s/Str
  "Override using the default-timezone."
  []
  #+clj (conf/env :default-timezone PST)
  #+cljs PST)

(s/defn calendar-str-to-date-time-obj
  "Takes in a string from an HTML field using the jquery
  datepicker."
  #+cljs
  ([date-str]
     (try (-> (format/formatter "MM/dd/yyyy")
              (format/parse date-str))
          (catch #+clj Exception #+cljs :default e nil)))
  #+clj
  ([date-str]
     (let [tz (default-timezone)]
       (calendar-str-to-date-time-obj date-str tz)))
  #+clj
  ([date-str timezone :- TimeZone]
     (try (format/parse (db-format timezone) date-str)
          (catch Exception e nil))))

(s/defn after? :- s/Bool
    "Takes in two timestamps and returns true if a is after b."
    [a :- s/Str b :- s/Str]
    (time/after? (calendar-str-to-date-time-obj a)
                 (calendar-str-to-date-time-obj b)))

(def now time/now)

(s/defn end-not-before-start? :- s/Bool
  "Takes in two calendar strings (formatted DD/MM/YYYY). Returns false
  if start is not on or before end."
  [start-date :- s/Str
   end-date :- s/Str]
  (let [start (calendar-str-to-date-time-obj start-date)
        end (calendar-str-to-date-time-obj end-date)]
    (or (= start-date end-date)
        (time/before? start end))))

(s/defn in-days :- s/Int
  "Takes in a DateTime object and returns the number of days form the
  epoch to the given date."
  [t]
  (time/in-days
   (time/interval (time/epoch) t)))

(s/defn days-since-epoch :- s/Int
    "Takes in a mm/dd/yyyy and returns the number of days since the Unix
  epoch. Useful for sorting dates."
    [s :- s/Str]
    (in-days (calendar-str-to-date-time-obj s)))

(s/defn within-n-days? :- s/Bool
  "Takes in a mm/dd/yyyy and returns true if its within plus or minus
  n days, inclusive, from today."
  [n :- (s/named s/Int "Plus/minus number of days")
   s :- (s/named s/Str "Date in mm/dd/yyy format")]
  (let [s-days (days-since-epoch s)
        now-days (in-days (time/now))]
    (and (>= s-days (- now-days n))
         (<= s-days (+ now-days n)))))

(s/defn mins-since-epoch :- s/Int
  "Takes in a timestamp and returns the number of minutes since the Unix
  epoch. Useful for sorting dates."
  [ts :- s/Str]
  (time/in-minutes
   (time/interval (time/epoch)
                  (timestamp-to-datetime ts))))

(s/defn calendar-str-to-unix-time :- UnixTime
  "Returns the seconds from the UNIX epoch (UTC) to the given
  mm/dd/yyyy in the given time zone; thus describing an instant in
  time."
  [s :- s/Str
   tz :- TimeZone]
  (time/in-seconds
   (time/interval (time/epoch)
                  (calendar-str-to-date-time-obj s tz))))

(s/defn same-date? :- s/Bool
  "Takes in two DateTime objects, and returns true if they represent
  the same date (ie same day, month, year)."
  [dt1 dt2]
  (try (and (= (time/day dt1) (time/day dt2))
            (= (time/month dt1) (time/month dt2))
            (= (time/year dt1) (time/year dt2)))
       (catch #+clj Exception #+cljs :default e false)))

(s/defn same-day? :- s/Bool
  "Takes in two calendar dates ie 9/15/2012, 4/23/2012 and returns
  true if they represent the same day."
  [t1 :- s/Str t2 :- s/Str]
  (let [dt1 (calendar-str-to-date-time-obj t1)
        dt2 (calendar-str-to-date-time-obj t2)]
    (same-date? dt1 dt2)))

(s/defn db-date-to-display-str :- (s/maybe s/Str)
    "Takes in a date object and returns a str suitable for use in a
  regatta, ie Jan 12."
    [date-obj]
    (try (format/unparse display-format date-obj)
         (catch #+clj Exception #+cljs :default e nil)))

(s/defn db-str-to-display-str :- (s/maybe s/Str)
    "Takes in a str of the date in the MM/dd/yyyy format, ie 11/12/2012
  and returns 'Nov 12'"
    [s :- s/Str]
    (db-date-to-display-str
     (calendar-str-to-date-time-obj s)))

(s/defn get-year-from-db-str :- s/Str
    "Returns a string representing the year of the DateTime object"
    [d :- s/Str]
    (format/unparse
     (:year format/formatters)
     (calendar-str-to-date-time-obj d)))

(s/defn date->mdy :- s/Str
  [s :- s/Str]
  (str (db-str-to-display-str s)
       ", "
       (get-year-from-db-str s)))

(s/defn get-header-str
  "Takes in a start and end date, both formatted as mm/dd/yy, and
  returns a string suitable for display as a header. Ie: Jan 12 to Jan
  16, 2012."
  [start :- s/Str end :- s/Str]
  (if (same-day? start end)
    (date->mdy start)
    (str (db-str-to-display-str start)
         " to "
         (date->mdy end))))

#+clj
(do
  (s/defn zoned-now :- DateTime
    "Returns the current time pegged to the supplied timezone. if no
  timezone is supplied, uses the configuration default."
    ([] (zoned-now (default-timezone)))
    ([tz :- TimeZone]
       (let [zone ^DateTimeZone (time/time-zone-for-id tz)]
         (DateTime. zone))))

  (s/defn zoned :- DateTime
    [dt :- DateTime
     tz :- TimeZone]
    (time/to-time-zone dt (time/time-zone-for-id tz)))

  (defn pst-time-str [ts]
    (format/unparse (format/formatter "MMM d hh:mm:ss aa z")
                    (time/from-time-zone (timestamp-to-datetime ts)
                                         (time/time-zone-for-offset 7))))

  (defn build-formatter
    [{:keys [timezone military longform]}]
    (cond
     (and military timezone) "MM/dd/yyyy HH:mm:ss Z"
     military "MM/dd/yyyy HH:mm:ss"
     timezone "MM/dd/yyyy hh:mm:ss aa Z"
     longform "MMM d yyyy hh:mm:ss aa z"
     :default "MM/dd/yyyy hh:mm:ss aa"))

  (s/defn datetime-to-display-str :- s/Str
    [datetime :- DateTime
     format :- (s/either s/Str s/Keyword)]
    (let [formatter (if (keyword? format)
                      (build-formatter {format true})
                      format)]
      (try (-> (format/formatter formatter (.getZone datetime))
               (format/unparse datetime))
           (catch Exception e nil))))

  (defn timestamp-to-display-str
    [ts & {:keys [zone format]
           :or {format :longform}}]
    (let [dt (timestamp-to-datetime ts)
          dt (if zone
               (zoned dt zone)
               dt)]
      (datetime-to-display-str dt format)))

  (s/defn timestamp->mdy :- s/Str
    [s :- ps/Timestamp]
    (timestamp-to-display-str s :format "MMM d, yyyy"))

  (defn format-timestamp
    "Takes in a timestamp, and returns a nicely formatted
  string. ."
    [ts & {:as opts}]
    (format/unparse (format/formatter (build-formatter opts))
                    (timestamp-to-datetime ts)))

  (s/defn midnight :- DateMidnight
    "Returns the midnight representing the BEGINNING of the DateTime."
    [dt :- DateTime]
    (.toDateMidnight dt))

  (s/defn next-midnight :- DateMidnight
    "Returns the midnight representing the END of the supplied
  DateTime."
    [dt :- DateTime]
    (let [dt ^DateTime (-> dt (time/plus (time/days 1)))]
      (.toDateMidnight dt)))

  ;; ## Time Mocking Facilities

  (s/defn with-time* [t :- (s/either DateTime DateMidnight)
                      f :- ps/Function]
    "Call the supplied function with the supplied time mocked. So, calls
  to (now) will return that time."
    (try (DateTimeUtils/setCurrentMillisFixed (.getMillis t))
         (f)
         (finally (DateTimeUtils/setCurrentMillisSystem))))

  (defmacro with-time
    "Executes the body with the supplied time mocked. So, calls to (now)
  will return that time."
    [t & body]
    `(with-time* ~t (fn [] ~@body)))

  (defn month-name
    "Takes in a date object and returns the month name."
    [date-obj]
    (format/unparse (format/formatter "MMM")
                    date-obj)))
