(ns paddleguru.util
  (:require [clojure.string :as st]
            #+cljs [cljs.core.async :refer [chan]]
            #+clj [clojure.core.async :refer [chan]]
            [schema.core :as s]
            #+clj [schema.macros :as sm]
            #+clj [cemerick.url :as url]
            #+clj [net.cgrand.enlive-html :as html]
            #+clj [potemkin :refer [import-vars]]
            #+cljs goog.debug
            #+cljs [goog.string :as gstring]
            #+cljs [goog.string.format :as gformat])
  #+cljs (:require-macros [schema.macros :as sm])
  #+clj (:import [java.net URI]
                 [com.google.i18n.phonenumbers NumberParseException PhoneNumberUtil]
                 [clojure.lang ArraySeq Symbol]
                 [java.util List UUID]
                 [java.security SecureRandom]
                 [org.apache.commons.validator EmailValidator]))

(defn with-chan [f]
  (let [output (chan)]
    (f output)
    output))

(defn insert [v pos val]
  (apply conj (subvec v 0 pos) val (subvec v pos)))

(defn jquery-clean
  [s]
  (st/replace s #"(:|\.|\[|\])" ""))

(sm/defn lowercase= :- s/Bool
  "returns true if the strings are equal without case sensitivity,
  false otherwise."
  [a :- s/Str b :- s/Str]
  (boolean
   (and a b (= (st/lower-case a)
               (st/lower-case b)))))

(defn map-by [key-func val-func coll]
  (into {} (map (juxt key-func val-func) coll)))

(defn between
  "returns a predicate that checks that the supplied number falls
  between the inclusive lower and exclusive upper bounds supplied."
  [low high]
  (fn [x]
    (and (>= x low)
         (< x high))))

(defn remove-values
  "Takes in a map and predicate that filters by map values."
  [pred m]
  (->> m
       (remove (fn [[k v]] (pred v)))
       (into {})))

(defn select-non-empty
  "Takes in a map and a sequence of keys - returns a subset of the map
  with the given sequence of keys that are present and have non-empty
  values."
  [m ks]
  (->> (select-keys m ks)
       (remove-values (some-fn nil? #{""} (every-pred coll? empty?)))))

(defn days->secs [days]
  (-> days (* 24) (* 60) (* 60)))

(defn leaves
  "Takes in a nested map structure (all leaves must have equal depth),
  and returns the keys of the lowest level map concated
  together. Useful for getting all usernames in a rankings map."
  [m]
  (if (map? (first (vals m)))
    (mapcat leaves (vals m))
    (keys m)))

(defn mapk
  "Maps the keyspace using the supplied function. Any duplicate keys
  will get knocked out in a nondeterministic order, so be careful!"
  [f m]
  (into {} (for [[k v] m]
             [(f k) v])))

(def Named
  (s/either s/Str s/Keyword))

(def Squashable
  "This is sort of a silly type, since the second condition matches
  anything. Mostly here for docs, since you might squash a map."
  {Named (s/either {Named s/Any} s/Any)})

(defn map-values
  "Maps the keyspace using the supplied function. Any duplicate keys
  will get knocked out in a nondeterministic order, so be careful!"
  [f m]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn merge-with-map
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by looking up the proper merge function and in the
  supplied map of key -> merge-fn and using that for the big merge. If
  a key doesn't have a merge function, the right value wins (as with
  merge)."
  [merge-fns & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
			(let [k (key e) v (val e)]
			  (if-let [f (and (contains? m k)
                                          (merge-fns k))]
			    (assoc m k (f (get m k) v))
			    (assoc m k v))))
          merge2 (fn [m1 m2]
		   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defn underscore->dash [m]
  (mapk (comp keyword #(st/replace % "_" "-") name)
        m))

(defn no-duplicates?
  "Takes in a collection and tests if all the elements are unique, ie
  no duplicates."
  [c]
  (= c (distinct c)))

(defn find-duplicates [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))

(defn update-in-all [m ks f]
  (reduce #(update-in %1 [%2] f) m ks))

(defn delete-index
  "Takes in a vector and an index, and returns a vector without the
  element at the given index."
  [v index]
  (vec (concat (subvec v 0 index)
               (subvec v (inc index) (count v)))))

(defn insert-index
  "Takes in a vector, index, and element and returns a vector with the
  element inserted at the given index (shifting the rest over)."
  [v index element]
  (vec (concat (subvec v 0 index)
               [element]
               (subvec v index (count v)))))

(def separate
  "Can be used in conjunction with a predicate function and a
collection to return: [(remove predicate items), (filter predicate
items)]"
  (juxt remove filter))

(defn wrap-p
  [s]
  {:tag :p
   :content [s]})

(defn wrap-all-in-ps
  [c]
  (map wrap-p (flatten c)))

(def div-rem
  "Takes two numbers and returns a 2-vector of [quotient, remainder]"
  (juxt quot mod))

(sm/defn truncate-string :- s/Str
  [s :- s/Str len :- s/Int]
  (if (> (count s) len)
    (str (apply str (take (- len 3) s)) "...")
    s))

(sm/defn ^:export clear-specials :- (s/maybe s/Str)
  [s :- (s/maybe s/Str)]
  (when s
    (st/replace s #"[^a-zA-Z0-9]+" "")))

(defn ^:export remove-spaces [input]
  (when input
    (st/replace input " " "")))

(defn ^:export lower-case
  [input]
  (when input
    (st/lower-case input)))

(sm/defn number-string? :- s/Bool
  "Checks to make sure that the given string contains only numbers,
  '.', or ','. The string may start with a '-'."
  [s :- (s/maybe s/Str)]
  (boolean (when s
             (re-matches #"-?[0-9.,]+" s))))

(sm/defn ^:export str-to-int :- s/Int
  "converts string to ints, returns 0 for exceptions."
  [s :- (s/maybe (s/either s/Str s/Num))]
  #+cljs (if (js/isNaN s)
           0
           (js/parseInt s))
  #+clj (if (number? s)
          (int s)
          (try (Integer/parseInt s)
               (catch Exception e 0))))

(sm/defn ^:export str-to-float :- s/Num
  "converts string to float, returns 0 for exceptions."
  [s :- (s/maybe (s/either s/Str s/Num))]
  #+cljs (if (js/isNaN s)
           0
           (js/parseFloat s))
  #+clj (if (number? s)
          (float s)
          (if (number-string? s)
            (try (Float/parseFloat s)
                 (catch Exception e 0.0))
            0.0)))

(sm/defn ^:export str-to-double :- s/Num
  "converts string to doubles, returns 0 for exceptions."
  [s :- (s/maybe (s/either s/Str s/Num))]
  #+cljs (if (js/isNaN s)
           0
           (js/parseFloat s))
  #+clj (if (number? s)
          (double s)
          (if (number-string? s)
            (try (Double/parseDouble s)
                 (catch Exception e 0.0))
            0.0)))

(defn pretty-int
  "Takes in an int or a str, adds commas where appropriate, returns a
  string."
  [n]
  (->> (reverse (str n))
       (partition 3 3 [])
       (interpose [\,])
       (map #(apply str %))
       (apply str)
       (st/reverse)))

(def div-mod (juxt quot mod))

(defn ^:export ms-to-time [millis]
  (let [[s ms] (div-mod millis 1000)
        [min s] (div-mod s 60)
        [hr min] (div-mod min 60)]
    (#+cljs gstring/format #+clj format "%02d:%02d:%02d.%02d" hr min s
            (int (#+cljs Math.floor #+clj Math/floor (/ ms 10))))))

(defn ^:export time-to-ms [time]
  (let [parsed (vec (map str-to-int
                         (re-seq #"[0-9]+" time)))]
    (+ (* (parsed 0) 3600000)
       (* (parsed 1) 60000)
       (* (parsed 2) 1000)
       (* (parsed 3) 10))))

(def max-time
  (time-to-ms "99:59:59.99"))

(sm/defn ^:export currency-amt :- s/Str
  [n :- (s/maybe (s/either s/Str s/Num))]
  (let [num (str-to-double n)
        nocommas (#+clj format #+cljs gstring/format "%.2f" num)
        whole-decimal-vec (st/split nocommas #"\.")
        whole-num-str (pretty-int (first whole-decimal-vec))]
    (str whole-num-str "." (second whole-decimal-vec))))

(sm/defn ^:export to-currency :- s/Str
  [n :- (s/maybe (s/either s/Str s/Num))]
  (str "$" (currency-amt n)))

(sm/defn ^:export valid-currency-amt? :- s/Bool
  "Returns true if the given string is a valid currency amount (cents
mandatory). Optional thousands separators; mandatory two-digit
fraction. Negative numbers return false."
  [s :- (s/maybe s/Str)]
  (boolean (when s
             (re-find #"^[0-9]{1,3}(?:,?[0-9]{3})*\.[0-9]{2}$" s))))

(sm/defn ^:export zero-or-positive :- s/Num
  [total :- (s/maybe s/Num)]
  (max 0 (or total 0)))

(sm/defn ^:export clean-currency :- s/Str
  "Returns a 'cleaned' version of the given input currency. Useful for
  validating form fields where the user has to enter a dollar
  amount. Truncates to two decimal points. Negative amounts not
  allowed. All invalid amounts become $0.00. Commas allowed."
  [s :- s/Str]
  (if (valid-currency-amt? s)
    s
    (currency-amt (zero-or-positive (str-to-double s)))))

(sm/defn pennies->double :- (s/maybe s/Num)
    "converts the incoming pennies into a double."
    [pennies :- (s/maybe s/Int)]
    (when pennies
      (/ (float pennies) 100.0)))

(sm/defn pennies->currency :- (s/maybe s/Str)
  "converts the incoming pennies into a currency string with a
    dollar sign prefix on the front."
  [pennies :- (s/maybe s/Int)]
  (when pennies
    (if (neg? pennies)
      (str "-" (to-currency (pennies->double (- pennies))))
      (to-currency (pennies->double pennies)))))

(sm/defn pennies->currency-str :- (s/maybe s/Str)
  "converts the incoming pennies into a currency string WITHOUT a
     dollar sign prefix on the front."
  [pennies :- (s/maybe s/Int)]
  (currency-amt (pennies->double pennies)))

(sm/defn double->pennies :- s/Int
  "Converts the incoming dollar amount (represented by a double)
    into pennies."
  [i :- s/Num]
  (Math/round (* i 100.0)))

#+clj
(do
  (sm/defn squash :- {s/Str s/Any}
    "Squashes a map nested to a single level into a Stripe API
  compatible form (with the brackets)"
    [m :- Squashable]
    (->> (for [[k v] m :let [k (name k)]]
           (if (map? v)
             (mapk #(format "%s[%s]" k (name %)) v)
             {k v}))
         (reduce into {})))

  (sm/defn uuid :- s/Str
    []
    (str (UUID/randomUUID)))

  (defmacro clone-ns
    "Accepts a namespace symbol and clones all vars from that namespace
    into this namespace, allowing users to use them without hassle."
    [sym]
    (require sym)
    (let [publics (cons sym (keys (ns-publics sym)))]
      `(import-vars ~publics)))

  (sm/defn ->int :- (s/maybe s/Int)
    [i :- s/Str]
    (try (Integer/parseInt i)
         (catch Exception _)))

  ;;this belongs in utils or somewhere else
  (s/defn national-phone-number :- (s/maybe s/Int)
    "Takes in a phone number string, which doesnt have to be all
  numbers, and returns a long of the phone number using Google's
  libphonenumber (or nil if the phone number was invalid."
    [phone :- (s/maybe s/Str)]
    (try (-> (PhoneNumberUtil/getInstance)
             (.parse phone "US")
             .getNationalNumber)
         (catch NumberParseException _)))
  ;;http://commons.apache.org/validator/apidocs/org/apache/commons/validator/routines/EmailValidator.html
  (s/defn email-valid? :- s/Bool
    "Returns true if the given email address is valid, with no spaces."
    [email :- s/Str]
    (and (.isValid (EmailValidator/getInstance) email)
         (nil? (re-seq #"[\s]" email))))

  ;; Clojure-only bullshit.
  (defn collectify [obj]
    (cond (nil? obj) []
          (or (sequential? obj) (instance? List obj)) obj
          :else [obj]))

  (defn ensure-defaults [m & k-default-pairs]
    {:pre [(even? (count k-default-pairs))]}
    (reduce (fn [m [k default]]
              (update-in m (collectify k)
                         (fn [v]
                           (if (coll? v)
                             (or (not-empty v) default)
                             (or v default)))))
            m
            (partition 2 k-default-pairs)))

  (s/defn index-of :- (s/maybe s/Int)
    [v :- ArraySeq entry :- s/Any]
    (let[idx (.indexOf v entry)]
      (when (not= idx -1)
        idx)))

  (s/defn try-require :- nil
    [sym :- Symbol]
    (try (require sym)
         (catch Throwable _
           (println "Namespace not available in current mode!" sym))))

  (defmacro maybe-resolve [ns method]
    `(when-let [n# (find-ns (quote ~ns))]
       (when-let [m# (ns-resolve n# (quote ~method))]
         @m#)))

  (s/defn valid-int? :- s/Bool
    [n :- s/Any]
    (or (integer? n)
        (boolean (try (Integer/parseInt n)
                      (catch Exception e nil)))))

  (defn parse-html-str
    "Takes in a string of html (like from an embedded html editor) and
  converts it into enlive nodes (suitable for use in an Enlive
  snippet)"
    [s]
    (html/html-snippet s))

  (defn nodes-to-html
    [n]
    (apply str (html/emit* n)))

  (defmacro maybe-substitute
    ([expr] `(if-let [x# ~expr]
               (html/substitute x#)
               identity))
    ([expr & exprs] `(maybe-substitute (or ~expr ~@exprs))))

  (defmacro maybe-content
    ([expr] `(if-let [x# ~expr] (html/content x#) identity))
    ([expr & exprs] `(maybe-content (or ~expr ~@exprs))))

  (sm/defn encode :- s/Str
    "Encodes the string in UTF-8 url format"
    [s :- s/Str]
    (url/url-encode s))

  (sm/defn decode :- s/Str
    "Decodes the string in UTF-8 url format"
    [s :- s/Str]
    (url/url-decode s))

  (sm/defn remove-leading-zeroes :- s/Str
    "Removes all leading zeroes. If the string contains only zeroes, returns 0."
    [s :- s/Str]
    (.replaceFirst s "^0+(?!$)" ""))

  (sm/defn capitalize-first-only :- s/Str
    "Like string/capitalize but doesnt force the rest of the string to
  lower case. Useful in cases like hyphenated last names so we dont
  force to second part to lowercase."
    [s :- s/Str]
    (str (.toUpperCase (subs s 0 1))
         (subs s 1 )))

  (sm/defn currency-str->pennies :- (s/maybe s/Int)
    [price :- (s/maybe s/Str)]
    (when price
      (-> (st/replace price #"\$" "")
          (str-to-double)
          (double->pennies))))

  (sm/defn hexadecimalize :- s/Str
    "Converts byte array to hex string"
    [a-byte-array]
    (->> (map #(format "%02X" %) a-byte-array)
         (apply str)
         (st/lower-case)))

  (defn generate-secure-token
    "email authentication stuff (from noir-auth-app)"
    [size & {:keys [hex?]}]
    ;; http://clojuredocs.org/clojure_core/clojure.core/byte-array
    (let [seed (byte-array size)]
      ;; http://docs.oracle.com/javase/6/docs/api/java/security/SecureRandom.html
      (.nextBytes (SecureRandom/getInstance "SHA1PRNG") seed)
      (if hex?
        (hexadecimalize seed)
        seed))))

#+cljs
(do
  (defn to-clj
    "Parses a javascript item with proper keywords."
    [item]
    (js->clj item :keywordize-keys true))

  (defn print-object [o]
    (println (goog.debug/deepExpose o)))

  (defn log [& strings]
    (.log js/console (apply str strings)))

  (sm/defn read :- (s/maybe s/Any)
    [s :- (s/maybe s/Str)]
    (when (not-empty s)
      (cljs.reader/read-string s)))

  (defn ^:export now
    "Returns the current UTC time (since epoch) in ms."
    []
    (js/Date.now))

  (defn ^:export get-target-attr
    "Takes in an HTML DOM event, and returns the value of the given
     attribute for the event's target element. Useful for listeners."
    [event attr]
    (-> (.-selectedTarget event)
        (.-attributes)
        (.getNamedItem attr)
        (.-value)))

  (defn ^:export get-target-value
    "Takes in an HTML DOM event, and returns the value of the event's
  target element. Useful for listeners."
    [event]
    (.-value (.-selectedTarget event)))

  (defn local-time-str
    "Takes in a timestamp in ms, and returns locale time str (ie
    1:02:23 PM)"
    [ms]
    (.toLocaleTimeString (js/Date. ms))))
