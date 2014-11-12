# RaceHub Util [![Build Status](https://secure.travis-ci.org/racehub/util.png)](http://travis-ci.org/racehub/util)

General utilities used by the RaceHub team.

## Usage

Here's the Leiningen dependency:

[![Clojars Project](http://clojars.org/racehub/util/latest-version.svg)](http://clojars.org/racehub/util)

You know what to do.

### Services Used:
* CDN: Amazon Cloudfront
* Storage: Amazon S3
* Session Store: Redis - [clj-redis-session](https://github.com/wuzhe/clj-redis-session) and [Carmine](https://github.com/ptaoussanis/carmine).
* Database: CouchDB - [Clutch](https://github.com/clojure-clutch/clutch)

### Important Libraries:
* Liberator for Resource Management: [Liberator](https://github.com/ptaoussanis/timbre)
* Logging: [Timbre](https://github.com/ptaoussanis/timbre)
* Authentication / Authorization: [Friend](https://github.com/cemerick/friend)

### Environmental Variables:
* GEONAMES_KEY - To get current timezone, in location.cljx.
* DEFAULT_TIMEZONE - For time.cljx.
* COUCH_URL - Url for your CouchDB.
* DATABASE_NAME - Database name of your CouchDB.
* COUCH_VIEW_DIRECTORY - Where your CouchDB views live.
* COUCH_VIEW_SUFFIX - If you use different view names in production and staging.
* REDIS_URL - Url of your redis service.
* SESSION_STORE - Or :redis by default.
* CRYPTO_KEY - For Base 64 encoding/decoding.

### Service Tokens:
**Facebook**
* FB_APP_SECRET
* FB_CLIENT_ID
* FB_CLIENT_SERVICE

## CLJX Namespaces

### Algebra.cljx
Semigroup protocol with 'plus' implementation - gives you monoids for cljs and clj to "add" maps or vectors.

### AWS.cljx
Utilities for managing image uploads to Amazon's S3, and Cloudfront CDN.

### CouchDB.cljx
Schemas and helper functions related to CouchDB: connecting, creating/deleting/updating documents or attachments, getting documents, querying views, and some helpers for marking the creation or update timestamps for a document, and other utility functions.

Environmental Variables: COUCH_URL, DATABASE_NAME, COUCH_VIEW_SUFFIX (We use different views for production and staging, so we can experiment with new views before doing a production release.)

### Location.cljx
Google Locations API

### Schema.cljx
Common schemas that we use in our projects, and helper functions such as 'toggle-optional'

### Time.cljx
[clj-time](https://github.com/clj-time/clj-time) helpers. Include convenience functions for creating timestamps, computing intervals, conversions between various time formats, timezone changes, etc.

### Util.cljx
A smorgasbord of helper functions for clj and cljs, spanning accounting, Enlive, core.async, form validation, map and vector operations, time/date manipulation, string formatting, and more.

### Validation.cljx
Common validators we use, including some Validateur helpers.


## CLJ Namespaces

### Lifecycle.clj
Functions for controlling the lifecycle of an application, deprecating in favor of [Component](https://github.com/stuartsierra/component)

## Session.clj
Functions for managing our Redis session store. Configuration using the REDIS_URL environmental variable, cache expiration, etc.

### Couchdb/Testing.clj
defdbtest is a macro that we use to test our CouchDB Views. You'll need to setup [Erica](https://github.com/benoitc/erica) in order for this to work. You should pass in a vector of design-docs to use with the :required-views option. Make sure that you have a COUCH_URL environmental variable (see CouchDB.cljx above). The macro creates a test database on your current CouchDB, binds the database to *test-database* for the duration of the body, creates the required views in the test database via Erica, evaluates all the tests in the body, and then deletes the test database and returns to your original db configuration.

## Middlewares:
### Bare.clj
If the request starts with "www.", strips it from the request, and forces a permanent (301) redirect. (We use a "bare" domain).

### Config.clj
Injects a :config key into the request, with :mode, :dev?, and :cdn keys.

### UUID.clj
Injects a :uuid into each request; useful for logging purposes. TODO: We don't have to heroku-request-id anymore. (Just generates a random one, which is fine).

### Request_Method.clj
Some of our forms have a "_method" keyword so our Liberator endpoints can process DELETE requests.

## Other:
### Util/Config.clj
Stores all the app config details in *env*. Helpers to get environmental keys as keyword or boolean; convenience macros like `with-env` to execute code with the supplied config. For service configurations, we use the same environmental variables in production and development, but the production ones have a "PROD_" prefix. We supply a `prefixed` macro to execute code with with given keys and prefix.

### Util/Crypto.clj
Encryption and decryption.

### Util/Facebook.clj
Wrapper for Facebook Auth. Needs FB environmental keys mentioned at the top.

### Util/Friend.clj
Helpers for authentication and authorization using Friend. Removing and merging authentications, login helpers, and 'spy-mode', which allows us to see what a given logged in user would see.

### Util/Friend/json.clj
TODO.

### Util/Liberator.clj
Helpers for our liberator resource management. Some base resources for form endpoints (for validation), media type negotiation, and merging resources.

### Util/Log.clj
Logging helpers for [Timbre](https://github.com/ptaoussanis/timbre). Adds in the action, uuid, uri, and current-user into the log. We define log/info, log/error, etc (same as in timbre), but have each of the previously mentioned keywords merge in for each logger.

### Util/Oauth.clj
Helpers for performing oauth. We define liberator resources for the initial handshake, token exchange, and unlinking.

### Util/Ring.clj
Ring helpers for various redirects, and extracting access tokens or referrers from request maps.

### Util/Session.clj
Helpers to add a success! or error! message to the flash.

## Authors

- Sam Ritchie <https://twitter.com/sritchie>
- Dave Petrovics <https://github.com/dpetrovics>

## License

Copyright © 2014 PaddleGuru LLC.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
