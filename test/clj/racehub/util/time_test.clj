(ns racehub.util.time-test
  (:require [clj-time.core :as time]
            [clojure.test :refer :all]
            [racehub.util.time :refer :all]))

(deftest time-mocking-test
  (let [mocked-time (time/date-time 2014)]
    (with-time mocked-time
      (is (= (time/now) mocked-time)
          "Mocking the time causes the mocked time to be
          returned."))))

(deftest midnight-test
  (let [now (time/now)]
    (is (time/after? (next-midnight now)
                     (midnight now))
        "The next midnight is after this midnight.")
    (is (= (time/plus (midnight now) (time/days 1))
           (next-midnight now))
        "Next midnight is exactly one day ahead of this
          midnight.")))
