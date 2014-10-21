(ns racehub.util.session
  (:require [noir.session :as session]))

;; ## Flash Modification

(defn error! [& strings]
  (when (bound? #'session/*noir-flash*)
    (session/flash-put! :flash ["error" (apply str strings)])))

(defn success! [& strings]
  (when (bound? #'session/*noir-flash*)
    (session/flash-put! :flash ["success" (apply str strings)])))
