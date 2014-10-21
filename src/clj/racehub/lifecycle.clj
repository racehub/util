(ns racehub.lifecycle
  "Functions for controlling the lifecycle of an application.")

(defprotocol LifeCycle
  (start! [this prev]
    "The `prev` arg is services already started at this point.")
  (stop! [this]))

(defn start-system
  "Boots the supplied system (a map of keyword -> LifeCycle enabled
  service) in the order specified by the vector under :order."
  [{:keys [order] :as system}]
  (reduce (fn [acc [identifier service]]
            (start! service acc)
            (assoc acc identifier service))
          {:order order}
          (map (fn [k] [k (system k)]) order)))

(defn stop-system
  "Shuts donw the supplied system (a map of keyword -> LifeCycle
  enabled service) in the REVERSE of the order specified by the vector
  under :order."
  [{:keys [order] :as system}]
  (doseq [s (reverse (map system order))]
    (stop! s)))
