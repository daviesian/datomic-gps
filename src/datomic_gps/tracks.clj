(ns datomic-gps.tracks
  (:use [clojure.pprint]
        [clojure.core]
        [datomic-gps.helpers]))

(def ^:dynamic *removed-points* (atom nil))

(defn remove-duplicate-trkpts [db trkpts]
  (reduce (fn [pts new-point] (if (and (= (:lat (last pts)) (:lat new-point))
                                      (= (:lon (last pts)) (:lon new-point)))
                               (do
                                 (when @*removed-points*
                                   (swap! *removed-points* #(conj % (:trkpt new-point))))
                                 pts)
                               (conj pts new-point)))
          [] trkpts))
