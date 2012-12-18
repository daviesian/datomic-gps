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

(defn deg->rad [degrees]
  (* (/ Math/PI 180) degrees))

(defn distance-between [tp1 tp2]
  (let [r     6371000
        d-lat (deg->rad (- (:lat tp1) (:lat tp2)))
        d-lon (deg->rad (- (:lon tp1) (:lon tp2)))
        a     (+ (* (Math/sin (/ d-lat 2)) (Math/sin (/ d-lat 2)))
                 (* (Math/cos (deg->rad (:lat tp1))) (Math/cos (deg->rad (:lat tp2)))
                    (Math/sin (/ d-lon 2)) (Math/sin (/ d-lon 2))))
        c     (* 2 (Math/asin (min 1 (Math/sqrt a))))
        d     (* r c)]
    d))


(defn with-distance [trkpts]
  (drop 1 (reductions (fn [a b]
                        (assoc b :distance (if (:distance a)
                                             (+ (distance-between a b) (:distance a))
                                             0)))
                      {}
                      trkpts)))
