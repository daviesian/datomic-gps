(ns datomic-gps.core
  (:use [datomic.api :only [q db] :as d]
        [datomic-gps.helpers]
        [datomic-gps.xml]
        [datomic-gps.gpx]
        [clojure.pprint]
        [clojure.xml]))


;; Init database

;;(def uri "datomic:mem://xml")
(def uri "datomic:free://localhost:4334/gpx")


(try (d/delete-database uri) (catch RuntimeException e))
(d/create-database uri)

(def conn (d/connect uri))


(def t (transact conn xml-schema))
(def t (transact conn gpx-schema))
(def t (transact conn gpx-fns))

;; Load data into database


(def gpx-root-entity (import-gpx-file conn "sample.gpx"))

;; Play with data in database

;; Find all trkpt lat/lon/time tuples
(pprint  (q '[:find ?tp ?lat ?lon ?time
              :in $ %
              :where
              [?tp :xml/tag :trkpt]
              (attrval ?tp ?a-name1 ?lat) [(= ?a-name1 :lat)]
              (attrval ?tp ?a-name2 ?lon) [(= ?a-name2 :lon)]
              (childval ?tp ?cn1 ?time) [(= ?cn1 :time)]]
            (db conn)
            xml-rules))



;; Find all trkpt lat/lon/times, but using the macro.

(pprint (query [:find ?tp ?lat ?lon ?time ?speed
                :in $ %
                :where
                [?tp :xml/tag :trkpt]
                (attrval ?tp ?a-name1 ?lat) [(= ?a-name1 :lat)]
                (attrval ?tp ?a-name2 ?lon) [(= ?a-name2 :lon)]
                (childval ?tp ?cn1 ?time) [(= ?cn1 :time)]
                (childval ?tp ?cn2 ?speed) [(= ?cn2 :speed)]]
               (db conn)
               xml-rules))


;; Now load some huge data

(time
 (def gpx-root-entity (import-gpx-file conn "d:\\Dropbox\\GPX Tracks\\2010-10-12 (Lakes and Home for Helen's Birthday).gpx")))



(def first-trk (first (tracks conn gpx-root-entity)))

(def pts (trackpoints conn first-trk))

(def trk-start-time (:time (first pts)))
(def trk-end-time (:time (last pts)))

(def duration-secs (/ (- (.getTime trk-end-time) (.getTime trk-start-time)) 1000))
