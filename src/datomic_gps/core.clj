(ns datomic-gps.core
  (:use [datomic.api :only [q db] :as d]
        [datomic-gps.helpers]
        [datomic-gps.xml]
        [clojure.pprint]
        [clojure.xml]))


;; Init database

(def uri "datomic:mem://xml")
;;(def uri "datomic:free://localhost:4334/xml")

(d/delete-database uri)
(d/create-database uri)

(def conn (d/connect uri))

(def t (transact conn xml-schema))


;; Load data into database

(def xml (parse  "sample.gpx"))

(transact-xml conn xml)

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

(def huge-xml (parse "d:\\Dropbox\\GPX Tracks\\2010-10-12 (Lakes and Home for Helen's Birthday).gpx"))

(time
 (def gpx-root-entity (transact-xml conn huge-xml)))

(def first-trk (first (tracks conn gpx-root-entity))))

(def pts (trackpoints conn first-trk))

(def trk-start-time (:time (first pts)))
(def trk-end-time (:time (last pts)))

(def duration-secs (/ (- (.getTime trk-end-time) (.getTime trk-start-time)) 1000))
