(ns datomic-gps.core
  (:use [datomic.api :only [q db] :as d]
        [datomic-gps.helpers]
        [datomic-gps.xml]
        [clojure.pprint]
        [clojure.xml]))


;; Init database

(def uri "datomic:mem://xml")
(def uri "datomic:free://localhost:4334/xml")

(d/delete-database uri)
(d/create-database uri)

(def conn (d/connect uri))

(def t (transact conn xml-schema))


;; Load data into database

(def xml (parse  "sample.gpx"))

(def data (xml->tx xml))

(def t (transact conn data))


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


;; Now load some big data

(def big-xml (parse "C:\\Users\\ipd21\\Documents\\My Dropbox\\GPX Tracks\\2009-12-18 (Moving House).gpx"))

(def big-data (xml->tx big-xml))

(def t (transact conn big-data))


;; Now load some huge data

(def huge-xml (parse "C:\\Users\\ipd21\\Documents\\My Dropbox\\GPX Tracks\\2010-10-12 (Lakes and Home for Helen's Birthday).gpx"))

(transact-xml conn huge-xml)
;;(def huge-data (xml->tx huge-xml))

;;(def t (transact conn huge-data))
