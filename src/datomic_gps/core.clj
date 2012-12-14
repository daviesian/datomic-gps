(ns datomic-gps.core
  (:use [datomic.api :only [q db] :as d]
        [datomic-gps.helpers]
        [datomic-gps.xml]
        [datomic-gps.gpx]
        [datomic-gps.worldwind]
        [datomic-gps.tracks]
        [clojure.pprint]
        [clojure.xml]))

(future (println "BEFORE")
        (Thread/sleep 1000)
        (println "AFTER"))
;; Start transactor

(future
  (println "hi")
  (let [datomic-dir "c:\\dev\\datomic-free-0.8.3551\\"
        transactor (.exec (Runtime/getRuntime)
                          (str datomic-dir "bin\\transactor.cmd " datomic-dir "config\\samples\\free-transactor-template.properties")
                          nil
                          (java.io.File. datomic-dir))]
    (println ">>> Transactor running.") ;; Doesn't work in NREPL right now.
    (.waitFor transactor)
    (println ">>> Transactor died.")))


;; Init database
(def uri "datomic:mem://xml")
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
                (attrVal ?tp :lat ?lat)
                (attrVal ?tp :lon ?lon)
                (childVal ?tp :time ?time)
                (childVal ?tp :speed ?speed)]
               (db conn)
               xml-rules))


;; Now load some huge data

(time
 (def gpx-root-entity (import-gpx-file conn "D:\\Dropbox\\GPX Tracks\\2012-09-15 (Sheffield and Bottisham).gpx")))


;; Find all gpx entities

(def gpx-entities (query [:find ?file ?gpx
                :in $ %
                :where
                          [?gpx :gpx/fileName ?file]] (db conn) xml-rules))

(pprint gpx-entities)

;; Load and display some stuff

(def trk (nth (tracks conn (:gpx (second gpx-entities))) 2))

(def pts (trackpoints conn trk))

(def trk-start-time (:time (first pts)))
(def trk-end-time (:time (last pts)))

(def duration-secs (/ (- (.getTime trk-end-time) (.getTime trk-start-time)) 1000))

(do
  (def world (create-worldwind))


  (def layer (add-layer world (create-track-layer pts))))

(remove-layer world layer)



(def layer (add-layer world
                      (create-track-layer
                       (remove-duplicate-trkpts
                        pts))))

(remove-layer world layer)


(defmacro report-task-progress [[report-fn max] & body]
  `(let [current-progress# (atom 0)
         ~report-fn (fn
                      ([] (swap! current-progress# inc))
                      ([progress#] (reset! current-progress# progress#)))]

     (add-watch current-progress# :progress-monitor (fn [k# r# old# new#]
                                                     (println "Progress:" new#)))
     ~@body

     (remove-watch current-progress# :progress-monitor)))

(comment (report-task-progress [p 500]
           (dorun (repeatedly 500 (fn []
                                    (Thread/sleep 15)
                                    ;;(println (System/currentTimeMillis))
                                    (p))))))
