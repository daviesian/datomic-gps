(ns datomic-gps.core
  (:use [datomic.api :only [q db] :as d]
        [datomic-gps.helpers]
        [datomic-gps.xml]
        [datomic-gps.gpx]
        [datomic-gps.worldwind]
        [datomic-gps.tracks]
        [datomic-gps.app-state]
        [clojure.pprint]
        [clojure.xml]))

;; Start transactor

(def transactor-started (promise))

(future

  (let [datomic-dir "c:\\dev\\datomic-free-0.8.3655\\"
        transactor (.exec (Runtime/getRuntime)
                          (str datomic-dir "bin\\transactor.cmd " datomic-dir "config\\samples\\free-transactor-template.properties")
                          nil
                          (java.io.File. datomic-dir))]
    (println ">>> Transactor running.") ;; Doesn't work in NREPL right now.
    (deliver transactor-started true)
    (.waitFor transactor)
    (println ">>> Transactor died.")))

(println "Transactor Running: " @transactor-started)

;; Init database
(def uri "datomic:mem://xml")
(def uri "datomic:free://localhost:4334/gpx")


;;(try (d/delete-database uri) (catch RuntimeException e))
(d/create-database uri)

(reset! conn (d/connect uri))


(def t (transact @conn xml-schema))
(def t (transact @conn gpx-schema))
(def t (transact @conn gpx-fns))

(create-worldwind)

(comment
  ;; Load data into database


  (def gpx-root-entity (import-gpx-file @conn "sample.gpx"))

  ;; Play with data in database

  ;; Find all trkpt lat/lon/time tuples
  (pprint  (q '[:find ?tp ?lat ?lon ?time
                :in $ %
                :where
                [?tp :xml/tag :trkpt]
                (attrval ?tp ?a-name1 ?lat) [(= ?a-name1 :lat)]
                (attrval ?tp ?a-name2 ?lon) [(= ?a-name2 :lon)]
                (childval ?tp ?cn1 ?time) [(= ?cn1 :time)]]
              (db @conn)
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
                 (db @conn)
                 xml-rules))


  ;; Now load some huge data

  (time
   (def gpx-root-entity (import-gpx-file @conn "C:\\Users\\ipd21\\Documents\\My Dropbox\\GPX Tracks\\2010-11-06 (Atlantis).gpx")))



  (pprint gpx-entities)

  ;; Load and display some stuff

  (def trk (nth (tracks @conn (:gpx (second gpx-entities))) 2))

  (def pts (trackpoints @conn trk))

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
 )
