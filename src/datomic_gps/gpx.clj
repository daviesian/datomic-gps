(ns datomic-gps.gpx
  (:use [datomic.api :only [q db] :as d]
        [datomic-gps.helpers]
        [datomic-gps.xml]
        [clojure.pprint]
        [clojure.xml])
  (:import [java.io.File]
           [java.util.Date]))

(def gpx-schema
  [{:db/id (d/tempid :db.part/db)
    :db/ident :gpx/fileName
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :gpx/fileModifiedTime
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :gpx.track/startTime
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :gpx.track/endTime
    :db/valueType :db.type/instant
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}])

(defn-db add-inst-attribute :gpx/addInstAttribute [db entity attribute time-string]
  [[:db/add entity attribute (read-string (str "#inst \"" time-string "\""))]])

(defn-db cache-track-details :gpx/cacheTrackDetails [db gpx-id]
  (let [tracks (map first (q '[:find ?trk :in $ ?gpx-id
                               :where
                               [?gpx-id :xml/child ?trk]
                               [?trk :xml/tag :trk]] db gpx-id))]
    (apply concat (map (fn [trk-id]
                         (let [pts (sort (map first (q '[:find ?time :in $ ?trk-id
                                                         :where
                                                         [?trk-id :xml/child ?trkseg]
                                                         [?trkseg :xml/tag :trkseg]
                                                         [?trkseg :xml/child ?trkpt]
                                                         [?trkpt :xml/tag :trkpt]
                                                         [?trkpt :xml/child ?time-tag]
                                                         [?time-tag :xml/tag :time]
                                                         [?time-tag :xml/value ?time]] db trk-id)))
                               start-time (first pts)
                               end-time (last pts)]
                           [[:gpx/addInstAttribute trk-id :gpx.track/startTime start-time]
                            [:gpx/addInstAttribute trk-id :gpx.track/endTime end-time]]))
                       tracks))))

(def gpx-fns
  [add-inst-attribute
   cache-track-details])

(defn import-gpx-file [conn file-name]
  (binding [*inserted-tag-count* (atom 0)
            *worker-monitor* (monitor-worker "Importing GPX File...")]
    (when *worker-monitor*
      (#'*worker-monitor* -1 "Reading XML"))
    (let [xml             (parse file-name)
          tag-count       (count-tags xml)
          file            (java.io.File. file-name)

          progress-logger (future
                            (when *worker-monitor*
                              (#'*worker-monitor* 0 "Inserting XML"))
                            (dorun (repeatedly
                                    #(do
                                       (Thread/sleep 1000)
                                       (when *worker-monitor*
                                         (#'*worker-monitor* (int (* 100 (/ @*inserted-tag-count* tag-count)))
                                                             (str "Inserting XML")))))))
          gpx-id          (try (batch-transact-xml conn 1000 [[nil [xml]]])
                               (finally (future-cancel progress-logger)))]
      (when *worker-monitor*
        (#'*worker-monitor* -1 "Caching track details"))
      (transact conn [[:db/add gpx-id :gpx/fileName (.getName file)]
                      [:db/add gpx-id :gpx/fileModifiedTime (java.util.Date. (.lastModified file))]
                      [:gpx/cacheTrackDetails gpx-id]])
      (when *worker-monitor*
        (#'*worker-monitor* -1 "Done")
        (#'*worker-monitor*))
      gpx-id)))


(defn export-gpx-to-file [db gpx-id path]
  (let [xml     (extract-xml db gpx-id)
        xml-str (with-out-str (xml->str xml))]
    (spit path xml-str)))

(defn export-gpx-to-dir [db gpx-id path]
  (let [path       (if (.endsWith path "\\") path (str path "\\"))
        gpx-entity (d/entity db gpx-id)
        file-name  (:gpx/fileName gpx-entity)
        full-path  (str path file-name)]
    (when-not file-name
      (throw (Exception. "GPX node was not imported from file and no name specified.")))
    (export-gpx-to-file db gpx-id full-path)
    (when (:gpx/fileModifiedTime gpx-entity)
      (.setLastModified (java.io.File. full-path) (.getTime (:gpx/fileModifiedTime gpx-entity))))))
