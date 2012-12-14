(ns datomic-gps.dnd
  (:use [seesaw.dnd]
        [datomic-gps.gpx]
        [datomic-gps.app-state]
        [datomic-gps.worldwind]))

(def last-dropped (atom nil))

(let [out *out*]
  (defn file-list-drop-handler [world {:keys [data]}]
    (future
      (binding [*out* out]
        (println "DROP " data)
        (doseq [file data]
          (let [gpx (or (get-gpx-entity-by-filename (.getName file))
                        (import-gpx-file @conn (.getPath file)))]
            (println "GPX ID:" gpx)
            (let [tracks (tracks @conn gpx)]
              (doseq [t tracks]
                (add-layer world (create-track-layer (trackpoints @conn t)))))))))))
