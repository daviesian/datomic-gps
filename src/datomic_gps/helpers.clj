(ns datomic-gps.helpers
  (:use [datomic.api :only [q db] :as d]
        [clojure.pprint]
        [seesaw.core]))

(native!)


;; A macro to associate returned values with their variable names in the query.
;; Works just like (datomic.api/q ...) except datalog should not be quoted.
(defmacro query [datalog & sources]
  (let [find (first datalog)
        vars (take-while #(not (keyword? %)) (drop 1 datalog))
        rest (drop (+ 1 (count vars)) datalog)]
    `(try (map (fn [matches#] (apply assoc {}
                                    (apply concat
                                           (map (fn [val# var#] [var# val#])
                                                matches#
                                                '~(map #(keyword (clojure.string/replace (str %) "?" "")) vars)))))
               (q '[~find ~@vars ~@rest] ~@sources))
          (catch Exception e# (pprint e#)))))


;; NOTE: transact fails if you give it an enormous seq. Maybe.
(defn transact [conn tx-data]
  (try
    @(d/transact conn tx-data)
    (catch Exception e (pprint e))
    (catch Error e (pprint e))))

(defmacro defn-db [name ident [& args] & body]
  `(def ~name {:db/id (d/tempid :db.part/db)
              :db/ident ~ident
              :db/fn (d/function '{:lang "clojure"
                                   :params ~args
                                   :code (do ~@body)})}))



(defn parse-date [s]
  (read-string (str "#inst \"" s "\"")))

(defn tracks [conn gpx-id]
  (map :trk-id (query [:find ?trk-id
               :in $ ?gpx-id
               :where
               [?gpx-id :xml/child ?trk-id]
               [?trk-id :xml/tag :trk]]
              (db conn)
              gpx-id)))

(defn trackpoints [conn trk-id]
  (sort-by :time
           (map #(assoc %
                   :time (parse-date (:time %))
                   :lat (Double/parseDouble (:lat %))
                   :lon (Double/parseDouble (:lon %))
                   :ele (Double/parseDouble (:ele %))
                   :speed (Double/parseDouble (:speed %)))
                (query [:find ?trkpt ?lat ?lon ?time ?speed ?ele
                        :in $ ?trk
                        :where
                        [?trk :xml/child ?trkseg] [?trkseg :xml/child ?trkpt]
                        [?trkpt :xml/attribute ?a1] [?a1 :xml.attribute/name :lat] [?a1 :xml.attribute/value ?lat]
                        [?trkpt :xml/attribute ?a2] [?a2 :xml.attribute/name :lon] [?a2 :xml.attribute/value ?lon]
                        [?trkpt :xml/child ?t1] [?t1 :xml/tag :time] [?t1 :xml/value ?time]
                        [?trkpt :xml/child ?t2] [?t2 :xml/tag :ele] [?t2 :xml/value ?ele]
                        [?trkpt :xml/child ?t3] [?t3 :xml/tag :speed] [?t3 :xml/value ?speed]]
                       (db conn)
                       trk-id))))

(defn ^:dynamic *worker-monitor*)

(defn monitor-worker [desc]
  (let [callback-data     (atom {:percent-done 0
                                 :message ""})

        p-bar             (progress-bar :paint-string? false :indeterminate? true)
        f                 (frame :title desc
                                 :content p-bar
                                 :size [640 :by 70]
                                 :resizable? false
                                 :visible? true
                                 :on-close :dispose)
        progress-callback (fn
                            ([] (remove-watch callback-data :monitor)
                               (println "Finished.")
                               (hide! f))
                            ([percent-done message]
                               (reset! callback-data {:percent-done percent-done
                                                      :message message})))]

    (add-watch callback-data :monitor (fn [k r old new]
                                        (let [v (:percent-done new)]
                                          (config! p-bar :indeterminate? (= v -1))
                                          (config! p-bar :paint-string? (not= v -1))
                                          (config! p-bar :value v)
                                          (config! f :title (:message new)))
                                        ;;(println (:message new) "  |  " (:percent-done new) "%")
                                        ))

    (println)
    (println desc)
    progress-callback))
