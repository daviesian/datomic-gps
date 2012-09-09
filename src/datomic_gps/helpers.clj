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

(let [time-format (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'")]
  (defn parse-time [s]
    (.parse time-format s)))

(defn retract-entity [db e]
  (if (number? e)
    (retract-entity db (d/entity db e))
    (let [as (keys e)]
      (apply concat
             (for [a as]
               (let [v          (a e)
                     many?      (coll? v)
                     ref?       (instance? datomic.query.EntityMap (if many? (first v) v))
                     component? (:db/isComponent (d/entity db a))]
                 (if many?
                   (concat
                    (for [vv v]
                      [:db/retract (:db/id e) a (if ref? (:db/id vv) vv)])
                    (when (and ref? component?)
                      (apply concat (for [child v] (retract-entity db child)))))
                   (concat [[:db/retract (:db/id e) a (if ref? (:db/id v) v)]]
                           (when (and ref? component?)
                             (retract-entity db v))))))))))
