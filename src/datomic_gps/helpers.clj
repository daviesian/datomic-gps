(ns datomic-gps.helpers
  (:use [datomic.api :only [q db] :as d]
        [clojure.pprint]))

;; A macro to associate returned values with their variable names in the query.
;; Works just like (datomic.api/q ...) except datalog should not be quoted.
(defmacro query [datalog & sources]
  (let [find (first datalog)
        vars (take-while #(not (keyword? %)) (drop 1 datalog))
        rest (drop (+ 1 (count vars)) datalog)]
    `(map (fn [matches#] (apply assoc {}
                               (apply concat
                                      (map (fn [val# var#] [var# val#])
                                           matches#
                                           '~(map #(keyword (clojure.string/replace (str %) "?" "")) vars)))))
          (q '[~find ~@vars ~@rest] ~@sources))))


;; NOTE: transact fails if you give it an enormous seq. Maybe.
(defn transact [conn tx-data]
  (try
    @(d/transact conn tx-data)
    (catch Exception e (pprint e))
    (catch Error e (pprint e))))
