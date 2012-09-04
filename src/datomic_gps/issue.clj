(use '[datomic.api :only [q db] :as d]) ;'
(use 'clojure.pprint)
(do
  (def uri "datomic:mem://seattle")
  (d/create-database uri)
  (def conn (d/connect uri))

  (def schema-tx (read-string (slurp "samples/seattle/seattle-schema.dtm")))
  @(d/transact conn schema-tx)

  (def data-tx (read-string (slurp "samples/seattle/seattle-data0.dtm")))
  @(d/transact conn data-tx))


(pprint (q '[:find ?e :where
            [?e :community/category "news"]
            [?e :community/category "food"]] (db conn)))

(pprint (q '[:find ?e ?cs1 ?cs2  :in $ % :where
             [hasCategory ?e "news"]
             [hasCategory ?e "food"]
             [allCatsExcept ?e "food" ?cs2]
             [allCatsExcept ?e "news" ?cs1]]
          (db conn)
          '[[[hasCategory ?e ?c]
             [?e :community/category ?c]]
            [[allCatsExcept ?e ?ex ?cs]
             [?e :community/category ?cs]
             [(!= ?ex ?cs)]]]))


(try
  (pprint (map #(nth % 2) (q '[:find ?e ?cs1 ?cs2  :in $ % :where
                           [hasCategory ?e "news"]
                           [hasCategory ?e "food"]
                           [allCatsExcept ?e "food" ?cs1]
                           [allCatsExcept ?e "news" ?cs2]]
                         (db conn)
                         '[[[hasCategory ?e ?c]
                            [?e :community/category ?c]]
                           [[allCatsExcept ?e ?ex ?cs]
                            [?e :community/category ?cs]
                            [(!= ?ex ?cs)]]])))
  (catch Exception e (pprint e)))
