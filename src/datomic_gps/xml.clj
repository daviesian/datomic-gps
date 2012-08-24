(ns datomic-gps.xml
  (:use [datomic.api :only [q db] :as d]
        [clojure.pprint]
        [clojure.xml]))

(def xml-schema
  [{:db/id (d/tempid :db.part/db)
    :db/ident :xml/tag
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :xml/attribute
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :xml/child
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :xml/value
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :xml.attribute/name
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :xml.attribute/value
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}])

(def xml-rules
  '[[(attrval ?node ?attr-name ?val)
     [?node :xml/attribute ?a-node]
     [?a-node :xml.attribute/name ?attr-name]
     [?a-node :xml.attribute/value ?val]]

    [(childval ?node ?childtag ?val)
     [?node :xml/child ?c]
     [?c :xml/tag ?childtag]
     [?c :xml/value ?val]]])

;; This is great for small XML structures.
;; Unfortunately, datomic seems to choke on a single 300Mb transaction.
(defn xml->tx
  ([xml] (xml->tx xml nil))
  ([xml parent-id]
     (let [new-id (d/tempid :db.part/user)]
       (if (map? xml )
         (take-while identity (apply concat [(merge {:db/id new-id
                                                     :xml/tag (:tag xml)}
                                                    (when parent-id
                                                      {:xml/_child parent-id}))]
                                     (concat
                                      (when (:attrs xml)
                                        [(map (fn [[attr value]]
                                                {:db/id (d/tempid :db.part/user)
                                                 :xml.attribute/name attr
                                                 :xml.attribute/value value
                                                 :xml/_attribute new-id}) (:attrs xml))])
                                      (map #(xml->tx % new-id) (:content xml)))))
         [[:db/add parent-id :xml/value xml]]))))


(defn transact-xml
  ([conn xml] (transact-xml conn xml nil))
  ([conn xml parent-id]
     (let [tid (d/tempid :db.part/user)
           t @(d/transact conn (apply concat [(merge {:db/id tid
                                                :xml/tag (:tag xml)}
                                               (when parent-id
                                                 {:xml/_child parent-id}))]
                                       (when (:attrs xml)
                                           [(map (fn [[attr value]]
                                                {:db/id (d/tempid :db.part/user)
                                                 :xml.attribute/name attr
                                                 :xml.attribute/value value
                                                 :xml/_attribute tid}) (:attrs xml))])))
           actual-id (d/resolve-tempid (:db-after t) (:tempids t) tid)]
       (doseq [c (:content xml)]
         (if (map? c)
           (transact-xml conn c actual-id)
           @(d/transact conn [[:db/add actual-id :xml/value c]]))))))
