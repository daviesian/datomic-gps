(ns datomic-gps.xml
  (:use [datomic.api :only [q db] :as d]
        [datomic-gps.helpers]
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
    :db.install/_attribute :db.part/db}

   {:db/id (d/tempid :db.part/db),
    :db/ident :db.part/xml,
    :db.install/_partition :db.part/db}])

(def xml-rules
  '[[(attrval ?node ?attr-name ?val)
     [?node :xml/attribute ?a-node]
     [?a-node :xml.attribute/name ?attr-name]
     [?a-node :xml.attribute/value ?val]]

    [(childval ?node ?childtag ?val)
     [?node :xml/child ?c]
     [?c :xml/tag ?childtag]
     [?c :xml/value ?val]]])

(defn xml-tag-tx-data [tag temp-id parent-id]
  (apply concat [(merge {:db/id temp-id
                         :xml/tag (:tag tag)}
                        (when parent-id
                          {:xml/_child parent-id})
                        (when (and (first (:content tag)) (not (map? (first (:content tag)))))
                          {:xml/value (first (:content tag))}))]
         (when (:attrs tag)
           [(map (fn [[attr value]]
                   {:db/id (d/tempid :db.part/xml)
                    :xml.attribute/name attr
                    :xml.attribute/value value
                    :xml/_attribute temp-id}) (:attrs tag))])))


;; Takes conn and a list of [parent-id xml-tags]
(defn batch-transact-xml [conn batch-size tags-with-parent-ids]
  ;; Expand into a list of [parent-id tag]
  (let [tags (apply concat (map (fn [[parent-id tags]]
                                  (map (fn [tag] [parent-id tag])
                                       tags))
                                tags-with-parent-ids))]
    tags

    ;; Transform into a new list of [temp-id tx-data children]
    (let [tx-units (map (fn [[parent-id tag]]
                          (let [temp-id (d/tempid :db.part/xml)]
                            [temp-id
                             (xml-tag-tx-data tag temp-id parent-id)
                             (if (map? (first (:content tag))) (:content tag) nil)]))
                        tags)]

      ;; Split new list into chunks
      (let [chunks (partition-all batch-size tx-units)

            ;; Execute a transaction with each chunck of tx-datas
            ;; resolving temp-ids to actual-ids for each chunk
            id-and-children (doall (map (fn [tx-chunk]
                                          (let [tx-data (apply concat (map second tx-chunk))
                                                t (transact conn (take-while identity tx-data))
                                                actual-ids (map (fn [[temp-id _ _]]
                                                                  (d/resolve-tempid (:db-after t) (:tempids t) temp-id)) tx-chunk)
                                                children (filter identity
                                                                 (map (fn [actual-id [_ _ children]]
                                                                        (when children
                                                                          [actual-id children])) actual-ids tx-chunk))]

                                            [(first actual-ids) children]))
                                        chunks))
            first-actual-id (ffirst id-and-children)

            ;; Merge actual-ids and children from each chunk.
            children (apply concat (map second id-and-children))]

        ;; Call once recursively with list of all [actual-id children]
        (when (seq children)
          (batch-transact-xml conn batch-size children))

        ;; Return the id of the first tag that was inserted.
        first-actual-id))))
