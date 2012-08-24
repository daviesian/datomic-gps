(ns datomic-gps.core
  (:use [datomic.api :only [q db] :as d]
        [clojure.pprint]
        [clojure.xml])
  (:require [clojure.zip :as zip]))

(def track (clojure.xml/parse "D:\\Dropbox\\GPX Tracks\\2010-05-28 (Punting).gpx"))
(def t-zip (zip/xml-zip track))

(defn zip-to [zipper & path]
  (let [[tag & remaining-path] path]
    (if (= tag (:tag (first zipper)))
      (do
        (if remaining-path
          (apply zip-to (zip/down zipper) remaining-path)
          zipper))
      (do
        (when (zip/right zipper)
          (apply zip-to (zip/right zipper) path))))))

(defn zip-to-content [zipper & path]
  (:content (first (apply zip-to zipper path))))

(defn xml-to-trkpt [xml]
  {:lat (get-in xml [:attrs :lat])
   :lon (get-in xml [:attrs :lon])
   })

(defn sprint [a]
  (print (take 100 (str a))))

(def uri "datomic:mem://my-database")

(d/delete-database uri)
(d/create-database uri)

(def conn (d/connect uri))

(def schema
  [{:db/id (d/tempid :db.part/db)
    :db/ident :person/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :person/age
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id (d/tempid :db.part/db)
    :db/ident :person/landlord
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}])

(try
  (def t @(d/transact conn schema))
  (catch Exception e (pprint e)))

(do
  @(d/transact conn [{:db/id #db/id [:db.part/user -1]
                      :person/name "Ian"
                      :person/age 26}
                     {:db/id #db/id [:db.part/user -2]
                      :person/name "Ben"
                      :person/age 57
                      :person/landlord #db/id [:db.part/user -1]}]))


;; Add a new attribute the long-winded way
(try
  @(d/transact conn (let [tid #db/id [:db.part/db]]
                      [[:db/add tid :db/ident :person/email]
                       [:db/add tid :db/valueType :db.type/string]
                       [:db/add tid :db/cardinality :db.cardinality/one]
                       [:db/add :db.part/db :db.install/attribute tid]]))
  (catch Exception e
    (pprint e)))
