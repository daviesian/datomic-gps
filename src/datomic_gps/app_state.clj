(ns datomic-gps.app-state)

(def conn (atom nil))

(def tooltip (atom nil))

(def last-dropped-trkpts (atom nil))
