(defproject datomic-gps "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-localrepo "0.4.1"]]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [com.datomic/datomic-free "0.8.3551"]
                 [seesaw "1.4.2"]
                 [gov.nasa/worldwind "1.4.0"]
                 [gov.nasa/worldwindx "1.4.0"]
                 [gov.nasa/jogl "1.4.0"]
                 [gov.nasa/gluegen-rt "1.4.0"]
                 [gov.nasa/gdal "1.4.0"]]
  :jvm-opts ["-Xmx1024M" "-Djava.library.path=worldwind-1.4.0"])
