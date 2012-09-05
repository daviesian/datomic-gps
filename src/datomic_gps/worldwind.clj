(ns datomic-gps.worldwind
  (:use [seesaw.core]
        [clojure.pprint])
  (:import [java.awt Dimension]
           [gov.nasa.worldwind Configuration WorldWind BasicModel]
           [gov.nasa.worldwind.geom LatLon]
           [gov.nasa.worldwind.layers RenderableLayer]
           [gov.nasa.worldwind.render SurfacePolyline]
           [gov.nasa.worldwind.awt WorldWindowGLCanvas]
           [gov.nasa.worldwind.avlist AVKey]
           [gov.nasa.worldwind.layers LatLonGraticuleLayer TerrainProfileLayer]))


(defn create-worldwind []
  (let [world (doto (WorldWindowGLCanvas.)
                (.setModel (BasicModel.)))]
    (pprint (type world))
    (show! (frame :title "Datomic WorldWind"
                  :content (border-panel :center world)
                  :size [800 :by 600]
                  :resizable? true
                  :on-close :dispose))
    world))

(defn add-layer [ww layer]
  (let [layers (.getLayers (.getModel ww))]
    (.add layers layer)))

;(def world (create-worldwind))

(defn trkpt->latlon [trkpt]
  (LatLon/fromDegrees (:lat trkpt) (:lon trkpt)))

(defn create-surface-line [trkpts]
  (SurfacePolyline. (map trkpt->latlon trkpts)))

(defn create-renderable-layer [renderables]
  (let [layer (RenderableLayer.)]
    (doseq [r renderables]
      (.addRenderable layer r))
    layer))
