(ns datomic-gps.worldwind
  (:use [seesaw.core]
        [clojure.pprint])
  (:import [java.awt Dimension Color]
           [gov.nasa.worldwind Configuration WorldWind BasicModel]
           [gov.nasa.worldwind.geom LatLon Position]
           [gov.nasa.worldwind.event PositionListener SelectListener]
           [gov.nasa.worldwind.layers RenderableLayer]
           [gov.nasa.worldwind.render Path SurfacePolyline BasicShapeAttributes Material Path$PositionColors]
           [gov.nasa.worldwind.awt WorldWindowGLCanvas]
           [gov.nasa.worldwind.avlist AVKey]
           [gov.nasa.worldwind.layers LatLonGraticuleLayer TerrainProfileLayer]))

(Configuration/setValue AVKey/INITIAL_LATITUDE 52.205)
(Configuration/setValue AVKey/INITIAL_LONGITUDE 0.124)
(Configuration/setValue AVKey/INITIAL_ALTITUDE 7000)

(defn pos [p]
  {:lat (.getDegrees (.getLatitude p))
   :lon (.getDegrees (.getLongitude p))
   :alt (.getAltitude p)})

(defn enable-layer [wwd name]
  (dorun
   (map #(.setEnabled % true)
        (filter #(= name (.getName %))
                (-> wwd .getModel .getLayers)))))

(defn create-worldwind []
  (let [world    (doto (WorldWindowGLCanvas.)
                   (.setModel (BasicModel.)))
        window   (frame :title "Datomic WorldWind"
                        :content (border-panel :center world)
                        :size [800 :by 600]
                        :resizable? true
                        :on-close :dispose)
        listener (reify PositionListener
                   (moved [this newPos]
                     (let [newPos (pos (.getPosition newPos))]
                       (config! window :title (str "Datomic WorldWind | "
                                                   (format "Lat: %.4f\u00B0, Lon: %.4f\u00B0"
                                                           (:lat newPos)
                                                           (:lon newPos)))))))
        select-listener (reify SelectListener
                          (selected [this event]
                            (pprint (bean (.getTopPickedObject event)))))
        ]
    (enable-layer world "MS Virtual Earth Aerial")
    (.addPositionListener world listener)
    (.addSelectListener world select-listener)
    (show! window)
    world))

(defn add-layer [ww layer]
  (let [layers (.getLayers (.getModel ww))]
    (.add layers layer))
  layer)

(defn remove-layer [ww layer]
  (let [layers (.getLayers (.getModel ww))]
    (.remove layers layer)))


;(def world (create-worldwind))

(defn trkpt->latlon [trkpt]
  (LatLon/fromDegrees (:lat trkpt) (:lon trkpt)))

(defn trkpt->position [trkpt]
  (Position. (LatLon/fromDegrees (:lat trkpt) (:lon trkpt)) (:ele trkpt)))

(defn create-surface-line [trkpts width color]
  (let [attrs (BasicShapeAttributes.)]
    (.setOutlineWidth attrs width)
    (.setOutlineMaterial attrs (Material. color))
    (SurfacePolyline. attrs (map trkpt->latlon trkpts))))

(defn create-path [trkpts]
  (let [colors (reify Path$PositionColors
                 (getColor [this pos ordinal] (Color. 255 0 128)))]
    (doto (Path. (map trkpt->position trkpts))
      (.setAltitudeMode WorldWind/CLAMP_TO_GROUND)
      (.setPositionColors colors)
      (.setFollowTerrain true)
      (.setShowPositions true)
      (.setShowPositionsThreshold 200))))

(defn create-renderable-layer [renderables]
  (let [layer (RenderableLayer.)]
    (doseq [r renderables]
      (.addRenderable layer r))
    layer))

(defn create-track-layer [trkpts]
  (create-renderable-layer [;(create-surface-line trkpts 10 java.awt.Color/BLUE)
                            (create-path trkpts)]))
