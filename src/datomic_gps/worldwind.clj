(ns datomic-gps.worldwind
  (:use [seesaw.core]
        [seesaw.dnd]
        [datomic-gps.gpx]
        [datomic-gps.app-state]
        [datomic-gps.helpers]
        [clojure.pprint])
  (:import [java.awt Dimension Color Point Insets]
           [gov.nasa.worldwind Configuration WorldWind BasicModel]
           [gov.nasa.worldwind.geom LatLon Position]
           [gov.nasa.worldwind.event PositionListener SelectListener SelectEvent]
           [gov.nasa.worldwind.layers RenderableLayer]
           [gov.nasa.worldwind.render Path SurfacePolyline BasicShapeAttributes Material Path$PositionColors ScreenAnnotation]
           [gov.nasa.worldwind.awt WorldWindowGLCanvas]
           [gov.nasa.worldwind.avlist AVKey]
           [gov.nasa.worldwind.layers LatLonGraticuleLayer TerrainProfileLayer]))

(Configuration/setValue AVKey/INITIAL_LATITUDE 52.205)
(Configuration/setValue AVKey/INITIAL_LONGITUDE 0.124)
(Configuration/setValue AVKey/INITIAL_ALTITUDE 10000)

(defn pos [p]
  {:lat (.getDegrees (.getLatitude p))
   :lon (.getDegrees (.getLongitude p))
   :alt (.getAltitude p)})

(defn enable-layer [wwd name]
  (dorun
   (map #(.setEnabled % true)
        (filter #(= name (.getName %))
                (-> wwd .getModel .getLayers)))))


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

(defn get-color [r g b]
  (Color. r g b))

(def get-color-memo (memoize get-color))

(defn rainbow-color [i]
  "i should be between 0 and 1. Returns Red -> Yellow -> Green"
  (let [r (if (< i 0.5) 255 (int (* 255 (* 2 (- 1 i)))))
        g (if (> i 0.5) (int (- 255 (* (- i 0.5) 110))) (int (* 255 (* 2 i))))]
    (get-color-memo r g 0)))

(defn create-path [trkpts]
  (let [max-speed (apply max (map :speed trkpts))
        cs        (to-array (map #(/ (:speed %) max-speed) trkpts))
        colors    (reify Path$PositionColors
                    (getColor [this pos ordinal] (rainbow-color #_(/ (mod ordinal 100) 100) (aget cs ordinal))))
        attrs (BasicShapeAttributes.)]
    (doto attrs
      (.setOutlineWidth 1))
    (doto (Path. (map trkpt->position trkpts))
      (.setValue "path-trkpts" trkpts)
      (.setAltitudeMode WorldWind/CLAMP_TO_GROUND)
      (.setAttributes attrs)
      (.setPositionColors colors)
      (.setShowPositions true)
      (.setShowPositionsScale 10)
      (.setFollowTerrain true))))

(defn create-renderable-layer [renderables]
  (let [layer (RenderableLayer.)]
    (doseq [r renderables]
      (.addRenderable layer r))
    layer))

(defn create-track-layer [trkpts]
  (doto
      (create-renderable-layer [ ;(create-surface-line trkpts 10 java.awt.Color/BLUE)
                                (create-path trkpts)])
    (.setName "GPX TRACK")))

(defn screen-annotation []
  (let [a (ScreenAnnotation. "Hello" (Point. 100 100))]
    (doto (.getAttributes a)
      (.setBackgroundColor (Color. (float 1) (float 1) (float 1) (float 0.8)))
      (.setInsets (Insets. 5 5 5 5))
      (.setCornerRadius 5))
    a))

(defn-bound file-list-drop-handler [world {:keys [data]}]
  (future
    (println "DROP " data)
    (doseq [file data]
      (let [gpx (or (get-gpx-entity-by-filename (.getName file))
                    (import-gpx-file @conn (.getPath file)))]
        (println "GPX ID:" gpx)
        (let [tracks (tracks @conn gpx)]
          (doseq [t tracks]
            (add-layer world (create-track-layer (trackpoints @conn t)))))))))

(defn-bound rollover [event]
  (when-let [pick-obj (.getTopPickedObject event)]
    (let [obj (.getObject pick-obj)]
      (when (and (= (.getEventAction event) (SelectEvent/ROLLOVER))
                 (= Path (type obj)))

        (let [trkpts (.getValue obj "path-trkpts")]
          (when-let [ordinal (.getValue pick-obj "gov.nasa.worldwind.avkey.Ordinal") ]
            (pprint (nth trkpts ordinal))))

        ;;(pprint *out*)
        ;;(pprint (bean (second (.getObjects event))))
        ;;(pprint (str "****" (.hasObjects event)))
        ;;(pprint (.getEntries pick-obj))
        (flush)))))

(defn create-worldwind []
  (let [world           (doto (WorldWindowGLCanvas.)
                          (.setModel (BasicModel.)))
        window          (frame :title "Datomic WorldWind"
                               :content (border-panel :center world)
                               :size [800 :by 600]
                               :resizable? true
                               :on-close :dispose)
        listener        (reify PositionListener
                          (moved [this newPos]
                            (let [newPos (pos (.getPosition newPos))]
                              (config! window :title (str "Datomic WorldWind | "
                                                          (format "Lat: %.4f\u00B0, Lon: %.4f\u00B0"
                                                                  (:lat newPos)
                                                                  (:lon newPos)))))))

        select-listener (reify SelectListener
                          (selected [this event]
                            (#'rollover event)))

        ]
    (enable-layer world "MS Virtual Earth Aerial")
    (.addPositionListener world listener)
    (.addSelectListener world select-listener)
    (.setTransferHandler window (default-transfer-handler :import [file-list-flavor (partial file-list-drop-handler world)]))
    (show! window)
    world))
