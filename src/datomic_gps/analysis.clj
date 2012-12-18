(ns datomic-gps.analysis
  (:use [incanter.core]
        [incanter.charts]
        [incanter.stats]
        [datomic-gps.app-state]
        [datomic-gps.tracks])
  (:import [be.ac.ulg.montefiore.run.jahmm ObservationVector ObservationReal Hmm OpdfGaussianFactory ]
           [be.ac.ulg.montefiore.run.jahmm.learn BaumWelchScaledLearner ]
           [java.util ArrayList]))

(def trkpt-indices (range (count @last-dropped-trkpts)))
(def speeds (map :speed @last-dropped-trkpts))
(def distances (map :distance @last-dropped-trkpts))

(def distances-from-start (map (partial distance-between (first @last-dropped-trkpts)) @last-dropped-trkpts))
(def distances-from-zero (map (partial distance-between {:lat 0 :lon 0}) @last-dropped-trkpts))

(def times (map #(.getTime (:time %)) @last-dropped-trkpts))
(def moving (take (count times) (concat (repeat 1136 20) (repeat 10610 0) (repeat 870 20) (repeat 3000 0))))
(defn first-difference [vals]
  (drop 1 (map first (reductions (fn [a b] [(- b (second a)) b]) [0 0] vals))))

(let [diff       (first-difference (map :distance @last-dropped-trkpts))
      ;;diff       (map #(* 50 %) diff)
      plot       (xy-plot times
                          diff)]
  ;;(add-lines plot times moving)
  ;;(add-lines plot trkpt-indices (map :distance @last-dropped-trkpts))
  ;;(add-lines plot trkpt-indices (map (partial * 15) speeds))
  (add-lines plot times (map - (map (partial * 15) speeds) diff))
  ;;(set-y-range plot (apply min distances-from-start) (apply max distances-from-start))
  (view plot))

(view (xy-plot trkpt-indices (map :distance @last-dropped-trkpts)))


(let [vals          (map - (map (partial * 15) speeds) (first-difference distances))

      hmm           (doto (Hmm. 2 (OpdfGaussianFactory.))
                      (.setPi 0 0.5)
                      (.setPi 1 0.5)

                      (.setOpdf 0 (OpdfGaussian. 30 57))
                      (.setOpdf 1 (OpdfGaussian. 283 117))

                      (.setAij 0 0 0.9999)
                      (.setAij 0 1 0.0001)
                      (.setAij 1 1 0.9999)
                      (.setAij 1 0 0.0001))

      obs           (map #(ObservationReal. %) vals)

      _             (println hmm)
      bwl           (BaumWelchScaledLearner.)
      ;;hmm           (.iterate bwl hmm (partition-all 2000 obs))
      _             (println hmm)

      mostLikelySeq (seq (.mostLikelyStateSequence hmm obs))




      plot          (xy-plot times vals)
      ]

  (add-lines plot times (map #(* % 200) mostLikelySeq))
  ;;(println (seq mostLikelySeq))

  (view plot)
  )
