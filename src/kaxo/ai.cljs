(ns ^:figwheel-always kaxo.ai
  "ai game play for kaxo"
  (:require [kaxo.core :as k]
            [clojure.set :refer (difference)])
  )

(defonce partitions #{})

(defn initial-dots
  "calculate initial dots in game"
  [n]
  (let [rng (range n)]
    (into #{} (for [x rng y rng] [x y])))
  )

(defn remaining-dots
  "find remaining dots in game"
  [n wps]
  (difference (initial-dots n) wps)
  )

(defn adjacent?
 "are 2 dots adjacent?"
 [[x1 y1 :as dot1] [x2 y2 :as dot2]]
 (and (<= (Math.abs (- x1 x2)) 1) (<= (Math.abs (- y1 y2)) 1))
 )

#_(defn dot-partition
  [n wps dot dots]
  (loop [neighbours #{dot}
         remotes (remaining-dots n wps)]



    recur))





(defn get-ai-move
  "plan an ai move"
  []
  nil)
