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
  (difference (initial-dots n) wps))

(defn linkable?
  "could this line link its endpoints?"
  [wps line]
  (not-any? (k/new-way-points (k/canonical-line line)) wps))

(defn adjacent?
 "are 2 dots adjacent?"
 [wps [x1 y1 :as dot1] [x2 y2 :as dot2]]
 (let [line [dot1 dot2]
       result (and (<= (Math.abs (- x1 x2)) 1)
                   (<= (Math.abs (- y1 y2)) 1)
                   (not= dot1 dot2)
                   (linkable? wps line))]
   (k/deb (str line) result))
 )

(defn set-adjacent?
  "determine whether a dot is adjacent to any dots in a given set"
  [wps neighbours dot]
  (some #(adjacent? wps % dot) neighbours))

(defn connected
  "the dots in the connected neighbourhood of a given dot"
  [wps dot remaining]
  (loop [neighbours #{dot}
         remotes remaining]
    (prn (str "neighbours " neighbours))
    (prn (str "remotes " remotes))
    (if (empty? remotes)
      neighbours
      (let [candidate (first remotes)
            remainder (rest remotes)]
        (if (set-adjacent? wps neighbours candidate)
          (recur (conj neighbours candidate) remainder)
          (recur neighbours remainder))))))

(defn regions
  "return all connected regions on game board"
  [n wps]
  (let [space (remaining-dots n wps)]
    (loop [regions []
           remaining space]
      (prn (str "regions " regions))
      (prn (str "remaining " remaining))
      (if (empty? remaining)
        regions
        (let [dot (k/deb "dot " (first (seq remaining)))
              foo (k/deb "foo remaining " remaining)
              region (k/deb "let reg " (connected wps dot remaining))
              space-left (difference remaining region)]
          (recur (conj regions region) space-left)
          ))
      ))
  )


(defn get-ai-move
  "plan an ai move"
  []
  nil)
