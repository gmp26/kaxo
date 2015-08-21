(ns ^:figwheel-always kaxo.ai
  "ai game play for kaxo"
  (:require [kaxo.core :as k]
            [clojure.set :refer (union difference)]))

(defn initial-dots
  "calculate initial dots in game"
  [n]
  (let [rng (range n)]
    (into #{} (for [x rng y rng] [x y]))))

(defn remaining-dots
  "find remaining dots in game"
  [n wps]
  (difference (initial-dots n) wps))

(defn linkable?
  "could this line link its endpoints?"
  [wps line]
  (not-any? (k/new-way-points (k/canonical-line line)) wps))

(defn get-boundary
  "find boundary of a neighbourhood around a new neighbouring dot within an area"
  [area wps neighbours [x y :as dot]]
  (let [yet-to-explore (difference area neighbours)]
    #_(prn "yet-to-explore " yet-to-explore)
    (into #{}
          (for [x' (range (- x 1) (+ x 2))
                y' (range (- y 1) (+ y 2))
                :let [dot' [x' y']]
                :when (and (not= dot' dot)
                           (yet-to-explore dot')
                           (linkable? wps (k/canonical-line [dot' dot])))]
            dot'))))

(defn grow-connected-region-in-area
  "grow a neighbourhood from a seed"
  [area wps dot]
  (loop [neighbours #{dot}
         boundary (get-boundary area wps neighbours dot)]
    (prn "  neighbours " neighbours)
    (prn "  boundary " boundary)
    (if (empty? boundary)
      neighbours
      (let [candidate (first boundary)
            remainder (into #{} (rest boundary))]
        (recur (conj neighbours candidate)
               (union remainder
                      (get-boundary area wps neighbours candidate)))))))

(defn regions
  "return all connected regions on game board"
  [n wps]
  (let [space (remaining-dots n wps)]
    (loop [regions []
           remaining space]
      (prn "regions " regions)
      (prn "remaining " remaining)
      (if (empty? remaining)
        regions
        (let [dot (first remaining)
              region (grow-connected-region-in-area remaining wps dot)
              space-left (difference remaining region)]
          (recur (conj regions region) space-left))))))

(defn get-ai-move
  "plan an ai move"
  []
  nil)
