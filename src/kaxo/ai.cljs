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
    #_(prn "  neighbours " neighbours)
    #_(prn "  boundary " boundary)
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
      #_(prn "regions " regions)
      #_(prn "remaining " remaining)
      (if (empty? remaining)
        regions
        (let [dot (first remaining)
              region (grow-connected-region-in-area remaining wps dot)
              space-left (difference remaining region)]
          (recur (conj regions region) space-left))))))

(defn random-move
  "pick a random valid move from a region"
  [regions]
  (rand-nth (seq (rand-nth regions)))
  )

(defn dimensions
  "calculate the dimensions of a region"
  [region]
  (let [xs (map #(first %) region)
        ys (map #(second %) region)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)
        ]
    (map inc [(- max-x min-x) (- max-y min-y)])))

(def nim-limit 4)

#_(defn nimber
  "calculate (or estimate) the nimber of a region"
  [region]
  (let [c (count region)
        [w h] (dimensions region)])
  (cond
    ;; lines
    (or
        (= w 1) ; vertical line
        (= h 1) ; horizontal line
        (not-any? (fn [[x y] (not= (- x) y)]) region) ; +1 slope
        (not-any? (fn [[x y] (not= (- x) y)]) region) ; -1 slope
        ) c

    ;; rectangles ?
    (= c (* w h)) 0

    (= c 3) 0

    :else c ; c is an upper bound on true nimber
    )
  )

(defn nimber
  "place-holder for testing - pretends every region is a simple row"
  [region]
  (count region))

(defn nimsum
  "find the nimsum of some nimbers"
  [nimbers]
  (reduce bit-xor nimbers))

(defn odd-single-count?
  "returns the count of regions with a nimber of 1"
  [nimbers]
  (odd? (count (filter #(= 1 %) nimbers))))

(defn n-position?
  "is the position a win for the next player?"
  [nimbers misere]
  (if (and misere (not-any? #(> 1 %) nimbers))
    (odd-single-count? nimbers)
    (= 0 (nimsum nimbers))))

(defn p-position?
  "is the position a win for the previous player?"
  [nimbers misere]
  (complement n-position?))

(defn get-ai-move
  "plan an ai move"
  []
  (prn "get-ai-move")
  (let [g @k/game
        n (:n g)
        wps @k/w-points
        regions' (regions n wps)]
    (prn regions')

    (random-move regions')))
