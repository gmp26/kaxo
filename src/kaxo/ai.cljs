(ns ^:figwheel-always kaxo.ai
  "ai game play for kaxo"
  (:require [kaxo.core :as k]
            [kaxo.hum :as hum]
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

;;;
;; To find a good move we need a way to represent the shape of a region, no matter how
;; it is positioned or orientated in the grid.
;; But how?
;;;

(defn random-move
  "pick a random valid move from a region"
  [regions]
  (rand-nth (seq (rand-nth regions)))
  )

;;;
;; Nimber calculations
;;;
(def nim-limit 4)

;; (if (or ; lines
;;       (= w 1) ; vertical line
;;       (= h 1) ; horizontal line
;;       (not-any? (fn [[x y] (not= x y)]) region) ; +1 slope
;;       (not-any? (fn [[x y] (not= (- x) y)]) region) ; -1 slope
;;       )
;;   c)

(defn default-nimber
  "place-holder for testing - pretends every region is a simple row"
  [region]
  (count region))

(def shape-nimbers
  {;; 1 x 1
   "0.000 0.000" 1 ;; single dot
   ;; 2 x 1
   "0.5000 0.2500" 2 ;; 2-line (h/v)
   ;; 2 x 2
   "1.333 0.4444" 0 ;; el
   "2.000 0.000" 1 ;; square
   "1.000 1.000" 2 ;; diagonal
   ;; 3 x 1
   "2.000 4.000" 3 ;; 3-line h/v
   ;; 3 x 2
   "3.000 5.000" 1 ;; zed
   "5.500 6.250" 2 ;; solid rect, also 3 x 3 lambda
   "2.750 1.563" 2 ;; Tee (triangle hypoteneuse as base)
   "4.000 4.000" 5 ;; 3 x 2 child's chair
   "2.667 5.778" 0 ;; sledge ????
   "3.750 4.063" 1 ;; comma
   "3.500 6.250" 4 ;; el
   "2.667 1.778" 3 ;; chevron
   "5.200 7.840" 2 ;; square bracket
   ;; 3 x 3
   "6.400 12.96" 1 ;; el
   "4.750 0.5625" 2 ;; Y
   "5.600 0.1600" 2 ;; glider1
   "6.800 5.440" 2 ;; push-chair
   "8.000 0.000" 2 ;; 5 die
   "5.500 20.25" 3 ;; 3 x 3 funnel
   "4.750 16.56" 4 ;; lawn mower
   "5.200 1.440" 5 ;; Tee
   "4.000 16.00" 2 ;; diagonal
   "6.000 0.8000" 4 ;; glider2
   "4.750 4.563" 1 ;; jay hook
   "4.800 4.640" 2 ;; glider 3
   "6.667 11.11" 1 ;; triangle 0 2 4 5
   "5.667 2.778" nil ;; speed boat - way points needed
   "5.600 2.560" nil ;; 3 x 3 boat - 3 variants of this depending on w-points
   "9.714 0.08163" nil ;; arrow 0 1 2 3
   ;; 4 x 1
   "5.000 25.00" 4 ;; 4-line h/v
   ;; 4 x 2
   "6.000 20.00" 1 ;; 4 x 2 caterpillar
   "8.167 37.36" 3 ;; 4 x 2 high back chair
   "7.600 41.76" 5 ;; 4 x 2 el
   ;; 4 x 3
   "11.50 56.25" 6 ;; el
   ;; 4 x 4
   "17.71 105.8" 0 ;; el

   "10.00 100.0" 5 ;; 5-line h/v
   "19.43 217.5" 0 ;; 5 x 3 el
   "14.17 167.4" 3 ;; 5 x 2 el
   "4.000 0.000" nil ;; open 3 x 3 plus - 2 variants
   "12.00 0.000" nil ;; 3 x 3 square
   "40.00 400.0" nil ;; 3 x 5 rect
   "65.00 225.0" nil ;; 4 x 5 rect
   })


(defn nimber
  "calculate (or estimate) the nimber of a region"
  [region]
  (let [area (count region)
        shape (hum/hu-moments region)])

  (default-nimber region)
  )

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
