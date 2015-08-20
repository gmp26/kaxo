(ns ^:figwheel-always kaxo.core
    (:require [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (union)]
              [cljsjs.react]
              ))

(enable-console-print!)

(defn deb
  "print and return value"
  ([x]
   (prn x) x)

  ([s x]
   (prn (str s x)) x))

(defn el [id] (.getElementById js/document id))

;;;
;; constants
;;;
(def initial-state {:n 4
                    :player :a
                    :players 2
                    :a-crosses #{}
                    :b-crosses #{}
                    :a-lines #{}
                    :b-lines #{}
                    :block false
                    })

(def initial-history [0 [[initial-state #{}]]])

;;;
;; define game state atoms once only so code is reloadable in figwheel
;;;
(defonce game (atom initial-state))
(defonce svg-point (atom false))        ; will be created to calculate svg screen transform
(defonce drag-line (atom [nil 0]))
(defonce w-points (atom #{}))
(defonce history (atom initial-history))

;;;
;; unused
;;;
(defn on-js-reload
  "placeholder for development browser reloaded code"
  []
  (swap! game update-in [:__figwheel_counter] inc))

;;;;;;;;;;;;;;;;;;;;;
;;
;; Game life cycle
;;
(defn real-points-removed [wps]
  (filter (fn [[x y]] (and (integer? x) (integer? y))) wps))

(defn game-over?
  "return :a :b or false"
  [g wps]
  (let [n (:n g)
        r-max (* n n)
        r-count (inc (count (real-points-removed wps)))]
    (cond
     (< r-max r-count) (if (= (:player g) :a) :b :a)
     (= r-max r-count) (:player g)
     :else false)))

(defn get-status
  "derive win/lose/turn status"
  [g wps]
  (let [pa (= (:player g) :a)
        gover (game-over? g wps)
        over-class (if gover " pulsed" "")]
    (if (= (:players g) 1)
      [over-class (cond
                    (= gover :a) :al-win
                    (= gover :b) :you-win
                    :else (if pa :yours :als))]
      [over-class (cond
                    (= gover :a) :b-win
                    (= gover :b) :a-win
                    :else (if pa :as-turn :bs-turn))])))

;;;
;; new line utilities (for on-drag)
;;;
(defn canonical-line
  "convert line to a canonical form for equality comparisons"
  [[p1 p2 :as line]]
  (let [[[x1 y1] [x2 y2]] [p1 p2]]
    (if (or (< x1 x2)
            (and (= x1 x2)
                 (< y1 y2))
            )
      line
      [p2 p1])))

(defn add-way-points
  "find dots traversed by line.
  Include mid-points between dots on diagonal lines.
  A zero slope-type means horizontal or vertical, else it's the line gradient
  Only 0, -1, 1 slope-types are allowed"
  [[[x1 y1] [x2 y2] :as [p1 p2]] slope-type]
  (if (= 0 slope-type)
    (if (= x1 x2)
      (into #{} (for [y (range y1 (inc y2) 1)] [x1 y]))
      (into #{} (for [x (range x1 (inc x2) 1)] [x y1])))
    (into #{} (for [x (range x1 (+ x2 0.4) 0.5)]
                [x (+ y1 (* (- x x1) slope-type))]))))

(defn new-way-points
  "way-points that a line crosses or nil for bad gradient or a point-line"
  [[[x1 y1] [x2 y2] :as [p1 p2 :as p]]]
  (if (= p1 p2)
    nil
    (let [dx (- x2 x1)
          dy (- y2 y1)]
      (cond
       (= 0 dx) (add-way-points p 0)
       (= 0 dy) (add-way-points p 0)
       (= dx dy) (add-way-points p 1)
       (= dx (- dy)) (add-way-points p -1)
       :else nil))))

;;;
;; Represent a move as a single point (tap) or double point (drag-line)
;;;
(defn is-dot?
  "if single dot return it, else nil"
  [dot]
  (when (vector? dot)
    (let [[x y] dot]
      (when (and (integer? x) (integer? y)) [x y]))))

(def dot-move? is-dot?)

(defn line-move?
  "If the move is a line return it, else nil"
  [line]
  (when (vector? line)
    (let [[p1 p2] line]
      (when (and  (vector? p1) (vector? p2))
        (let [[p1' p2'] (canonical-line line)]
          (when (not= p1' p2') [p1' p2']))))))

;;;
;; We'd like to be able to compose moves
;;;
(defn play-line-move
  "play a drag-line move"
  [game-state line]
  (when-let [[g wps] game-state]
    (let [new-wps (new-way-points line)]
      (when (not-any? new-wps wps)  ;ignore interecting lines
        (let  [pl (:player g)
               line-key (if (= :a pl) :a-lines :b-lines)
               updated-lines (conj (line-key g) line)]
          (prn (str "line-move " line))
          [(assoc g line-key updated-lines :player (if (= :a pl) :b :a)) (union wps new-wps)])))))

(defn play-dot-move
  "play a tap on dot"
  [[g wps :as game-state] dot]
  (let [a-crosses (:a-crosses g)
        b-crosses (:b-crosses g)]
    (when (and (not (a-crosses dot)) (not (b-crosses dot)) (not (wps dot)))
      (let [pl (:player g)
            [key crosses new-player] (if (= :a pl)
                            [:a-crosses a-crosses :b]
                            [:b-crosses b-crosses :a])
            g' (assoc g key (conj crosses dot) :player new-player)
            wps' (union wps #{dot})]
        (prn (str "dot-move" dot))
        [g' wps']))))

(defn play-move
  "play a move"
  [game-state move]
  (cond
    (line-move? move) (play-line-move game-state move)
    (dot-move? move) (play-dot-move game-state move)
    :else nil
    ))

(defn play-moves
  "play a sequence of moves"
  [game-state moves]
  (reduce #(play-move %1 %2) game-state moves)
  )
