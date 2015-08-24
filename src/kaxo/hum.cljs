(ns kaxo.hum
  (:require [kaxo.core :as k]
            [clojure.string :as str]))

;;
;; Look at momemnts and hu moments of a region in order to find some useful invariants
;; for shape recognition. See https://en.wikipedia.org/wiki/Image_moment
;;

(defn bounding-box
  "the bounding-box of a region"
  [region]
  (let [xs (map first region)
        ys (map second region)]
    {:top (apply min ys)
     :left (apply min xs)
     :bottom (apply max ys)
     :right (apply max xs)
    })
  )

(defn shift-to
  "region offsets from origin"
  [region [ox oy]]
  (map (fn [[x y]] [(- x ox) (- y oy)]) region)
  )

(defn clipped
  "a clipped region"
  [region]
  (let [bb (bounding-box region)
        {:keys [top left bottom right]} bb
        origin [left top]
        ;dims (map inc [(- right left) (- bottom top)])
        ]
    {:origin origin :clip (set (shift-to region origin)) })
  )

(defn dot-moment
  "raw moment of a single dot"
  [p q [x y]]
  (* (Math.pow x p) (Math.pow y q))
  )

(defn raw-moment
  "find m_p_q of a region"
  [p q region]
  (reduce + (map #(dot-moment p q %) region)))

(def area #(raw-moment 0 0 %))

(defn centroid
  "find the centroid of a region"
  [region]
  (let [m #(raw-moment %1 %2 region)
        m00 (m 0 0)]
    [(/ (m 1 0) m00) (/ (m 0 1) m00)]))

(defn sq [n] (* n n))

(defn central-moment
  "find mu_p_q of a region - the central moments"
  [p q region]
  (let [m #(raw-moment %1 %2 region)
        m00 (m 0 0)
        x-bar (/ (m 1 0) m00)
        y-bar (/ (m 0 1) m00)]
    ({ [0 0] m00
       [0 1] 0
       [1 0] 0
       [1 1] (- (m 1 1) (* x-bar (m 0 1)))
       [2 0] (- (m 2 0) (* x-bar (m 1 0)))
       [0 2] (- (m 0 2) (* y-bar (m 0 1)))
       [2 1] (+ (m 2 1) (* -2 x-bar (m 1 1)) (* -1 y-bar (m 2 0)) (* 2 (sq x-bar) (m 0 1)))
       [1 2] (+ (m 1 2) (* -2 y-bar (m 1 1)) (* -1 x-bar (m 0 2)) (* 2 (sq y-bar) (m 1 0)))
       [3 0] (+ (m 3 0) (* -3 x-bar (m 2 0)) (* 2 (sq x-bar) (m 1 0)))
       [0 3] (+ (m 0 3) (* -3 y-bar (m 0 2)) (* 2 (sq y-bar) (m 0 1)))} [p q])))

;;;
;; comment out the second version if you need scale-invariant hu moments
;;;

(def eta-scale central-moment)

#_(defn eta-scale
  "scaled central moment"
  [p q region]
  (let [mu #(central-moment %1 %2 region)]
    (/ (mu p q) (Math.pow (mu 0 0) (inc (/ (+ p q) 2))))))

(defn hu-moment
  "find hu_n of a region - the hu moments"
  [n region]
  (let [eta #(eta-scale %1 %2 region)]
    (nth [(+ (eta 2 0) (eta 0 2))
          (+ (sq (- (eta 2 0) (eta 0 2))) (* 4 (sq (eta 1 1))))
          (+ (sq (- (eta 3 0) (* 3 (eta 1 2)))) (sq (- (* 3 (eta 2 1)) (eta 0 3))))
          ;; (+ (sq (+  (eta 3 0) (eta 1 2))) (sq (+ (eta 2 1) (eta 0 3))))
          ]
       (- n 1))))

(defn hu-moments
  "return a hash using hu-moments of a region"
  [region]
  (str/join " " (for [n (range 1 3)] (.toPrecision (hu-moment n region) 4)))
  )
