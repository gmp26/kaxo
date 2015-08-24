(ns ^:figwheel-always kaxo.mex)


;;
;; Note that for large shapes it may be necessary to include way-points in the
;; canonical representation. We ignore that complication for now.
;;
(defn shift
  "translate a region so its top-left is at [0 0]"
  [region]
  (let [dims (dimensions region)]
    (map (fn [[x y]] (- x (:l dims)) (- y (:t dims))) region)
    )
  )

#_(defn shape-recogniser
  "find a canonical representation of the shape of a region"
  [region]
  (let [dims])
  )
