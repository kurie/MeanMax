(ns Player
  (:require [clojure.pprint :as pp])
  (:gen-class))

;; math

(defn distance-sq
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (+ (* dx dx)
       (* dy dy))))

(defn inside?
  [{:keys [radius] :as entity1}
   entity2]
  (< (distance-sq entity1 entity2)
     (* radius radius)))

;; model

(def ^:const reaper-type 0)
(def ^:const wreck-type 4)
(def ^:const reaper-radius 400)
(def ^:const reaper-friction 0.2)
(def ^:const epsilon 0.00001)
(def ^:const max-throttle 300)

(defn thrust
  "Update the vx and vy of the entity applying the given throttle action

  For a reaper of mass 0.5 and a throttle of 300, the change in
  velocity is 600 in the throttle direction"
  [{:keys [x y mass] :as entity}
   {px :x py :y throttle :throttle :as action}]
  (let [distance (Math/sqrt (distance-sq entity action))
        coef (/ throttle mass distance)]
    (if (<= distance epsilon)
      entity
      (-> entity
          (update :vx + (* coef (- px x)))
          (update :vy + (* coef (- py y)))))))

(comment
  (let [throttle 300
        mass 0.5
        px 10
        x 0
        distance (- px x)
        coef (/ throttle mass distance)]
    (* coef (- px x)))

  (let [before {:x 0 :y 0 :mass 0.5 :vx 0 :vy 0}
        after (thrust before {:x 10 :y 0 :throttle 300})
        vx' (:vx after)
        vy' (:vy after)]
    (Math/sqrt (+ (* vx' vx') (* vy' vy')))))

(defn move
  [entity]
  (-> entity
      (update :x + (:vx entity))
      (update :y + (:vy entity))))

(defn adjust
  [entity]
  (-> entity
      (update :x #(Math/round %))
      (update :y #(Math/round %))
      (update :vx #(Math/round (* % (- 1.0 (:friction entity)))))
      (update :vy #(Math/round (* % (- 1.0 (:friction entity)))))))

(defn update-reaper
  [entity action]
  (-> entity
      (assoc :friction reaper-friction) ;TODO there will be special friction conditions later
      (thrust action)
      (move)
      (adjust)))

(comment
  (-> {:x 3160 :y 1180 :vx -117 :vy -323 :mass 0.5 :friction reaper-friction}
      (thrust {:x 2206 :y -2033 :throttle 300})
      (move)
      (adjust))

  (update-reaper {:x 3160 :y 1180 :vx -117 :vy -323 :mass 0.5}
                 {:x 2206 :y -2033 :throttle 300}))

(defn prn-err
  [& args]
  (binding [*out* *err*]
    (apply prn args)))

(defn pp-err
  [object]
  (pp/pprint object *err*))

(defn print-table-err
  [& args]
  (binding [*out* *err*]
    (apply pp/print-table args)))

(defn go-to
  [{:keys [x y vx vy mass] :as entity}
   {tx :x ty :y :as target-entity}]
  (let [head-x (- tx vx)
        head-y (- ty vy)
        dvx (- head-x x)
        dvy (- head-y y)
        speed (Math/sqrt (+ (* dvx dvx) (* dvy dvy)))
        want-throttle (* speed mass)
        throttle (min max-throttle want-throttle)]
    {:x head-x
     :y head-y
     :throttle throttle
     :note (:unit-id target-entity)}))

(defn stop
  "Try to kill velocity"
  [{:keys [x y vx vy mass] :as entity}]
  (let [speed (Math/sqrt (+ (* vx vx) (* vy vy)))]
    {:x (- x vx)
     :y (- y vy)
     :throttle (min max-throttle (* speed mass))}))

(comment
  (let [entity {:x 0 :y 0 :vx 500 :vy 500 :mass 0.5}]
    (thrust entity (stop entity)))

  (let [entity {:x 1000 :y 1000 :vx 200 :vy 300 :mass 0.5}]
    ;(go-to entity {:x 1010 :y 1010})
    ;(thrust entity {:x 10, :y 10, :throttle 7.0710678118654755, :note nil})
    ;(thrust entity (go-to entity {:x 1010 :y 1010}))
    (update-reaper entity (go-to entity {:x 1010 :y 1010}))))

(defn reaper-action
  [state]
  (when-let [nearest-wreck (when (not-empty (:wrecks state))
                             (apply min-key #(distance-sq (:reaper state) %) (:wrecks state)))]
    (if (inside? nearest-wreck (:reaper state))
      (stop (:reaper state))
      (go-to (:reaper state) nearest-wreck))))

(defn actions
  [state]
  [(reaper-action state)
   nil
   nil])

(defn action-str
  [{:keys [x y throttle note] :as action}]
  (if action
    (format "%d %d %d %s" (int x) (int y) (int throttle) note)
    "WAIT"))

(def unit-keys
  [:unit-id
   :unit-type
   :player
   :mass
   :radius
   :x
   :y
   :vx
   :vy
   :extra
   :extra2])

(defn read-state
  []
  (let [base (hash-map :my-score (read)
                       :enemy-score-1 (read)
                       :enemy-score-2 (read)
                       :my-rage (read)
                       :enemy-rage-1 (read)
                       :enemy-rage-2 (read)
                       :unit-count (read))
        units (doall (for [i (range (:unit-count base))]
                       (hash-map :unit-id (read)
                                 :unit-type (read)
                                 :player (read)
                                 :mass (read)
                                 :radius (read)
                                 :x (read)
                                 :y (read)
                                 :vx (read)
                                 :vy (read)
                                 :extra (read)
                                 :extra2 (read))))]
    (assoc base
           :units units
           :wrecks (filter #(= wreck-type (:unit-type %)) units)
           :reaper (first (filter #(and (= reaper-type (:unit-type %))
                                        (zero? (:player %))) units)))))

(defn -main [& args]
  (while true
    (let [state (read-state)]

      (pp-err (select-keys state [:my-score :enemy-score-1 :enemy-score-2 :my-rage :enemy-rage-1 :enemy-rage-2 :unit-count]))
      (print-table-err unit-keys (:units state))

      ; Write action to stdout
      (run! println (map action-str (actions state))))))
