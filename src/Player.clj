(ns Player
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as string])
  (:gen-class))

(def ^:const env :dev)
(def ^:const sha "00000000")

;; util
(defn prn-err
  [& args]
  (when (= env :dev)
    (binding [*out* *err*]
      (apply prn args))))

(defn print-table-err
  [& args]
  (when (= env :dev)
    (binding [*out* *err*]
      (apply pp/print-table args))))

(defn note-str
  [s]
  (if (= env :dev)
    s
    (subs sha 0 8)))

(defn elapsed-nanos
  [state]
  (- (System/nanoTime) (:start-nanos state)))

(defn elapsed-millis
  [state]
  (/ (- (System/nanoTime) (:start-nanos state))
     1000000.0))

(defn time-limit
  [tick]
  (if (= env :dev)
    1000
    (if (zero? tick) 990 45)))

;; math

(defn distance-sq
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (+ (* dx dx)
       (* dy dy))))

(defn distance
  [entity1 entity2]
  (Math/sqrt (distance-sq entity1 entity2)))

(defn cart->polar
  "Returns a map {:theta :r} for the {:x :y} coordinates of the given entity"
  [{:keys [x y]}]
  {:theta (Math/atan2 (double y) (double x))
   :r (Math/sqrt (+ (* x x) (* y y)))})

(defn polar->cart
  "returns a map {:x :y} for the given {:theta :r-squared} polar coordinates"
  [{:keys [theta r]}]
  {:x (* r (Math/cos theta))
   :y (* r (Math/sin theta))})

(comment
  (polar->cart (cart->polar {:x 123 :y 456})))

(defn overlaps?
  [entity1 entity2]
  (let [dist-sq (distance-sq entity1 entity2)
        radii (+ (:radius entity1) (:radius entity2))]
    (<= dist-sq (* radii radii))))

(defn mid-chord
  "Finds a point in the overlap between two entity circles
  (Specifically the midpoint of the common chord)"
  [{x1 :x y1 :y r1 :radius :as entity1}
   {x2 :x y2 :y r2 :radius :as entity2}]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        dsq (+ (* dx dx) (* dy dy))
        r1sq (* r1 r1)
        r2sq (* r2 r2)
        a*d (/ (+ r1sq (- r2sq) dsq)
               2)]
    {:x (double (+ x1 (* (/ a*d dsq) dx)))
     :y (double (+ y1 (* (/ a*d dsq) dy)))
     :a*d a*d}))

(defn intersections
  "returns the points where the two entities' bounding circles intersect, or
  nil if they do not intersect (may be non-overlapping or one entirely
  within the other) or if they are the same circle."
  [{x1 :x y1 :y r1 :radius :as entity1}
   {x2 :x y2 :y r2 :radius :as entity2}]
  (if (or (and (= x1 x2) (= y1 y2)) ;concentric circles
          (not (overlaps? entity1 entity2)))
    nil
    (let [dx (- x2 x1)
          dy (- y2 y1)
          dsq (+ (* dx dx) (* dy dy))
          r1sq (* r1 r1)
          {xmid :x ymid :y a*d :a*d} (mid-chord entity1 entity2)
          hsq (- r1sq (/ (* a*d a*d) dsq))
          h-div-d (* 0.99 (Math/sqrt (/ hsq dsq))) ;fudging this down to make sure we're inside the circles
          hx (* h-div-d dy)
          hy (* h-div-d dx)]
      [{:x (+ xmid hx)
        :y (- ymid hy)}
       {:x (- xmid hx)
        :y (+ ymid hy)}])))

(comment
  (nil? (intersections {:x 0 :y 0 :radius 123} {:x 0 :y 0 :radius 456}))
  (nil? (intersections {:x 0 :y 0 :radius 100} {:x 301 :y 0 :radius 200}))
  (= [{:x 100.0 :y 0.0} {:x 100.0 :y 0.0}] (intersections {:x 0 :y 0 :radius 100} {:x 300 :y 0 :radius 200}))
  (= [{:x 100.0 :y 0.0} {:x 0.0 :y 100.0}] (intersections {:x 0 :y 0 :radius 100} {:x 100 :y 100 :radius 100}))

  (let [c1 {:radius 800, :x 731, :y 1665}
        c2 {:radius 750, :x -166, :y 2539}]
    (= [{:x -54.883057710783646, :y 1800.0748252098704} {:x 575.5616265208137, :y 2447.11015902651}]
       (intersections c1 c2))))

(defn all-intersections
  [[entity1 & others]]
  (into (mapcat #(intersections entity1 %) others)
        (when (not-empty others) (all-intersections others))))

(defn average-point
  [points]
  (let [xs (reduce + (map :x points))
        ys (reduce + (map :y points))
        cnt (count points)]
    {:x (/ xs cnt)
     :y (/ ys cnt)}))

(defn inside?
  "Is the center of entity2 within entity1"
  [{:keys [radius] :as entity1}
   entity2]
  (< (distance-sq entity1 entity2)
     (* radius radius)))

(defn maxes-by
  [f coll]
  (when (not-empty coll)
    (let [groups (group-by f coll)
          max-k (reduce max (keys groups))]
      (get groups max-k))))

(defn mins-by
  [f coll]
  (let [groups (group-by f coll)
        min-k (reduce min (keys groups))]
    (get groups min-k)))

(comment
  (def coll [{:a 1 :b 1 :c 3}
             {:a 1 :b 2 :c 1}
             {:a 1 :b 2 :c 2}
             {:a 0 :b 3 :c 3}])

  (maxes-by :c (shuffle coll))
  (mins-by :c (shuffle coll)))

;; model

(def ^:const reaper-type 0)
(def ^:const destroyer-type 1)
(def ^:const doof-type 2)
(def ^:const tanker-type 3)
(def ^:const wreck-type 4)
(def ^:const tar-type 5)
(def ^:const oil-type 6)
(def ^:const reaper-mass 0.5)
(def ^:const reaper-radius 400)
(def ^:const reaper-friction 0.2)
(def ^:const destroyer-friction 0.3)
(def ^:const destroyer-mass 1.5)
(def ^:const doof-friction 0.25)
(def ^:const doof-mass 1.0)
(def ^:const epsilon 0.00001)
(def ^:const max-throttle 300)
(def ^:const field-radius 6000)
(def ^:const skill-range 2000)
(def ^:const skill-radius 1000)
(def ^:const max-tick 200) ; 200 time steps, 600 game turns in the UI because they count every user's ticks

(defn remaining-turns
  "remaining turns, including the one we are planning"
  [state]
  (- max-tick (:tick state)))

(defn nade-rage?
  [state]
  (>= (:my-rage state) 60))

(defn oil-rage?
  [state]
  (>= (:my-rage state) 30))

(defn mine?
  [entity]
  (zero? (:player entity)))

(defn enemy?
  [entity]
  (#{1 2} (:player entity)))

(defn reaper?
  [entity]
  (= reaper-type (:unit-type entity)))

(defn destroyer?
  [entity]
  (= destroyer-type (:unit-type entity)))

(defn doof?
  [entity]
  (= doof-type (:unit-type entity)))

(defn wreck?
  [entity]
  (= wreck-type (:unit-type entity)))

(defn tanker?
  [entity]
  (= tanker-type (:unit-type entity)))

(defn tar?
  [entity]
  (= tar-type (:unit-type entity)))

(defn oil?
  [entity]
  (= oil-type (:unit-type entity)))

(defn in-bounds?
  [{:keys [x y] :as entity}]
  (< (+ (* x x) (* y y))
     (* field-radius field-radius)))

(defn in-wreck?
  [entity state]
  (some #(inside? % entity) (:wrecks state)))

(defn in-oil?
  [entity state]
  (some #(inside? % entity) (:oil state)))

(defn highest-enemy-reaper
  [state]
  (let [reapers (->> (:units state)
                     (filter reaper?)
                     (group-by :player))]
    (if (> (:enemy-score-1 state) (:enemy-score-2 state))
      (first (get reapers 1))
      (first (get reapers 2)))))

(defn thrust
  "Update the vx and vy of the entity applying the given throttle action

  For a reaper of mass 0.5 and a throttle of 300, the change in
  velocity is 600 in the throttle direction"
  [{:keys [x y mass] :as entity}
   {px :x py :y throttle :throttle :as action}]
  (let [distance (distance entity action)
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
      (update :x #(Math/round ^Double %))
      (update :y #(Math/round ^Double %))
      (update :vx #(Math/round (* % (- 1.0 (:friction entity)))))
      (update :vy #(Math/round (* % (- 1.0 (:friction entity)))))))

(defn update-reaper
  [entity action]
  (-> entity
      (assoc :friction reaper-friction)
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

(defn vdot
  [a b]
  (reduce + (mapv * a b)))

(defn perpendicular
  [[x y]]
  [(- y) x])

(defn relative-velocities
  "Returns the pair [vparallel, vperpendicular] where
  vparallel is the speed the entities are moving apart along the line between them and
  vperpendicular is the speed they are moving apart perpendicularly
  Returns [NaN NaN] if they are already at the same position"
  [{x1 :x y1 :y vx1 :vx vy1 :vy :as entity1}
   {x2 :x y2 :y vx2 :vx vy2 :vy :as entity2}]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        d (Math/sqrt (+ (* dx dx) (* dy dy)))
        norm-from-1 [(/ dx d)
                     (/ dy d)]
        dv [(- vx2 vx1)
            (- vy2 vy1)]]
    [(vdot norm-from-1 dv)
     (vdot (perpendicular norm-from-1) dv)]))

(comment
  (relative-velocities {:x 0 :y 0 :vx 0 :vy 0} {:x 100 :y 0 :vx 0 :vy 0})
  (relative-velocities {:x 0 :y 0 :vx 1 :vy 0} {:x 100 :y 0 :vx -1 :vy 0})
  (relative-velocities {:x 0 :y 0 :vx 1 :vy 1} {:x 100 :y 0 :vx -1 :vy -1})
  (relative-velocities {:x 0 :y 0 :vx 0 :vy 1} {:x 100 :y 100 :vx -1 :vy 0})
  (relative-velocities {:x -70 :y -30 :vx 1 :vy 1 :radius 20} {:x 60 :y 30 :vx -1 :vy -1 :radius 20}))

(defn collision-time
  "Returns the time when entity1 and entity2 might collide.
  A bit optimistic about collisions, in that it treats the entity boundaries like squares instead of circles,
  so it could return a collision when the circles would have just barely missed.
  Returns 0 if they have already collided
  Returns nil if they will not collide in the future"
  [{r1 :radius :as entity1}
   {r2 :radius :as entity2}]
  (let [dsq (distance-sq entity1 entity2)
        sum-radii (+ r1 r2)
        rsq (* sum-radii sum-radii)]
    (if (< dsq rsq) ;already collided
      0
      (let [[vpar vperp] (relative-velocities entity1 entity2)
            d (Math/sqrt dsq)
            t (/ (- d sum-radii) ;time until vpar closes the gap
                 (- vpar))
            dperp (* t vperp)] ;how far they have moved apart in the perpendicular direction
        (if (or (not (pos? t)) ; collision is not in the future
                (> dperp sum-radii) (> (- dperp) sum-radii) ;have moved far enough perpendicularly to avoid
                (zero? vpar)) ; not moving closer
          nil
          t)))))

(comment
  (collision-time {:x 0 :y 0 :vx 0 :vy 0 :radius 20} {:x 100 :y 0 :vx 0 :vy 0 :radius 20})
  (collision-time {:x 0 :y 0 :vx 1 :vy 0 :radius 20} {:x 100 :y 0 :vx -1 :vy 0 :radius 20})
  (collision-time {:x 0 :y 0 :vx 1 :vy 1 :radius 20} {:x 100 :y 0 :vx -1 :vy -1 :radius 20})
  (collision-time {:x 0 :y 0 :vx 1 :vy 1 :radius 20} {:x 0 :y 100 :vx -1 :vy -1 :radius 20})
  (collision-time {:x 0 :y 0 :vx 1 :vy 1 :radius 20} {:x 0 :y 100 :vx 1 :vy -1 :radius 20})
  (collision-time {:x -70 :y -30 :vx 1 :vy 1 :radius 20} {:x 70 :y 30 :vx -1 :vy -1 :radius 20}) ;near miss at t=5
  (collision-time {:x -70 :y -30 :vx 1 :vy 1 :radius 20} {:x 60 :y 30 :vx -1 :vy -1 :radius 20}) ;hit when entity2 starts a little further left
  ;swap entities
  (collision-time {:x 70 :y 30 :vx -1 :vy -1 :radius 20} {:x -70 :y -30 :vx 1 :vy 1 :radius 20}) ;near miss at t=5
  (collision-time {:x 60 :y 30 :vx -1 :vy -1 :radius 20} {:x -70 :y -30 :vx 1 :vy 1 :radius 20}) ;hit when entity2 starts a little further left
)

(defn will-collide?
  [entity1 entity2 max-time]
  (let [t (collision-time entity1 entity2)]
    (and t (< t max-time))))

(comment
  (will-collide? {:x 60 :y 30 :vx -1 :vy -1 :radius 20} {:x -70 :y -30 :vx 1 :vy 1 :radius 20} 10.0) ;not enough time
  (will-collide? {:x 60 :y 30 :vx -1 :vy -1 :radius 20} {:x -70 :y -30 :vx 1 :vy 1 :radius 20} 40.0)) ;plenty of time

(defn tankers-to-be-wrecked
  [state]
  (let [destroyers (filter destroyer? (:units state))]
    (filter (fn [tanker]
              (some (fn [destroyer] (will-collide? tanker destroyer 1.0))
                    destroyers))
            (filter tanker? (:units state)))))

(comment
  (let [destroyers [{:x 586 :y 0 :radius reaper-radius :vx 354 :vy -362 :unit-type 1}
                    #_{:x -869 :y 868 :radius reaper-radius :vx 43 :vy 275 :unit-type 1}
                    #_{:x 1188 :y 1712 :radius reaper-radius :vx -145 :vy 270 :unit-type 1}]
        tankers [{:y 4320, :mass 3.0, :unit-type 3, :extra2 7, :extra 1, :unit-id 10, :radius 750, :vx 175, :vy -188, :player -1, :x -4013}
                 {:y 5705, :mass 3.0, :unit-type 3, :extra2 8, :extra 1, :unit-id 13, :radius 800, :vx 174, :vy -190, :player -1, :x -5230}]]
    (tankers-to-be-wrecked {:units (into destroyers tankers)}))

  (will-collide? {:x 586 :y 0 :radius reaper-radius :vx 354 :vy -362 :unit-type 1}
                 {:y 5705 :unit-type 3 :unit-id 13 :radius 800 :vx 174 :vy -190 :x -5230} 1.0)

  (collision-time {:x 586 :y 0 :radius reaper-radius :vx 354 :vy -362 :unit-type 1}
                  {:y 5705 :unit-type 3 :unit-id 13 :radius 800 :vx 174 :vy -190 :x -5230}))

;; bot commands

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
     :note (str "GOTO " (:unit-id target-entity))}))

(defn go-through
  [{:keys [x y vx vy mass] :as entity}
   target-entity]
  (let [{tx :x ty :y :as target} (move target-entity)
        to-target [(- tx x)
                   (- ty y)]
        dv [(- (to-target 0) vx)
            (- (to-target 1) vy)]]
    {:x (+ x (dv 0))
     :y (+ y (dv 1))
     :throttle max-throttle
     :note (str "GOTHRU " (:unit-id target-entity))}))

(defn go-near
  [self target & [buffer]]
  (assoc (go-to self
                (-> target
                    (update :x + (:radius self) (:radius target) (or buffer 0))))
         :note (str "GO-NEAR " (:unit-id target))))

(defn go-near-radial
  [self target & [buffer]]
  (let [^Integer buffer (or buffer 0)
        dist (+ (Math/abs buffer) (:radius self) (:radius target))
        sign (if (zero? buffer)
               1
               (/ buffer (Math/abs buffer)))
        point (-> target
                  (move)
                  (cart->polar)
                  (update :r + (* sign dist))
                  (polar->cart))]
    (assoc (go-to self point)
           :note (str "GO-NEAR-R " (:unit-id target)))))

(comment
  (go-near-radial {:radius 100 :x 100 :y 200 :vx 0 :vy 0 :mass 0.5} {:radius 200 :x 100 :y 0 :vx 500 :vy 0}))

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

(defn nearest-entity
  [self entities]
  (when (not-empty entities)
    (apply min-key
           #(distance-sq self %)
           entities)))

(defn turns-dist
  "A rough estimate of how many turns' travel a distance represents"
  ([self other]
   (if (inside? other self)
     0
     (turns-dist (distance self other))))
  ([dist]
   (cond
     (= 0     dist)      0
     (<= 0    dist 600)  1
     (<= 600  dist 1200) 2
     (<= 1200 dist 2400) 3
     (<= 2400 dist 3600) 4
     (<= 3600 dist 5400) 5
     (<= 5400 dist 9600) 7
     :else 9)))

(defn value-over-time
  "Average value per tick of the given wreck-or-state until it's exhausted or for `tick` ticks"
  [self wreck-or-overlap state ticks]
  (let [oil-time (:in-oil wreck-or-overlap) ; oil-time is in #{nil 1 2 3}
        travel-time (turns-dist self wreck-or-overlap)
        start-time (max (or oil-time 0) travel-time)
        time-values (take ticks (concat (repeat start-time 0)
                                        (:values-per-turn wreck-or-overlap)))]
    (if (not-empty time-values)
      {:avg-value (/ (reduce + time-values) (count time-values))
       :oil-time oil-time
       :travel-time travel-time
       :start-time start-time
       :time-values time-values}
      {:avg-value 0})))

(defn best-wreck
  "Evaluates wreck or overlap of wrecks value, based on how much we can expect
  to get from them and how long it will take to get there. Picks the best one
  of the given collection."
  [self wrecks-or-overlaps state]
  (let [time-horizon (min (remaining-turns state) 10)]
    (some->> wrecks-or-overlaps
             (not-empty)
             (map #(merge % (value-over-time self % state time-horizon)))
             (apply max-key :avg-value))))

(defn go-to-wreck
  [self state]
  (let [target (best-wreck self (into (:overlaps state) (:wrecks state)) state)]
    (prn-err "target" target)
    (cond
      (nil? target)         nil
      (inside? target self) (stop self)
      target                (go-to self target))))

(defn go-to-tanker
  [self state]
  (let [fattest-tanker (some->> (:tankers state)
                                (filter #(< (distance-sq % {:x 0 :y 0}) (* 4000 4000)))
                                (maxes-by :extra)
                                (not-empty)
                                (apply min-key #(distance-sq % self)))]
    (when fattest-tanker
      (go-near-radial self fattest-tanker -1))))

(defn reaper-action
  ;TODO the tanker collisions happen while units are moving, so it might be worthwhile to consider expected wrecks (given current tanker and destroyer velocities)
  [state]
  (prn-err "reaper-action start" (elapsed-millis state))
  (let [self (:reaper state)]
    (or
     (go-to-wreck self state)
     (go-to-tanker self state)
     (go-near self {:x 0 :y 0 :radius 0}))))

(defn destroyer-target?
  [entity]
  (and (not (mine? entity))
       (reaper? entity)))

(defn in-range?
  "in range of a skill attack"
  [self entity]
  (inside? (assoc self :radius skill-range)
           entity))

(defn protect-reaper
  "Returns an action to protect the reaper"
  [self state]
  (let [reaper (:reaper state)
        nade (assoc reaper :radius skill-radius)
        enemies (filter enemy? (:units state))]
    (if (and (nade-rage? state)
             (in-range? self reaper)
             (in-wreck? reaper state)
             (some #(inside? nade %) enemies))
      {:x (:x reaper)
       :y (:y reaper)
       :note "PROTEC"})))

(defn follow-reaper
  [self state]
  (go-near-radial self (:reaper state) (- (/ skill-range 2))))

(defn go-to-nearest-tanker
  [self state]
  (when (< (count (:wrecks state)) 3)
    (let [reaper (:reaper state)
          nearby-tankers (filter (fn [tanker]
                                   (and (in-bounds? tanker)
                                        (< (distance-sq reaper tanker)
                                           (* 3000 3000))))
                                 (:tankers state))
          nearest-tanker (when (not-empty nearby-tankers)
                           (apply min-key #(distance-sq reaper %) nearby-tankers))]
      (when nearest-tanker
        (go-to self nearest-tanker)))))

(defn destroyer-action
  [state]
  (prn-err "destroyer-action start" (elapsed-millis state))
  (let [destroyer (:destroyer state)]
    (or
     (protect-reaper destroyer state)
     (go-to-nearest-tanker destroyer state)
     (go-through destroyer (highest-enemy-reaper state)))))

(defn circle-doof
  [doof]
  (let [x (:x doof)
        y (:y doof)
        theta (Math/atan2 (double y) (double x))
        r 3000
        theta' (+ theta (/ Math/PI 8))
        x' (* r (Math/cos theta'))
        y' (* r (Math/sin theta'))]
    {:x x'
     :y y'
     :throttle max-throttle}))

(defn oilable?
  "predicate for an enemy reaper that is harvesting a wreck"
  [target self state]
  (and (not (mine? target))
       (reaper? target)
       (> (:my-rage state) 30)
       (in-range? self target)
       (not (inside? (assoc target :radius skill-radius) self))
       (in-wreck? target state)
       (not (in-oil? target state))))

(defn throw-oil
  "Try to throw oil at the given target, throwing short if we can still get them in the oil radius"
  [{:keys [x y] :as self}
   {tx :x ty :y :as target}]
  ;TODO make sure my reaper is not hit...
  (let [dx (- tx x)
        dy (- ty y)
        dist-sq (+ (* dx dx) (* dy dy))
        act-dist-sq (min (* skill-range skill-range)
                         dist-sq)
        scale (Math/sqrt (/ act-dist-sq dist-sq))]
    {:x (+ x (* scale dx))
     :y (+ y (* scale dy))
     :note (str "OIL " (:unit-id target))}))

(defn doof-action
  [state]
  (prn-err "doof-action start" (elapsed-millis state))
  (let [doof (:doof state)
        oil-target (highest-enemy-reaper state)
        oil-target' (move oil-target)]
    (cond
      (oilable? oil-target doof state)  (throw-oil doof oil-target)
      (oilable? oil-target' doof state) (throw-oil doof oil-target')
      :else                             (go-through doof oil-target))))

(comment
  (doof-action {:doof {:x 0 :y 3000}}))

(defn future-actions
  [state]
  [(future (let [action (reaper-action state)]
             (prn-err "end reaper" (elapsed-millis state))
             action))
   (future (let [action (destroyer-action state)]
             (prn-err "end destroyer" (elapsed-millis state))
             action))
   (future (let [action (doof-action state)]
             (prn-err "end doof" (elapsed-millis state))
             action))])

(defn actions
  [state]
  (let [reaper (reaper-action state)
        _ (prn-err "end reaper" (elapsed-millis state))
        destroyer (destroyer-action state)
        _ (prn-err "end destroyer" (elapsed-millis state))
        doof (doof-action state)]
    (prn-err "end doof" (elapsed-millis state))
    [reaper
     destroyer
     doof]))

(defn action-str
  [{:keys [x y throttle note timeout] :as action}]
  (cond
    (and x y throttle) (format "%d %d %d %s" (int x) (int y) (int throttle) (note-str note))
    (and x y) (format "SKILL %d %d %s" (int x) (int y) (note-str note))
    timeout (str "WAIT TIMEOUT " timeout)
    (nil? action) "WAIT null"
    :else "WAIT fallthrough"))

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

(defn find-common-point
  "Takes a collection of overlapping wrecks and finds a point that is within all of them.
  returns a map like
  {:wrecks #{the wrecks}
   :x double
   :y double}
  Returns nil if there didn't turn out to be any common point."
  [wrecks]
  (let [common (cond
                 (< (count wrecks) 2) (throw (Exception. (str "not enough wrecks: " (count wrecks))))
                 (= 2 (count wrecks)) (apply mid-chord wrecks) ;TODO find out if this is meaningless when one circle is inside the other
                 :else (some->> wrecks
                                (vec)
                                (all-intersections)
                                (filter (fn [point] (every? #(inside? % point) wrecks)))
                                (not-empty)
                                (average-point)))]
    (when (some? common)
      {:wrecks wrecks
       :unit-id (str "CF" (string/join "," (map :unit-id wrecks)))
       :x (:x common)
       :y (:y common)
       :radius 100})))

(comment
  (def wrecks [{:radius 850, :x 783, :y 1908}
               {:radius 800, :x 731, :y 1665}
               {:radius 750, :x -166, :y 2539}])

  (overlaps? (wrecks 0) (wrecks 1))
  (overlaps? (wrecks 1) (wrecks 2))
  (overlaps? (wrecks 2) (wrecks 0))

  (every? #(inside? (wrecks 0) %) (intersections (wrecks 0) (wrecks 1)))
  (every? #(inside? (wrecks 1) %) (intersections (wrecks 0) (wrecks 1)))
  (every? #(inside? (wrecks 1) %) (intersections (wrecks 1) (wrecks 2)))
  (every? #(inside? (wrecks 2) %) (intersections (wrecks 1) (wrecks 2)))

  (def pts (all-intersections wrecks))
  (filter #(inside? (wrecks 0) %) pts)
  (filter #(inside? (wrecks 1) %) pts)
  (filter #(inside? (wrecks 2) %) pts)

  (->> wrecks
       (vec)
       (all-intersections)
       (filter (fn [point] (every? #(inside? % point) wrecks)))))

(defn evaluate-overlap-group
  "Takes a map from `find-common-point` and adds a :value key with a numeric
  score for that group"
  [group]
  (let [values (map :extra (:wrecks group))
        max-value (apply max values)
        values-per-turn (for [turn (range max-value)]
                          (count (filter #(> % turn) values)))]
    (assoc group
           :value (reduce + values)
           :values-per-turn values-per-turn
           :first-turn-value (first values-per-turn))))

(defn evaluate-wreck
  [wreck]
  (assoc wreck
         :value (:extra wreck)
         :values-per-turn (repeat (:extra wreck) 1)
         :first-turn-value 1))

(defn find-overlaps
  [wrecks]
  (for [wreck wrecks]
    (assoc wreck :overlaps (set (map :unit-id (filter #(and (overlaps? wreck %) (not= wreck %)) wrecks))))))

(defn bron-kerbosch
  "Returns all of the maximal cliques in the graph formed by the vertices in
  set p, given a neighbors-fn that takes two vertices and returns true if they are neighbors"
  ([neighbors-fn p]
   (bron-kerbosch #{} (set p) #{} neighbors-fn))
  ([r p x neighbors-fn]
   (if (and (empty? p) (empty? x))
     [r]
     (loop [p p
            x x
            result []]
       (if (empty? p)
         result
         (let [v (first p)
               new-cliques (bron-kerbosch (conj r v)
                                          (set/select #(neighbors-fn v %) p)
                                          (set/select #(neighbors-fn v %) x)
                                          neighbors-fn)]
           (recur (disj p v)
                  (conj x v)
                  (into result new-cliques))))))))

(comment
  (let [wikipedia-nodes [1 2 3 4 5 6]
        wikipedia-neigh {1 #{2 5}
                         2 #{1 3 5}
                         3 #{2 4}
                         4 #{3 5 6}
                         5 #{1 2 4}
                         6 #{4}}]
    (bron-kerbosch #((wikipedia-neigh %1) %2) wikipedia-nodes)))

(defn find-overlap-groups
  [wrecks]
  (some->> wrecks
           (remove #(empty? (:overlaps %)))
           (not-empty)
           (bron-kerbosch (fn [w1 w2] (contains? (:overlaps w1) (:unit-id w2))))
           (mapv find-common-point)
           (remove nil?)
           (mapv evaluate-overlap-group)))

(defn apply-oil
  [entity oil]
  (let [in-oils (filter #(inside? % entity) oil)
        turns (when (not-empty in-oils)
                (apply max (map :extra in-oils)))]
    (if (some? turns)
      (assoc entity :in-oil turns)
      entity)))

(defn augment-state
  "takes the state that we read in, and adds a bunch of other stuff"
  [state]
  (let [units (:units state)
        oil (filter oil? units)
        wrecks (find-overlaps (map #(apply-oil % oil) (map evaluate-wreck (filter wreck? units))))
        new-wrecks (tankers-to-be-wrecked state)
        expected-wrecks (into wrecks new-wrecks)]
    (prn-err "NEW WRECKS" new-wrecks)
    (assoc state
           :wrecks expected-wrecks
           :overlaps (map #(apply-oil % oil) (find-overlap-groups expected-wrecks))
           :tankers (filter tanker? units)
           :oil oil
           :tar (filter tar? units)
           :reaper (first (filter #(and (reaper? %) (mine? %)) units))
           :destroyer (first (filter #(and (destroyer? %) (mine? %)) units))
           :doof (first (filter #(and (doof? %) (mine? %)) units)))))

(defn read-state
  []
  (let [score (read) ;read one field before recording the start time
        start-nanos (System/nanoTime)
        _ (prn-err "start-nanos" start-nanos)
        base (hash-map :my-score score
                       :enemy-score-1 (read)
                       :enemy-score-2 (read)
                       :my-rage (read)
                       :enemy-rage-1 (read)
                       :enemy-rage-2 (read)
                       :unit-count (read)
                       :start-nanos start-nanos)
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
    (assoc base :units units)))

(defn -main [& args]
  (System/gc)
  (loop [tick 0]
    (let [state (assoc (read-state) :tick tick)
          _ (prn-err "after read-state" (elapsed-millis state))
          state' (augment-state state)
          max-millis (time-limit tick)]
      (prn-err "after augment" (elapsed-millis state'))

      (doseq [action (actions state')
              :let [t (- max-millis (elapsed-millis state'))]]
        (-> action
            (action-str)
            (println)))
      (flush)
      (prn-err "end" (elapsed-millis state')))
    (recur (inc tick))))
