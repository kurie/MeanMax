(ns Player
  (:require [clojure.pprint :as pp]
            [clojure.set :as set])
  (:gen-class))

;; math

(defn distance-sq
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (+ (* dx dx)
       (* dy dy))))

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
    {:x (+ x1 (* (/ a*d dsq) dx))
     :y (+ y1 (* (/ a*d dsq) dy))
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
          h-div-d (Math/sqrt (/ hsq dsq))]
      [{:x (+ xmid (* h-div-d dy))
        :y (- ymid (* h-div-d dx))}
       {:x (- xmid (* h-div-d dy))
        :y (+ ymid (* h-div-d dx))}])))

(comment
  (nil? (intersections {:x 0 :y 0 :radius 123} {:x 0 :y 0 :radius 456}))
  (nil? (intersections {:x 0 :y 0 :radius 100} {:x 301 :y 0 :radius 200}))
  (= [{:x 100.0 :y 0.0} {:x 100.0 :y 0.0}] (intersections {:x 0 :y 0 :radius 100} {:x 300 :y 0 :radius 200}))
  (= [{:x 100.0 :y 0.0} {:x 0.0 :y 100.0}] (intersections {:x 0 :y 0 :radius 100} {:x 100 :y 100 :radius 100})))

(defn inside?
  "Is the center of entity2 within entity1"
  [{:keys [radius] :as entity1}
   entity2]
  (< (distance-sq entity1 entity2)
     (* radius radius)))

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
  (let [buffer (or buffer 0)
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

(defn reaper-action
  [state]
  ;TODO find more valuable wreck overlaps
  (let [self (:reaper state)
        nearest-clean-wreck (nearest-entity self
                                            (filter #(not (in-oil? % state)) (:wrecks state)))
        nearest-wreck (nearest-entity self (:wrecks state))
        nearest-dest-tanker (some->> (:tankers state)
                                     (not-empty)
                                     (apply min-key #(distance-sq % (:destroyer state))))]
    (cond
      (and nearest-clean-wreck (inside? nearest-clean-wreck self)) (stop self)
      nearest-clean-wreck                                          (go-to self nearest-clean-wreck)
      nearest-wreck                                                (go-to self nearest-wreck)
      :else                                                        (go-near-radial self nearest-dest-tanker (/ skill-range 2)))))

(defn destroyer-target?
  [entity]
  (and (not (mine? entity))
       (reaper? entity)))

(defn in-range?
  "in range of a skill attack"
  [self entity]
  (inside? (assoc self :radius skill-range)
           entity))

(defn protect-reaper?
  "Should we protect the reaper by throwing a grenade at its center?"
  [self state]
  (let [nade (assoc (:reaper state) :radius skill-radius)
        enemies (filter enemy? (:units state))]
    (and (in-wreck? (:reaper state) state)
         (some #(inside? nade %) enemies))))

(defn protect-reaper
  "Returns an action to protect the reaper"
  [self state]
  (let [reaper (:reaper state)]
    {:x (:x reaper)
     :y (:y reaper)
     :note "PROTEC"}))

(defn follow-reaper
  [self state]
  (go-near-radial self (:reaper state) (- (/ skill-range 2))))

(defn destroyer-action
  [state]
  (let [destroyer (:destroyer state)
        nearest-tanker (some->> (:tankers state)
                                (filter in-bounds?)
                                (filter #(< (Math/sqrt (distance-sq % destroyer))
                                            (+ (:radius destroyer) (:radius %) 300)))
                                (not-empty)
                                (apply min-key #(distance-sq (:reaper state) %)))]
    (cond
      (and (nade-rage? state)
           (in-range? destroyer (:reaper state))
           (protect-reaper? destroyer state))
      (protect-reaper destroyer state)

      nearest-tanker
      (go-to destroyer nearest-tanker)

      :else
      (follow-reaper destroyer state))))

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

(defn highest-enemy-reaper
  [state]
  (let [reapers (->> (:units state)
                     (filter reaper?)
                     (group-by :player))]
    (if (> (:enemy-score-1 state) (:enemy-score-2 state))
      (first (get reapers 1))
      (first (get reapers 2)))))

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
    (prn-err "throw-oil scale" scale)
    {:x (+ x (* scale dx))
     :y (+ y (* scale dy))
     :note (str "OIL " (:unit-id target))}))

(defn doof-action
  [state]
  (let [doof (:doof state)
        oil-target (highest-enemy-reaper state)
        oil-target' (move oil-target)]
    (cond
      (oilable? oil-target doof state)  (throw-oil doof oil-target)
      (oilable? oil-target' doof state) (throw-oil doof oil-target')
      :else                             (go-through doof oil-target))))

(comment
  (doof-action {:doof {:x 0 :y 3000}}))

(defn actions
  [state]
  [(future (reaper-action state))
   (future (destroyer-action state))
   (future (doof-action state))])

(defn action-str
  [{:keys [x y throttle note timeout] :as action}]
  (cond
    (and x y throttle) (format "%d %d %d %s" (int x) (int y) (int throttle) note)
    (and x y) (format "SKILL %d %d %s" (int x) (int y) note)
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

(defn find-overlaps
  [wrecks]
  (for [wreck wrecks]
    (assoc wreck :overlaps (set (filter #(and (overlaps? wreck %) (not= wreck %)) wrecks)))))

(defn bron-kerbosch
  "Returns all of the maximal cliques in the graph formed by the vertices in
  set p, given a neighbors-fn that takes a vertex and returns a set of its
  neighbors"
  ([neighbors-fn p] (bron-kerbosch #{} (set p) #{} neighbors-fn))
  ([r p x neighbors-fn]
   (if (and (empty? p) (empty? x))
     [r]
     (loop [p p
            x x
            result []]
       (let [v (first p)
             neighbors (neighbors-fn v)
             new-cliques (bron-kerbosch (conj r v)
                                        (set/intersection p neighbors)
                                        (set/intersection x neighbors)
                                        neighbors-fn)]
         (if (empty? p)
           result
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
    (bron-kerbosch wikipedia-neigh wikipedia-nodes)))

(defn find-overlap-groups
  [wrecks]
  (->> wrecks
       (remove #(empty? (:overlaps %)))
       (bron-kerbosch :overlaps)))

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
                                 :extra2 (read))))
        wrecks (find-overlaps (filter wreck? units))]
    (assoc base
           :units units
           :wrecks wrecks
           :overlaps (find-overlap-groups wrecks)
           :tankers (filter tanker? units)
           :oil (filter oil? units)
           :tar (filter tar? units)
           :reaper (first (filter #(and (reaper? %) (mine? %)) units))
           :destroyer (first (filter #(and (destroyer? %) (mine? %)) units))
           :doof (first (filter #(and (doof? %) (mine? %)) units)))))

(defn -main [& args]
  (while true
    (let [state (read-state)]

      ;(pp-err (select-keys state [:my-score :enemy-score-1 :enemy-score-2 :my-rage :enemy-rage-1 :enemy-rage-2 :unit-count]))
      ;(print-table-err unit-keys (:units state))
      (pp-err (:overlaps state))

      ; Write action to stdout
      (doseq [action (actions state)]
        (-> action
            (deref 45 {:timeout 45})
            (action-str)
            (println))))))
