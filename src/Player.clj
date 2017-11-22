(ns Player
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as string])
  (:gen-class))

(def ^:const env :dev)

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
  (when (= env :dev) s))

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
      (update :x #(Math/round ^Double %))
      (update :y #(Math/round ^Double %))
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
     (turns-dist (distance-sq self other))))
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

(defn best-wreck
  "Evaluates wreck or overlap of wrecks value, based on how much we can expect
  to get from them and how long it will take to get there. Picks the best one
  of the given collection."
  [self wrecks-or-overlaps]
  ; TODO check clean vs oily here, since the oil may be gone by the time we can get there
  (when (not-empty wrecks-or-overlaps)
    (apply max-key #(- (:value %) (turns-dist self %)) wrecks-or-overlaps)))

(defn go-to-wreck
  [self state]
  (let [wrecks (:wrecks state)
        clean-wrecks (filter #(not (in-oil? % state)) wrecks) ;TODO preprocess in-oil for wrecks and overlaps (and assoc them on, so I can look at how long they'll last in best-wreck)
        overlaps (:overlaps state)
        clean-overlaps (filter #(not (in-oil? % state)) overlaps)
        target (or (best-wreck self (into clean-overlaps clean-wrecks))
                   (best-wreck self (into overlaps wrecks)))]
    (cond
      (nil? target)         nil
      (inside? target self) (stop self) ;TODO check order of game loop. Might not be worthwhile to stop if I'll finish up the wreck this tick, and can pick something better to do instead.
      target                (go-to self target))))

(defn go-to-tanker
  [self state]
  (let [nearest-dest-tanker (some->> (:tankers state)
                                     (filter #(in-bounds? %))
                                     (not-empty)
                                     (apply min-key #(distance-sq % (:destroyer state))))] ;TODO ideally within the 4000 unit circle, maybe do a check
    (when nearest-dest-tanker
      (go-near-radial self nearest-dest-tanker (/ skill-range 2)))))

(defn reaper-action
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
  (prn-err "destroyer-action start" (elapsed-millis state))
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

(defn actions
  [state]
  [(future (reaper-action state))
   (future (destroyer-action state))
   (future (doof-action state))])

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
  (->> (:wrecks group)
       (map :extra)
       (reduce +)
       (assoc group :value))) ;TODO also assoc initial value-per-turn (number of wrecks), or value-per-turn sequence assuming someone is in the overlap consuming from all wrecks

(defn evaluate-wreck
  [wreck]
  (assoc wreck :value (:extra wreck)))

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

(defn augment-state
  "takes the state that we read in, and adds a bunch of other stuff"
  [state]
  (let [units (:units state)
        wrecks (find-overlaps (map evaluate-wreck (filter wreck? units)))]
    (assoc state
           :wrecks wrecks
           :overlaps (find-overlap-groups wrecks)
           :tankers (filter tanker? units)
           :oil (filter oil? units)
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
    (let [state (read-state)
          _ (prn-err "after read-state" (elapsed-millis state))
          state' (augment-state state)
          max-millis (time-limit tick)]
      (prn-err "after augment" (elapsed-millis state'))

      (doseq [action (actions state')
              :let [t (- max-millis (elapsed-millis state'))]]
        (-> action
            (deref t {:timeout max-millis})
            (action-str)
            (println))))
    (recur (inc tick))))
