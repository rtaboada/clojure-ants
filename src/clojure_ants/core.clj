(ns clojure-ants.core
  (:gen-class)
  (:import (java.awt.image BufferedImage)
           (javax.swing.JPanel)
           (javax.swing JFrame)))

(def dim 80)

(def nants-sqrt 7)

(def food-places 35)

(def food-range 100)

(def pher-scale 20.0)

(def food-scale 30.0)

(def evap-rate 0.99)

(def animation-sleep-ms 100)
(def ant-sleep-ms 40)
(def evap-sleep-ms 1000)

(def running true)

(defstruct cell :food :pher)

(def world
  (apply vector
         (map (fn [_]
                (apply vector (map (fn [_] (ref (struct cell 0 0)))
                                   (range dim))))
              (range dim))))


(defn place [[x y]]
  (-> world (nth x) (nth y)))

(defstruct ant :dir)

(defn create-ant
  "create an ant at the location, returning an ant agent on the location"
  [loc dir]
  (sync nil
        (let [p (place loc)
              a (struct ant dir)]
          (alter p assoc :ant a)
          (agent loc))))

(def home-off (/ dim 4))
(def home-range (range home-off (+ nants-sqrt home-off)))

(defn setup
  "places initial food and ants, returns seq of ant agents"
  []
  (sync nil
        (dotimes [_ food-places]
          (let [p (place [(rand-int dim) (rand-int dim)])]
                     (alter p assoc :food (rand-int food-range))))
        (doall
          (for [x home-range y home-range]
            (do
              (alter (place [x y])
                     assoc :home true)
              (create-ant [x y] (rand-int 8)))))))

(defn bound
  "returns n wrapeed into range 0-b"
  [b n]
  (let [n (rem n b)]
    (if (neg? n)
      (+ n b)
      n)))

(defn wrand
  "given a vector of slice sizes, returns the index of a slice given a random
  spin of a roulette wheel with compartments proportional to slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(def dir-delta {0 [0 -1]
                1 [1 -1]
                2 [1 0]
                3 [1 1]
                4 [0 1]
                5 [-1 1]
                6 [-1 0]
                7 [-1 -1]})

(defn delta-loc
  "returns the location one step in the given dir. Note the world is a torus"
  [[x y] dir]
  (let [[dx dy] (dir-delta (bound 8 dir))]
    [(bound dim (+ x dx)) (bound dim (+ y dy))]))

(defn turn
  "turns the ant at the location by the given amount"
  [loc amt]
  (dosync
    (let [p (place loc)
          ant (:ant @p)]
      (alter p assoc :ant (assoc ant :dir (bound 8 (+ (:dir ant) amt))))))
  loc)

(defn move
  "moves the ant in the direction it is heading. Must be called in a
  transaction that has verified the way is clear"
  [loc]
  (let [oldp (place loc)
        ant (:ant @oldp)
        newloc (delta-loc loc (:dir ant))
        p (place newloc)]
    ; move the ant
    (alter p assoc :ant ant)
    (alter oldp dissoc :ant)
    ;leave pheromone trail
    (when-not (:home @oldp)
      (alter oldp assoc :pher (inc (:pher @oldp))))
    newloc))

(defn take-food
  "Takes one food from current location. Must be called in a
  transaction that has verified ther is food available"
  [loc]
  (let [p (place loc)
        ant (:ant @p)]
    (alter p assoc
           :food (dec (:food @p))
           :ant (assoc ant :food true))
    loc))

(defn drop-food
  "Drops food at current location. Must be called in a transaction
  that has verified the ant has food"
  [loc]
  (let [p (place loc)
        ant (:ant @p)]
    (alter p assoc
           :food (inc (:food @p))
           :ant (dissoc ant :food))
    loc))

(defn rank-by
  "returns a map of xs to their 1-based rank when sorted by keyfn"
  [keyfn xs]
  (let [sorted (sort-by (comp float keyfn) xs)]
    (reduce (fn [ret i] (assoc ret (nth sorted i) (inc i)))
            {} (range (count sorted)))))

(defn behave
  "the main function for the ant agent"
  [loc]
  (let [p (place loc)
        ant (:ant @p)
        ahead (place (delta-loc loc (:dir ant)))
        ahead-left (place (delta-loc loc (dec (:dir ant))))
        ahead-right (place (delta-loc loc (inc (:dir ant))))
        places [ahead ahead-left ahead-right]]
    (dosync
      (when running
        (send-off *agent* #'behave))
      (Thread/sleep ant-sleep-ms)
      (if (:food ant)
        ;going home
        (cond
          (:home @p)
          (-> loc drop-food (turn 4))
          (and (:home @ahead) (not (:ant @ahead)))
          (move loc)
          :else
          (let [ranks (merge-with +
                                  (rank-by (comp #(if (:home %) 1 0) deref) places)
                                  (rank-by (comp :pher deref) places))]
            (([move #(turn % -1) #(turn % 1)]
               (wrand [(if (:ant @ahead) 0 (ranks ahead))
                       (ranks ahead-left) (ranks ahead-right)]))
              loc)))
        ;foraging
        (cond
          (and (pos? (:food @p)) (not (:home @p)))
          (-> loc take-food (turn 4))
          (and (pos? (:food @ahead)) (not (:home @ahead)) (not (:ant @ahead)))
          (move loc)
          :else
          (let [ranks (merge-with +
                                  (rank-by (comp :food deref) places)
                                  (rank-by (comp :pher deref) places))]
            (([move #(turn % -1) #(turn % 1)]
               (wrand [(if (:ant @ahead) 0 (ranks ahead))
                       (ranks ahead-left) (ranks ahead-right)]))
              loc)))))))

(defn evaporate
  "causes all the pheromones to evaporate a bit"
  []
  (dorun
    (for [x (range dim) y (range dim)]
      (dosync
        (let [p (place [x y])]
          (alter p assoc :pher (* evap-rate (:pher @p))))))))



(import
  '(java.awt Color Graphics Dimension)
  '(java.awt.image BufferedImage)
  '(javax.swing JPanel JFrame))

(def scale 5)

(defn fill-cell [^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-ant [ant ^Graphics g x y]
  (let [[hx hy tx ty] ({0 [2 0 2 4]
                        1 [4 0 0 4]
                        2 [4 2 0 2]
                        3 [4 4 0 0]
                        4 [2 4 2 0]
                        5 [0 4 4 0]
                        6 [0 2 4 2]
                        7 [0 0 4 4]}
                        (:dir ant))]
    (doto g
      (.setColor (if (:food ant)
                  (Color. 255 0 255 255)
                  (Color. 0 0 0 255)))
      (.drawLine (+ hx (* x scale)) (+ hy (* y scale))
                 (+ tx (* x scale)) (+ ty (* y scale))))))

(defn render-place [g p x y]
  (when (pos? (:pher p))
    (fill-cell g x y (Color. 0 255 0
                          (int (min 255 (* 255 (/ (:pher p) pher-scale)))))))
  (when (pos? (:food p))
    (fill-cell g x y (Color. 255 0 0
                          (int (min 255 (* 255 (/ (:food p) food-scale)))))))
  (when (:ant p)
    (render-ant (:ant p) g x y)))

(defn render [g]
  (let [v (dosync (apply vector (for [x (range dim) y (range dim)]
                                  @(place [x y]))))
        img (BufferedImage. (* scale dim) (* scale dim) BufferedImage/TYPE_INT_ARGB)
        bg (.getGraphics img)]
    (doto bg
      (.setColor Color/white)
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    (dorun
      (for [x (range dim) y (range dim)]
        (render-place bg (v (+ (* x dim) y)) x y)))
    (doto bg
      (.setColor Color/blue)
      (.drawRect (* scale home-off) (* scale home-off)
                (* scale nants-sqrt) (* scale nants-sqrt)))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(def panel (doto (proxy [JPanel] []
                   (paint [g] (render g)))
             (.setPreferredSize (Dimension.
                                  (* scale dim)
                                  (* scale dim)))))

(def frame (doto (JFrame.) (.add panel) (.pack) (.show)))

(def animator (agent nil))

(defn animation [x]
  (when running
    (send-off *agent* #'animation))
  (.repaint panel)
  (Thread/sleep animation-sleep-ms)
  nil)

(def evaporator (agent nil))

(defn evaporation [x]
  (when running
    (send-off *agent* #'evaporation))
  (evaporate)
  (Thread/sleep evap-sleep-ms)
  nil)
