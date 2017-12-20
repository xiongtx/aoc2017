(ns aoc2017.day20
  (:require [clojure.algo.generic.functor :as functor]
            [clojure.string :as str]))


;;; Part 1

;; Suddenly, the GPU contacts you, asking for help. Someone has asked it to
;; simulate too many particles, and it won't be able to finish them all in
;; time to render the next frame at this rate.

;; It transmits to you a buffer (your puzzle input) listing each particle in
;; order (starting with particle 0, then particle 1, particle 2, and so
;; on). For each particle, it provides the X, Y, and Z coordinates for the
;; particle's position (p), velocity (v), and acceleration (a), each in the
;; format <X,Y,Z>.

;; Each tick, all particles are updated simultaneously. A particle's
;; properties are updated in the following order:

;; - Increase the X velocity by the X acceleration.

;; - Increase the Y velocity by the Y acceleration.

;; - Increase the Z velocity by the Z acceleration.

;; - Increase the X position by the X velocity.

;; - Increase the Y position by the Y velocity.

;; - Increase the Z position by the Z velocity.

;; Because of seemingly tenuous rationale involving z-buffering, the GPU would
;; like to know which particle will stay closest to position <0,0,0> in the
;; long term. Measure this using the Manhattan distance, which in this
;; situation is simply the sum of the absolute values of a particle's X, Y,
;; and Z position.

;; For example, suppose you are only given two particles, both of which stay
;; entirely on the X-axis (for simplicity). Drawing the current states of
;; particles 0 and 1 (in that order) with an adjacent a number line and
;; diagram of current X positions (marked in parenthesis), the following would
;; take place:

;; p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;; p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

;; p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;; p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

;; p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;; p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

;; p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
;; p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)

;; At this point, particle 1 will never be closer to <0,0,0> than particle 0,
;; and so, in the long run, particle 0 will stay closest.

;; Which particle will stay closest to position <0,0,0> in the long term?

(def line-regex #"p=< *(-?\d+),(-?\d+),(-?\d+)>, v=< *(-?\d+),(-?\d+),(-?\d+)>, a=< *(-?\d+),(-?\d+),(-?\d+)>")

(defn parse-line
  [line]
  (let [xs (re-find line-regex line)]
    (->> (rest xs)
         (map #(Long/parseLong %))
         (partition 3)
         (map vec)
         vec)))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map-indexed (fn [i line] [i (parse-line line)]))))

(defn manhattan
  [xs]
  (apply + (map #(Math/abs %) xs)))

(defn origin-comparator
  "Compare two particles by acceleration, velocity, and position.

  Long-term closer to origin is smaller."
  [p1 p2]
  (let [[mp1 mv1 ma1] (map manhattan (second p1))
        [mp2 mv2 ma2] (map manhattan (second p2))]
    (if (= ma1 ma2)
      (if (= mv1 mv2)
        (compare mp1 mp2)
        (compare mv1 mv2))
      (compare ma1 ma2))))

(defn closest
  [particles]
  (-> (sort origin-comparator particles)
      first
      first))


;;; Part 2

;; To simplify the problem further, the GPU would like to remove any particles
;; that collide. Particles collide if their positions ever exactly
;; match. Because particles are updated simultaneously, more than two
;; particles can collide at the same time and place. Once particles collide,
;; they are removed and cannot collide with anything else after that tick.

;; For example:

;; p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
;; p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
;; p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
;; p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

;; p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>
;; p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
;; p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)
;; p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

;; p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>
;; p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
;; p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)
;; p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

;; ------destroyed by collision------
;; ------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
;; ------destroyed by collision------                      (3)
;; p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>

;; In this example, particles 0, 1, and 2 are simultaneously destroyed at the
;; time and place marked X. On the next tick, particle 3 passes through
;; unharmed.

;; How many particles are left after all collisions are resolved?

(defn same-or-zero?
  [xs]
  (let [signums (map #(Long/signum %) xs)]
    (or (every? (some-fn zero? pos?) signums)
        (every? (some-fn zero? neg?) signums))))

(defn steady-state?
  "Particle is in steady state if, in every dim, its pos, vel, and acc are
  same sign or zero."
  [[_ specs]]
  (let [xs (map first specs)
        ys (map second specs)
        zs (map last specs)]
    (every? same-or-zero? [xs ys zs])))

(defn steady-state-signum
  "Steady state particle's signum in a dim.

  Steady state signum is 0 if each of pos, vel, and acc is 0; otherwise, the
  nonzero signum applies."
  [xs]
  (if (every? zero? xs)
    0
    (Long/signum (first (remove zero? xs)))))

(defn steady-state-type
  "Return a particle's steady state type.

  The type is [sx sy sz], where sx is the `steady-state-signum` of pos, vel,
  and acc in dim x, etc."
  [[_ specs]]
  (let [xs (map first specs)
        ys (map second specs)
        zs (map last specs)]
    (mapv steady-state-signum [xs ys zs])))

(defn position
  [[_ [pos & _]]]
  pos)

(defn remove-colliding
  [ps]
  (->> ps
       (sort-by position)
       (partition-by position)
       (remove #(> (count %) 1))
       (map first)))

(defn update-particle
  [[i [pos vel acc]]]
  (let [new-vel (mapv + vel acc)
        new-pos (mapv + pos new-vel)]
    [i [new-pos new-vel acc]]))

(defn steady-state-particles
  "Update particles until they are in steady state."
  [ps]
  (loop [ps (remove-colliding ps)]
    (if (or (empty? ps)
            (every? steady-state? ps))
      ps
      (recur (->> ps
                  (map update-particle)
                  remove-colliding)))))

(defn x-expanding?
  "Particles ps are x-expanding when those with higher acc also have higher vel
  and higher pos. I.e. they're moving further away from each other."
  [ps]
  (let [x-pos (comp first first second)
        x-vel (comp first second second)
        x-acc (comp first last second)]
    (= (sort-by x-pos ps)
       (sort-by x-vel ps)
       (sort-by x-acc ps))))

(defn y-expanding?
  [ps]
  (let [y-pos (comp second first second)
        y-vel (comp second second second)
        y-acc (comp second last second)]
    (= (sort-by y-pos ps)
       (sort-by y-vel ps)
       (sort-by y-acc ps))))

(defn z-expanding?
  [ps]
  (let [z-pos (comp last first second)
        z-vel (comp last second second)
        z-acc (comp last last second)]
    (= (sort-by z-pos ps)
       (sort-by z-vel ps)
       (sort-by z-acc ps))))

(defn noncolliding-group
  "Given particles with same `steady-state-type`, update until no
  collisions are possible."
  [ps]
  (loop [ps (remove-colliding ps)]
    (if (or (empty? ps)
            ;; `and` takes a very long time, but `or` could fail when two
            ;; particles have same pos, vel, acc in one dimension
            (x-expanding? ps)
            (y-expanding? ps)
            (z-expanding? ps))
      ps
      (recur (->> ps
                  (map update-particle)
                  remove-colliding)))))

(defn noncolliding-particles
  "For particles in steady state, group by `stead-state-type`, then update each
  group until no collisions are possible, returning noncolliding particles."
  [ps]
  (->> ps
       (group-by steady-state-type)
       (pmap (fn [[_ ps]] (noncolliding-group ps)))
       (apply concat)))

(defn num-noncolliding
  [ps]
  (-> ps
      steady-state-particles
      noncolliding-particles
      count))
