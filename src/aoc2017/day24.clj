(ns aoc2017.day24
  (:require [clojure.set :as set]
            [clojure.string :as str]))


;;; Part 1

;; The CPU itself is a large, black building surrounded by a bottomless
;; pit. Enormous metal tubes extend outward from the side of the building at
;; regular intervals and descend down into the void. There's no way to cross,
;; but you need to get inside.

;; No way, of course, other than building a bridge out of the magnetic
;; components strewn about nearby.

;; Each component has two ports, one on each end. The ports come in all
;; different types, and only matching types can be connected. You take an
;; inventory of the components by their port types (your puzzle input). Each
;; port is identified by the number of pins it uses; more pins mean a stronger
;; connection for your bridge. A 3/7 component, for example, has a type-3 port
;; on one side, and a type-7 port on the other.

;; Your side of the pit is metallic; a perfect surface to connect a magnetic,
;; zero-pin port. Because of this, the first port you use must be of type
;; 0. It doesn't matter what type of port you end with; your goal is just to
;; make the bridge as strong as possible.

;; The strength of a bridge is the sum of the port types in each
;; component. For example, if your bridge is made of components 0/3, 3/7, and
;; 7/4, your bridge has a strength of 0+3 + 3+7 + 7+4 = 24.

;; For example, suppose you had the following components:

;; 0/2
;; 2/2
;; 2/3
;; 3/4
;; 3/5
;; 0/1
;; 10/1
;; 9/10

;; With them, you could make the following valid bridges:

;; - 0/1
;; - 0/1--10/1
;; - 0/1--10/1--9/10
;; - 0/2
;; - 0/2--2/3
;; - 0/2--2/3--3/4
;; - 0/2--2/3--3/5
;; - 0/2--2/2
;; - 0/2--2/2--2/3
;; - 0/2--2/2--2/3--3/4
;; - 0/2--2/2--2/3--3/5

;; (Note how, as shown by 10/1, order of ports within a component doesn't
;; matter. However, you may only use each port on a component once.)

;; Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength
;; of 0+1 + 1+10 + 10+9 = 31.

;; What is the strength of the strongest bridge you can make with the
;; components you have available?

(defn parse-line
  [line]
  (let [[_ p1 p2] (re-find #"(\d+)/(\d+)" line)]
    (mapv #(Long/parseLong %) [p1 p2])))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map parse-line)
       frequencies))

(defn remove-component
  "Remove one copy of component c from components frequency map."
  [c components]
  (if (= 1 (get components c))
    (dissoc components c)
    (update components c dec)))

(defn extend-bridge
  "Return all [bridge components] pairs from extending bridge by one component."
  [[bridge components]]
  (let [[_ b] (or (last bridge) [0 0])]
    (for [[x y] (keys components)
          :when (or (= b x) (= b y))
          :let [new (if (= b x) [x y] [y x])]]
      [(conj bridge new) (remove-component [x y] components)])))

(defn finished?
  "Return true if bridge-component pair cannot be extended."
  [pair]
  (empty? (extend-bridge pair)))

(defn bridges
  "Return all non-extensible bridges that can be built from components."
  [components]
  (loop [bs (extend-bridge [[] components])
         finished #{}]
    (if (empty? bs)
      (map first finished)
      (let [fs (filter finished? bs)]
        (recur (mapcat extend-bridge (set/difference (set bs) fs))
               (set/union finished fs))))))

(defn strength
  [bridge]
  (apply + (flatten bridge)))

(defn strongest-bridge-strength
  [components]
  (->> (bridges components)
       (map strength)
       (apply max)))


;;; Part 2

;; The bridge you've built isn't long enough; you can't jump the rest of the
;; way.

;; In the example above, there are two longest bridges:

;; 0/2--2/2--2/3--3/4
;; 0/2--2/2--2/3--3/5

;; Of them, the one which uses the 3/5 component is stronger; its strength is
;; 0+2 + 2+2 + 2+3 + 3+5 = 19.

;; What is the strength of the longest bridge you can make? If you can make
;; multiple bridges of the longest length, pick the strongest one.

(defn strongest-longest-bridge-strength
  [components]
  (->> (bridges components)
       (sort #(> (count %1) (count %2)))
       (partition-by count)
       first
       (map strength)
       (apply max)))
