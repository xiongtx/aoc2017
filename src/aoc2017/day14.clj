(ns aoc2017.day14
  (:require [aoc2017.day10 :as day10]
            [clojure.core.reducers :as r]
            [clojure.pprint :refer [cl-format]]
            [clojure.string :as str]
            [jordanlewis.data.union-find :as uf]))


;;; Part 1

;; Suddenly, a scheduled job activates the system's disk defragmenter. Were
;; the situation different, you might sit and watch it for a while, but today,
;; you just don't have that kind of time. It's soaking up valuable system
;; resources that are needed elsewhere, and so the only option is to help it
;; finish its task as soon as possible.

;; The disk in question consists of a 128x128 grid; each square of the grid is
;; either free or used. On this disk, the state of the grid is tracked by the
;; bits in a sequence of knot hashes.

;; A total of 128 knot hashes are calculated, each corresponding to a single
;; row in the grid; each hash contains 128 bits which correspond to individual
;; grid squares. Each bit of a hash indicates whether that square is free (0)
;; or used (1).

;; The hash inputs are a key string (your puzzle input), a dash, and a number
;; from 0 to 127 corresponding to the row. For example, if your key string
;; were flqrgnkx, then the first row would be given by the bits of the knot
;; hash of flqrgnkx-0, the second row from the bits of the knot hash of
;; flqrgnkx-1, and so on until the last row, flqrgnkx-127.

;; The output of a knot hash is traditionally represented by 32 hexadecimal
;; digits; each of these digits correspond to 4 bits, for a total of 4 * 32 =
;; 128 bits. To convert to bits, turn each hexadecimal digit to its equivalent
;; binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes
;; 1110, f becomes 1111, and so on; a hash that begins with a0c2017... in
;; hexadecimal would begin with 10100000110000100000000101110000... in binary.

;; Continuing this process, the first 8 rows and columns for key flqrgnkx
;; appear as follows, using # to denote used squares, and . to denote free
;; ones:

;; ##.#.#..-->
;; .#.#.#.#
;; ....#.#.
;; #.#.##.#
;; .##.#...
;; ##..#..#
;; .#...#..
;; ##.#.##.-->
;; |      |
;; V      V

;; In this example, 8108 squares are used across the entire 128x128 grid.

;; Given your actual key string, how many squares are used?

;; Your puzzle input is hfdlxzhv.

(defn hexstr->binstr
  [xs]
  (->> xs
       (map #(Long/parseLong (str %) 16))
       (map #(cl-format nil "~4,'0B" %))
       str/join))

(defn rowstr->binstr
  [rs]
  (->> rs
       day10/parse-input-bytes
       day10/dense-hash
       hexstr->binstr))

(defn rowstr->num-used
  [rs]
  (->> rs
       rowstr->binstr
       (filter #(= % \1))
       count))

;; This is a case where parallelism really helps performance.

(defn num-used-sequential
  [ks]
  (reduce (fn [n i]
            (+ n (rowstr->num-used (str ks "-" i))))
          0
          (range 128)))

;; > (time (num-used-sequential "hfdlxzhv"))
;; "Elapsed time: 22605.590682 msecs"


(defn num-used-parallel
  [ks]
  (->> (range 128)
       (pmap #(rowstr->num-used (str ks "-" %)))
       (apply + )))

;; > (time (num-used-parallel "hfdlxzhv"))
;; "Elapsed time: 10135.822622 msecs"


;; Using a reducer is also an option, though be careful!
;; - ForkJoin only applies to vectors and maps (not lists!)
;; - Need to choose a good n for r/fold
;;   - Note that n = 32 gives similar performance to pmap
;;   - This is b/c 128 / 32 = 4, while pmap uses 2 + 4 = 6 procs

(defn num-used-reducer
  [ks]
  (->> (vec (range 128))
       (r/map #(rowstr->num-used (str ks "-" %)))
       (r/fold 32 + +)))

;; > (time (sut/num-used-reducer "hfdlxzhv"))
;; "Elapsed time: 10653.879244 msecs"


;;; Part 2

;; Now, all the defragmenter needs to know is the number of regions. A region
;; is a group of used squares that are all adjacent, not including
;; diagonals. Every used square is in exactly one region: lone used squares
;; form their own isolated regions, while several adjacent squares all count
;; as a single region.

;; In the example above, the following nine regions are visible, each marked
;; with a distinct digit:

;; 11.2.3..-->
;; .1.2.3.4
;; ....5.6.
;; 7.8.55.9
;; .88.5...
;; 88..5..8
;; .8...8..
;; 88.8.88.-->
;; |      |
;; V      V

;; Of particular interest is the region marked 8; while it does not appear
;; contiguous in this small view, all of the squares marked 8 are connected
;; when considering the whole 128x128 grid. In total, in this example, 1242
;; regions are present.

;; How many regions are present given your key string?

(defn make-grid
  [ks]
  (->> (range 128)
       (pmap #(-> (str ks "-" %) rowstr->binstr))
       (mapv vec)))

(defn adjacents
  [[i j]]
  (->> [[(dec i) j] [i (inc j)] [i (dec j)] [(inc i) j]]
       (filter (fn [[x y]] (and (<= 0 x 127) (<= 0 y 127))))))

(defn used?
  [grid [i j]]
  (= \1 (get-in grid [i j])))

(defn connect-adjacents
  [grid uf p]
  (reduce (fn [uf p2]
            (if (used? grid p2)
              (uf/union uf p p2)
              uf))
          uf
          (adjacents p)))

(defn num-regions
  [grid]
  (-> (let [used-ps (->> (for [x (range 128) y (range 128)] [x y])
                     (filter (partial used? grid)))
        uf (apply uf/union-find used-ps)]
        (reduce (partial connect-adjacents grid) uf used-ps))
      count))
