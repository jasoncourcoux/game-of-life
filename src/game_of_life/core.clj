(ns game-of-life.core
  (:require [clojure.spec :as s]
            [clojure.test.check.generators :as gen]
            [clojure.spec.test :as stest]))

;; We would like a function which will take a board representing the current state, and return a new board with the
;; next state.
;;
;; Refering to the rules on wikipedia a board is an infinite two-dimensional grid of square cells, where each cell
;; can be in one of two states - either dead or alive.
;;
;; So we can start to spec out what a cell looks like - There are several ways we can do this, use 0 or 1 for example
;; to represent each state, or use keywords to represent, the two different state
;;
;; (def valid-cell-state? #{0 1})
;; (def valid-cell-state? #{:alive :dead})
;;
;; I'm going to stick with 0 or 1 for now as I suspect this will make a board more readable - but this can easilly be
;; changed later on:

(def cell? #{0 1})

;; Now we can spec out what a row and board looks like:-
(s/def ::row (s/coll-of cell? :min-count 1))
(s/def ::board (s/coll-of ::row))

;; And spec our function so we can create some tets output to exercise it.
(s/fdef next-state :args (s/cat :board ::board))

(s/exercise (:args (s/get-spec `next-state)))

;; We've missed the fact that every row needs to be the same size, as a board is rectangular.
;; so we can modify our board spec to be something like the following:
(s/def ::board (s/and (s/coll-of ::row :min-count 1)
                      #(apply = (map count %))))

;; This appears to work, however exercising the function appears to struggle to generate
;; data which satisfies the constraints and running in the repl, often seems to result in
;; ExceptionInfo Couldn't satisfy such-that predicate after 100 tries.
(s/exercise (:args (s/get-spec `next-state)))

;; So we will have to come up with a custom generator to create our board, we can use bind, to
;; create a generator that takes the output from one generator and feeds it into the next.
(s/def ::board (s/with-gen
                 (s/and (s/coll-of ::row :min-count 1)
                        #(apply = (map count %)))
                 #(gen/bind (s/gen pos-int?)
                            (fn [n]
                              (s/gen (s/coll-of (s/coll-of cell? :count n)))))))

(s/exercise (:args (s/get-spec `next-state)))

;; This is now producing some sensible results, and we can use this as an input for our function;

;; The return state is also a board, so we can add this in to the function spec
(s/fdef next-state
        :args (s/cat :board ::board)
        :ret (s/cat :board ::board))

(s/exercise (:args (s/get-spec `next-state)))

;; We know the size of the input board will be the same size as the output board, so we can
;; add this constraint to the output spec. I'm also going to more tightly constrain my board
;; to be a maximum of 12x12 for now - this is purely so I can get more benefit from the
;; data generated in the repl - I can expand it out again later.
(s/def ::board (s/with-gen
                 (s/and (s/coll-of ::row :min-count 1 :max-count 12)
                        #(apply = (map count %)))
                 #(gen/bind (s/gen (s/int-in 1 12))
                            (fn [n]
                              (s/gen (s/coll-of (s/coll-of cell? :count n)))))))

(s/fdef next-state
        :args (s/cat :input ::board)
        :ret ::board
        :fn (fn [{{input :input} :args output :ret}]
              (let [input-height (count input)
                    output-height (count output)
                    input-width (-> input first count)
                    output-width (-> output first count)]
                (and (= input-width output-width)
                     (= input-height output-height)))))

;; We create a dummy function here, for testing
(defn next-state [board]
  (reverse board))

;; Test our function with clojure.specs test/check wrapper
(stest/check `next-state)
;;=>
;; ({:spec #object[clojure.spec$fspec_impl$reify__14270 0xa5b9f5 "clojure.spec$fspec_impl$reify__14270@a5b9f5"],
;;  :clojure.spec.test.check/ret {:result true, :num-tests 1000, :seed 1476786370486},
;;  :sym game-of-life.core/next-state})

;; Now we can start to develop our solution, I'm going to use some wishful thinking to shape
;; out how I want the function to behave, and then we can start to spec out the individual
;; parts to the function.
(defn count-live-neighbours [cell-position board])

(defn next-state [board]
  (map-indexed (fn [row-index row]
                 (map-indexed (fn [col-index cell]
                                (let [live-n (count-live-neighbours {::row-index row-index ::col-index col-index} board)]
                                  (cond (= cell 0) (if (= live-n 3) 1 0) ;;Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
                                        (< live-n 2) 0      ;;Any live cell with fewer than two live neighbours dies, as if caused by under-population.
                                        (<= live-n 3) 1     ;;Any live cell with two or three live neighbours lives on to the next generation.
                                        :else 0)))          ;;Any live cell with more than three live neighbours dies, as if by over-population.
                              row))
               board))

;; Assuming, we have a function count-live-neighbours - this should solve our problem, so next we can look at spec'ing
;; out count-live-neighbours. As before, we start with the args

(s/def ::row-index pos-int?)

(s/def ::col-index pos-int?)

(s/fdef count-live-neighbours
        :args (s/cat :coords (s/keys :req [::row-index ::col-index])
                     :board ::board))

;; And we can have a quick look to make sure this is generating something sensible
(s/exercise (:args (s/get-spec `count-live-neighbours)))

;; Next we add the constraint that the cell position, must be within the board.
(s/fdef count-live-neighbours
        :args (s/and (s/cat :coords (s/keys :req [::row-index ::col-index])
                            :board ::board)
                     (fn [{:keys [coords board]}]
                       (let [height (count board)
                             width (-> board first count)]
                         (and (< (::row-index coords) height)
                              (< (::col-index coords) width))))))

;; This starts to cause some problems with the generator not being able to satisfy the
;; constraints within a small number of tries as the ::row-index and ::col-index are not
;; particularly tightly constrained. To fix this, I'm going to apply the previous min/max
;; values, but we can pull those out in to varibales so they are kept in sync.

(def max-width 12)

(def max-height 12)

;; Redefine the row and board specs
(s/def ::row (s/coll-of cell? :min-count 1 :max-count max-width))

(s/def ::board (s/with-gen
                 (s/and (s/coll-of ::row :min-count 1 :max-count max-height)
                        #(apply = (map count %)))
                 #(gen/bind (s/gen (s/int-in 1 max-width))
                            (fn [n]
                              (s/gen (s/coll-of (s/coll-of cell? :count n)))))))

;; Redefine our ::row-index and ::col-specs with the new max width and height variables
(s/def ::row-index (s/int-in 1 max-height))

(s/def ::col-index (s/int-in 1 max-width))

;; And now we can consistently generate values within the right range
(s/exercise (:args (s/get-spec `count-live-neighbours)))

;; Next we spec the return value which is an integer between 0 and 9
(s/fdef count-live-neighbours
        :args (s/and (s/cat :coords (s/keys :req [::row-index ::col-index])
                            :board ::board)
                     (fn [{:keys [coords board]}]
                       (let [height (count board)
                             width (-> board first count)]
                         (and (< (::row-index coords) height)
                              (< (::col-index coords) width)))))
        :ret (s/int-in 0 9))

;; There isn't any obvious constraint between our input and output here, so we can skip straight to
;; starting to implement the function, again using wishful thinking to extract the other functions
;; we need to create.
(defn neighbours [cell-position board] (list 1 0 0 0 1 1 0 0 1))

(defn count-live-neighbours [cell-position board]
  (let [n (neighbours cell-position board)
        live-n (filter #{1} n)]
    (count live-n)))

;; And we can run some tests to show our structure is at least right
(stest/check `count-live-neighbours)

;; Now we repeat for the neighbours function - this is going to have identical args, so lets extract that
;; so we can reuse it
(s/def ::coordinates-and-board (s/and (s/cat :coords (s/keys :req [::row-index ::col-index])
                                             :board ::board)
                                      (fn [{:keys [coords board]}]
                                        (let [height (count board)
                                              width (-> board first count)]
                                          (and (< (::row-index coords) height)
                                               (< (::col-index coords) width))))))

;; And redefine count-live-neighbours function spec
(s/fdef count-live-neighbours
        :args ::coordinates-and-board
        :ret (s/int-in 0 10))

;; And create our new neighburs spec
(s/fdef neighbours
        :args ::coordinates-and-board
        :ret (s/coll-of #{0 1} :min-count 0 :max-count 9))

(s/exercise (:args (s/get-spec `neighbours)))

(s/exercise (:ret (s/get-spec `neighbours)))

;; Now lets write our function, again using wishful thinking for the missing function which gets the neighbour
;; coordinates - I'm in two minds about whether to leave this inline, but I feel it's probably a seperate operation
;; that this function is performing - if anything, I might move the mapping over the coordinates to get the values
;; back into the count-live-neighbours function in my final solution - but will keep them seperate for now.
(defn get-neighbour-coordinates [cell-position width height]
  [])

(defn neighbours [cell-position board]
  (let [height (count board)
        width (-> board first count)
        n-coords (get-neighbour-coordinates cell-position width height)]
    (map (fn [[x y]] (get-in board [y x])) n-coords)))

;; And test our functions
(stest/check `neighbours)

;; We can pull out the ::cell-position spec so we can reuse it in the get-neighbour-coordinates function spec
(s/def ::cell-position (s/keys :req [::row-index ::col-index]))

(s/def ::coordinates-and-board (s/and (s/cat :coords ::cell-position
                                             :board ::board)
                                      (fn [{:keys [coords board]}]
                                        (let [height (count board)
                                              width (-> board first count)]
                                          (and (< (::row-index coords) height)
                                               (< (::col-index coords) width))))))

;; And spec out our function
(s/fdef get-neighbour-coordinates
        :args (s/cat :coords ::cell-position
                     :width pos-int?
                     :height pos-int?)
        :ret (s/coll-of (s/tuple pos-int? pos-int?) :min-count 0 :max-count 9))

(s/exercise-fn `get-neighbour-coordinates)

;; Now we can implement the get-neighbour-coordinates function, and here we haven't had to use any wishful thinking,
;; so should have all the code needed to generate the next step for any given board.
(defn get-neighbour-coordinates [cell-position width height]
  (let [xs (->> [-1 0 1]
                (map #(+ (::col-index cell-position) %))
                (filter #(<= 0 % (dec width))))
        ys (->> [-1 0 1]
                (map #(+ (::row-index cell-position) %))
                (filter #(<= 0 % (dec height))))]
    (filter #(not= [(::col-index cell-position) (::row-index cell-position)] %)
            (for [x xs y ys] [x y]))))

;; Evaluate some tests for out next-state function, and it looks like we have a function which is working as expected.
(s/exercise-fn `next-state)












