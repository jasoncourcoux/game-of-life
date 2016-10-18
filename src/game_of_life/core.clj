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
;; to represent each state, or use keywords to represent, the two different states
;;
;; (def valid-cell-state? #{0 1})
;; (def valid-cell-state? #{:alive :dead})
;;
;; I'm going to stick with 0 or 1 for now as I suspect this will make a board more readable when generating values in
;; the repl - but this can easily be chenged later on.

;; By splitting the values for dead/live out, we can use them in our specs later on if needed.
(def dead-cell 0)

(def live-cell 1)

(def cell? #{dead-cell live-cell})

;; Now we can spec out what a row and board looks like - we use the built in collection specs here
(s/def ::row (s/coll-of cell? :min-count 1))

(s/def ::board (s/coll-of ::row))

;; And now we can spec our function so we can create some test output to exercise it.
(s/fdef next-state :args (s/cat :board ::board))

(s/exercise (:args (s/get-spec `next-state)))

;; We've missed the fact that every row needs to be the same size, as a board is rectangular. Here we modify our board
;; spec to deal with this. s/and flows conformed values through so we can add this check like so:
(s/def ::board (s/and (s/coll-of ::row :min-count 1)
                      #(apply = (map count %))))

(s/exercise (:args (s/get-spec `next-state)))

;; This appears to work, however when trying to exercise the function, it doesn't always generate data as the data
;; generated doens't always satisfy the predicate within a reasonble set:
;;
;; This results in the occasional:
;; ExceptionInfo Couldn't satisfy such-that predicate after 100 tries.
;;
;; To deal with this, we will have to come up with a custom generator to generate boards, we can use bind, to
;; create a generator that takes the output from one generator and feeds it into the next, and then use the with-gen
;; function to get a new spec which uses our custom genertor:
(s/def ::board (s/with-gen
                 (s/and (s/coll-of ::row :min-count 1)
                        #(apply = (map count %)))
                 #(gen/bind (s/gen pos-int?)
                            (fn [n]
                              (s/gen (s/coll-of (s/coll-of cell? :count n)))))))

;; This is now producing some sensible results, and we can use this as an input for our function.
(s/exercise (:args (s/get-spec `next-state)))

;; The return state is also a board, so we can add this in to the function spec. As the return value is just a single
;; value we can use it directly, as so:
(s/fdef next-state
        :args (s/cat :board ::board)
        :ret ::board)

(s/exercise (:args (s/get-spec `next-state)))

;; We know the size of the input board will be the same size as the output board, so we can add this constraint to the
;; output spec using the :fn option. We are also going to more tightly constrain the board to be a maximum of 12x12 - this
;; is purely so we can get more benefit from the data generated in the repl - and we cna expand out later if required.
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

;; We create a skelton function here so we can exercise it and check that our data is looking to be the right shape.
;; For now, we'll just return the same board that's passed to it.
(defn next-state [board]
  board)

;; We can exercise the function using to see that the arguments and return values are as expected.
(s/exercise-fn `next-state)

;; And also test our function with clojure.specs test/check wrapper to call it 1000 times and make sure it always
;; conforms to the spec.
(stest/check `next-state)

;; This gives us enough that we can now we can start to develop our solution. Here we are going to use some wishful
;; thinking to shape out how we want the function to behave, and if anymore functions are required we can start to spec
;; those out in a similar manner.
(defn count-live-neighbours [cell-position board])

(defn next-state [board]
  (map-indexed (fn [row-index row]
                 (map-indexed (fn [col-index cell]
                                (let [live-n (count-live-neighbours {::row-index row-index ::col-index col-index} board)]
                                  (cond (= cell dead-cell) (if (= live-n 3) live-cell dead-cell) ;;Any dead cell with exactly three live neighbours becomes a live cell
                                        (< live-n 2) dead-cell ;;Any live cell with fewer than two live neighbours dies
                                        (<= live-n 3) live-cell ;;Any live cell with two or three live neighbours lives
                                        :else dead-cell)))  ;;Any live cell with more than three live neighbours dies
                              row)) board))

;; Assuming, we have a function count-live-neighbours - this should solve our problem, so next we can look at spec'ing
;; out count-live-neighbours. As before, we start with the args
(s/def ::row-index pos-int?)

(s/def ::col-index pos-int?)

(s/fdef count-live-neighbours
        :args (s/cat :coords (s/keys :req [::row-index ::col-index])
                     :board ::board))

;; And we can have a quick look to make sure this is generating something sensible:
(s/exercise (:args (s/get-spec `count-live-neighbours)))

;; Next we add the constraint that the cell position, must be within the board - again we can use the fact that s/and
;; flows conformed values:
(s/fdef count-live-neighbours
        :args (s/and (s/cat :coords (s/keys :req [::row-index ::col-index])
                            :board ::board)
                     (fn [{:keys [coords board]}]
                       (let [height (count board)
                             width (-> board first count)]
                         (and (< (::row-index coords) height)
                              (< (::col-index coords) width))))))

;; As before, this starts to cause some problems with the generator not being able to satisfy the constraints within a
;; small number of tries, as the ::row-index and ::col-index are not particularly tightly constrained. To fix this, I'm
;; going to apply the previous min/max values, but we can pull those out in to varibales so they are kept in sync and can
;; be easily updated at a later date.
(def max-width 12)

(def max-height 12)

;; Redefine the row and board specs using the new variables:
(s/def ::row (s/coll-of cell? :min-count 1 :max-count max-width))

(s/def ::board (s/with-gen
                 (s/and (s/coll-of ::row :min-count 1 :max-count max-height)
                        #(apply = (map count %)))
                 #(gen/bind (s/gen (s/int-in 1 max-width))
                            (fn [n]
                              (s/gen (s/coll-of (s/coll-of cell? :count n)))))))

;; Redefine our ::row-index and ::col-specs with the new variables
(s/def ::row-index (s/int-in 1 max-height))

(s/def ::col-index (s/int-in 1 max-width))

;; And now we can consistently generate values within the right range
(s/exercise (:args (s/get-spec `count-live-neighbours)))

;; Next we spec the return value which is an integer between 0 and 8 - 0 when it's a 1x1 board
(s/fdef count-live-neighbours
        :args (s/and (s/cat :coords (s/keys :req [::row-index ::col-index])
                            :board ::board)
                     (fn [{:keys [coords board]}]
                       (let [height (count board)
                             width (-> board first count)]
                         (and (< (::row-index coords) height)
                              (< (::col-index coords) width)))))
        :ret (s/int-in 0 8))

;; There isn't any obvious constraint between our input and output here, so we can skip straight to starting to
;; implement the function, again using wishful thinking to extract the other functions we need to create, and just
;; stubbing those out with some dummy data.
(defn neighbours [cell-position board] (list 1 0 0 0 1 1 0 0 1))

(defn count-live-neighbours [cell-position board]
  (let [n (neighbours cell-position board)
        live-n (filter #{live-cell} n)]
    (count live-n)))

;; And we can run some tests to check our structure is looking sensible, or exercise the function so we can inspect the
;; data in the repl
(stest/check `count-live-neighbours)
(s/exercise-fn `count-live-neighbours)

;; Now we repeat for the neighbours function - this is going to have identical args to the count-live-neighbours, so
;; lets extract that spec so we can reuse it.
(s/def ::coordinates-and-board (s/and (s/cat :coords (s/keys :req [::row-index ::col-index])
                                             :board ::board)
                                      (fn [{:keys [coords board]}]
                                        (let [height (count board)
                                              width (-> board first count)]
                                          (and (< (::row-index coords) height)
                                               (< (::col-index coords) width))))))

;; And now redefine count-live-neighbours function spec
(s/fdef count-live-neighbours
        :args ::coordinates-and-board
        :ret (s/int-in 0 10))

;; And create our new neighburs spec with a return value which contains a collection cells
(s/fdef neighbours
        :args ::coordinates-and-board
        :ret (s/coll-of cell? :min-count 0 :max-count 9))

;; Again we can exercise this to check the generated data is looking okay from within the repl.
(s/exercise (:args (s/get-spec `neighbours)))
(s/exercise (:ret (s/get-spec `neighbours)))

;; Now lets write our function, again using wishful thinking for the missing function which gets the neighbour
;; coordinates - I'm in two minds about whether to leave this inline, but I feel it's probably a seperate operation
;; that this function is performing - if anything, I might end up mapping over the coordinates from within
;; count-live-neighbours later on - but will keep them seperate for now.
(defn get-neighbour-coordinates [cell-position width height]
  [])

(defn neighbours [cell-position board]
  (let [height (count board)
        width (-> board first count)
        n-coords (get-neighbour-coordinates cell-position width height)]
    (map (fn [[x y]] (get-in board [y x])) n-coords)))

;; And exercise/test the neighbours function
(s/exercise-fn `neighbours)
(stest/check `neighbours)

;; We can pull out the ::cell-position spec so we can reuse it in the get-neighbour-coordinates function spec
(s/def ::cell-position (s/keys :req [::row-index ::col-index]))

;; Redefine ::coordinates-and-board spec using the ::cell-position spec
(s/def ::coordinates-and-board (s/and (s/cat :coords ::cell-position
                                             :board ::board)
                                      (fn [{:keys [coords board]}]
                                        (let [height (count board)
                                              width (-> board first count)]
                                          (and (< (::row-index coords) height)
                                               (< (::col-index coords) width))))))

;; And spec out get-neighbour-coordinates
(s/fdef get-neighbour-coordinates
        :args (s/cat :coords ::cell-position
                     :width nat-int?
                     :height nat-int?)
        :ret (s/coll-of (s/tuple nat-int? nat-int?) :min-count 0 :max-count 9))

;; As before, exercise the function
(s/exercise-fn `get-neighbour-coordinates)


;; Now we can implement the get-neighbour-coordinates function.
(defn get-neighbour-coordinates [cell-position width height]
  (let [xs (->> [-1 0 1]
                (map #(+ (::col-index cell-position) %))
                (filter #(<= 0 % (dec width))))
        ys (->> [-1 0 1]
                (map #(+ (::row-index cell-position) %))
                (filter #(<= 0 % (dec height))))]
    (filter #(not= [(::col-index cell-position) (::row-index cell-position)] %)
            (for [x xs y ys] [x y]))))

;; And generate some tests
(stest/check `get-neighbour-coordinates)

;; As this doesn't require any extra functions to be created, we should have all the code needed to generate the next
;; step for a given board. So we can evaluate some tests for out next-state function to inspect that it's all behaving
;; as expected.
(s/exercise-fn `next-state)















