(ns peg-solitaire.core
  (:require [clojure.set :as set])
  (:gen-class))

(declare 
 user-entered-valid-move 
 user-entered-invalid-move 
 prompt-move
 prompt-rows
 prompt-empty-peg
 game-over)

;; Creating the board

(defn tri*
  "Generate lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Check if the number is triangular (i.e. 1, 3, 6, ...)"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "Return the triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Return row number of the position"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  "Form a mutual connection between the given position and 
  the one to its right"
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  "Form a mutual connection between the given position and 
  the one to its down-left"
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  "Form a mutual connection between the given position and
  the one to its down-right"
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Peg the board and perform connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Create a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos]
              (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

;; Moving pegs

(defn pegged?
  "Check if the position has a peg in it"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at the given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at the given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take the peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return a map of all valid moves for the given position,
  where the key is the destination and the value is
  the jumped position"
  [board pos]
  (into {} (filter (fn [[destination jumped]]
                     (and (pegged? board jumped)
                          (not (pegged? board destination))))
                   (get-in board [pos :connections]))))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid,
  nil otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

;; Rendering and printing the board

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(def ansi-styles
  {:red "[31m"
   :green "[32m"
   :blue "[34m"
   :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize "+" :blue)
         (colorize "-" :red))))

(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))

(defn row-padding
  "String of spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))

(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

;; Player interaction

(defn letter->pos
  "Convert a letter string to the corresponding position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Wait for user to enter text and hit enter, then clean the input"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn characters-as-strings
  "Given a string, return a collection consisting of each
  individual character"
  [string]
  (re-seq #"[a-zA-Z]" string))

(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input " ")))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn user-entered-valid-move
  "Handle the next step after a user has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn user-entered-invalid-move
  "Handle the next step after a user has entered an invalid move"
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))

(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (prompt-rows)
      (do
        (println "Bye!")
        (System/exit 0)))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn -main
  [& args]
  (println "Get ready to play peg solitaire!")
  (prompt-rows))
