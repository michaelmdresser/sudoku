; Sudoku solver!
; https://norvig.com/sudoku.html is a reference
; columns 1-9
; rows A-I
; collection of 9 squares (column, row, box) = unit

; (1) If a square has only one possible value, then eliminate that value from the square's peers. 
; (2) If a unit has only one possible place for a value, then put the value there.

; A1 A2 A3 | A4 A5 A6 | A7 A8 A9
; B1 B2 B3 | B4 B5 B6 | B7 B8 B9
; C1 C2 C3 | C4 C5 C6 | C7 C8 C9
; ------------------------------
; D1 D2 D3 | D4 D5 D6 | D7 D8 D9
; E1 E2 E3 | E4 E5 E6 | E7 E8 E9
; F1 F2 F3 | F4 F5 F6 | F7 F8 F9
; ------------------------------
; G1 G2 G3 | G4 G5 G6 | G7 G8 G9
; H1 H2 H3 | H4 H5 H6 | H7 H8 H9
; I1 I2 I3 | I4 I5 I6 | I7 I8 I9


;          |          |         
;    1     |    2     |   3     
;          |          |         
; ------------------------------
;          |          |         
;    4     |    5     |   6     
;          |          |         
; ------------------------------
;          |          |         
;    7     |    8     |   9     
;          |          |         



(def digits ["1" "2" "3" "4" "5" "6" "7" "8" "9"])
(def rows ["A" "B" "C" "D" "E" "F" "G" "H" "I"])
(def cols digits)
(def harderBoard "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")
(def trivialBoard "003020600900305001001806400008102900700000008006708200002609500800203009005010300")

; ordered for grid parsing
(def squares
  (for [row rows
        col cols]
    [row col]))

(def squaresWithAllPossible
  (apply hash-map (interleave squares (repeat (* 9 9) (set digits)))))

; TODO: add checking if we have the right number
; takes string of user input and turns it into a map
; from square ident to the value entered in the input
(defn cleanGrid [rawInput]
  (let [stringified (map str (seq rawInput))]
  (zipmap squares (filter (fn [c]
            (or (contains? (set digits) c)
                (contains? #{"0" "."} c))) stringified))))

(defn getRowMembers [row]
  (set (map (fn [col] [row col]) cols)))

(defn getColMembers [col]
  (set (map (fn [row] [row col]) rows)))

(defn cartesianProduct [c1 c2]
  (for [e1 c1
        e2 c2]
    [e1 e2]))

(def squareMembership
  (let [rowThirds [(subvec rows 0 3)
                   (subvec rows 3 6)
                   (subvec rows 6 9)]
        colThirds [(subvec cols 0 3)
                   (subvec cols 3 6)
                   (subvec cols 6 9)]]
    (for [rowThird rowThirds
          colThird colThirds]
      (set (cartesianProduct rowThird colThird)))))

(defn getSquarePeers [row col]
  (disj (first (filter (fn [squareMembers] (contains? squareMembers [row col]))
                 squareMembership)) [row col]))

(defn getRowPeers [row col]
  (let [allRowMembers (getRowMembers row)]
    (disj allRowMembers [row col])))

(defn getColPeers [row col]
  (let [allColMembers (getColMembers col)]
    (disj allColMembers [row col])))

(require '(clojure.set))

(defn getPeers [row col]
  (clojure.set/union (set (getRowPeers row col))
         (set (getColPeers row col))
         (set (getSquarePeers row col))))

(defn removeValueFromPeer [currentBoard value peer]
  (assoc currentBoard peer (disj (get currentBoard peer) value)))

(defn eliminate [value currentBoard peer]
  ;(prn "eliminating: " value " from peer: " peer)
  ;(prn currentBoard)
  (if (contains? (get currentBoard peer) value)
    (let [newBoard (removeValueFromPeer currentBoard value peer)]
      (cond
        (= (count (get newBoard peer)) 0) (throw (Exception. "why"))
        (= (count (get newBoard peer)) 1) (sudokuApply newBoard [peer (first (get newBoard peer))])
        :else (reduce (fn [board square] 
                        (if (= (count (get board square)) 1)
                          (sudokuApply board [square (first (get board square))])
                          board))
                      newBoard squares)))
    (do ;(println "peer " peer " does not contain " value)
        currentBoard)))

(defn sudokuApply [currentBoard newAssoc]
  (let [square (first newAssoc)
        value (second newAssoc)
        row (first square)
        col (second square)
        peers (getPeers row col)]
    ;(prn "square: " square " value: " value)
    ;(prn "current board: " currentBoard)
    (if (contains? (set digits) value) ; ignore the . and 0 values
      (do ;(prn "current board: " currentBoard)
          (reduce (partial eliminate value) currentBoard peers))
      currentBoard)))

(defn parseGrid [rawInput]
  (let [cleanedGrid (cleanGrid rawInput)
        initialPossible squaresWithAllPossible]
    ;(prn "cleaned" cleanedGrid)
   (reduce sudokuApply initialPossible cleanedGrid)))

(defn displayRow [board row]
  (doseq [col cols]
    (when (or (= col "4")
              (= col "7"))
      (print " | "))
    (print (first (get board [row col]))))
  )
  ;(map (fn [col] (print (first (get board [row col])))) cols))

(defn display [board]
  (doseq [row rows]
    (when (or (= row "D")
            (= row "G"))
      (println "---------------"))
    (displayRow board row)
    (newline)))

; (display (parseGrid trivialBoard))
