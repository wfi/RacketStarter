#lang racket

;;;; ants.rkt

;;; State Class

(define state%
  (class object%
    (field [params (make-hasheq '((rows . 100)
                                  (cols . 100)
                                  (turn-time . 1000) ;; in milliseconds
                                  (load-time . 3000) ;; in milliseconds
                                  (view-radius2 . 93)
                                  (attack-radius2 . 6)
                                  (spawn-radius2 . 6)
                                  (turns . (void))))]
           )
    (field [game-map (void)])
    (field [enemy-ants empty])
    (field [my-ants empty])
    (field [food (void)])
    (field [hills empty])
    (field [turn-start-time (void)])
    (field [turn (void)])
    
    (super-new)
    
    
    ;;; Helper Functions

    (define (set-map! m r c v)
      (vector-set! (vector-ref m r) c v))
    (define (get-map m r c)
      (vector-ref (vector-ref m r) c))
    
    (define (distance row1 col1 row2 col2)
      "Returns the shortest distance between ROW1,COL1 and ROW2,COL2 for a grid that wraps around."
      (let* ((drow (abs (- row1 row2)))
             (dcol (abs (- col1 col2)))
             (minrow (min drow (- (hash-ref params 'rows) drow)))
             (mincol (min dcol (- (hash-ref params 'cols) dcol))))
        (sqrt (+ (* minrow minrow) (* mincol mincol)))))
    
    (define/public (issue-order row col direction)
      "Prints a formatted order for ROW,COL and DIRECTION to standard output.
  Silently drops orders when DIRECTION isn't one of :north, :east, :south
  or :west."
      (when (member direction '(#:north #:east #:south #:west))
        (fprintf (current-output-port) "o ~A ~A ~A~%" row col
                 (case direction
                   ((#:north) "N")
                   ((#:east)  "E")
                   ((#:south) "S")
                   ((#:west)  "W")))))
    
    ;; TODO needs better docstring (needs better code as well!)
    (define (new-location row col direction)
      "Returns '(NEW-ROW . NEW-COL) for ROW,COL and DIRECTION for a grid that
  wraps around."
      (let ((dst-row (cond ((eq? direction '#:north)
                            (if (<= row 0)
                                (- (hash-ref params 'rows) 1)
                                (- row 1)))
                           ((eq? direction '#:south)
                            (if (>= (+ row 1) (hash-ref params 'rows))
                                0
                                (+ row 1)))
                           (else row)))
            (dst-col (cond ((eq? direction '#:east)
                            (if (>= (+ col 1) (hash-ref params 'cols))
                                0
                                (+ col 1)))
                           ((eq? direction '#:west)
                            (if (<= col 0)
                                (- (hash-ref params 'cols) 1)
                                (- col 1)))
                           (else col))))
        (cons dst-row dst-col)))
    
    (define (water? row col direction)
      "Returns T if the tile in the DIRECTION of ROW,COL is water, otherwise
  returns NIL."
      (let ((nl (new-location row col direction)))
        (= 1 (vector-ref (vector-ref game-map (car nl)) (cdr nl)))))
    
    (define (finish-turn )
      "Prints the \"finish turn\" string to standard output."
      (format (current-output-port) "~&go~%")
      (flush-output (current-output-port)))
    
    (define (starts-with sequence subsequence)
      "Checks if the given sequence contains the subsequence"
      (let ((sublen (length subsequence)))
        (when (and (> sublen 0)
                   (<= sublen (length sequence)))
          (string=? (substring sequence 0 sublen) subsequence))))
    
    (define (setup)
      "Parses turn 0 game parameters and sets them in *STATE*.  Also creates
  initial game map and assigns it to (GAME-MAP *STATE*)."
      (do ((line (regexp-split " " (read-line (current-input-port)))
                 (regexp-split " " (read-line (current-input-port)))))
        ((string=? (first line) "ready") #t)
        (hash-set! params
                   (string->symbol (first line))
                   (string->number (second line))))
      (let ((lcols (hash-ref params 'cols)))
        (set! game-map
              (build-vector (hash-ref params 'rows)
                            (lambda (_)
                              (make-vector lcols 0))))))
    
    ;; TODO is this the right thing to do?
    (define (reset-game-map )
      "Sets all tiles on the map to land (0) if they're not already land or
  water (1).  Modifies (GAME-MAP *STATE*)."
      (for/vector ((r game-map))
                  (vector-map! (lambda (c)
                                 (if (> c 1) 0 c))
                               r)))
    ;; TODO needs a docstring
    (define (split-state-string string)
      (regexp-split " +" string))
    

    (define (set-ant string)
      "Parses the \"a row col owner\" STRING and sets the specific map tile to
  an ant of owner.  Modifies (ENEMY-ANTS *STATE*), (MY-ANTS *STATE*) and
  (GAME-MAP *STATE*)."
      (let* ((split (split-state-string string))
             (r (string->number (second split)))
             (c (string->number (third split)))
             (owner (string->number (fourth split))))
        (if (= owner 0)
            (set! my-ants (cons (list r c)
                                my-ants))
            (set! enemy-ants (cons (list r c)
                                   enemy-ants)))
        (set-map! game-map r c (+ owner 100))))

    (define (set-dead string)
      "Parses the \"d row col owner\" STRING and sets the specific map tile to
  a dead ant of owner.  Modifies (GAME-MAP *STATE*)."
      (let* ((split (split-state-string string))
             (r (string->number (second split)))
             (c (string->number (third split)))
             (owner (string->number (fourth split))))
        (unless (= 2 (vector-ref (vector-ref game-map r) c))
          (set-map! game-map r c (+ owner 200)))))
    
    (define (set-food string)
      "Parses the \"f row col\" STRING and sets the specific map tile to food.
  Modifies (FOOD *STATE*) and (GAME-MAP *STATE*)."
      (let* ((split (split-state-string string))
             (r (string->number (second split)))
             (c (string->number (third split))))
        (set! food (cons (list r c) food))
        (set-map! game-map r c 2)))
    
    (define (set-water string)
      "Parses the \"w row col\" STRING and sets the specific map tile to water.
  Modifies (GAME-MAP *STATE*)."
      (let* ((split (split-state-string string))
             (r (string->number (second split)))
             (c (string->number (third split))))
        (set-map! game-map r c 1)))
  
    
    ;; TODO detect the razing of hills
    (define (set-hill string)
      "Parses the \"a row col owner\" STRING and sets the specific map tile to
  a hill of owner.  Modifies (HILLS *STATE*) and (GAME-MAP *STATE*)."
      (let* ((split (split-state-string string))
             (r (string->number (second split)))
             (c (string->number (third split)))
             (owner (string->number (fourth split))))
        (let ((hill-record (list r c owner)))
          (unless (member hill-record hills)
            (set! hills (cons hill-record hills))))
        (set-map! game-map r c (+ owner 300))))
        
    
    (define (parse-turn )
      "Parses a typical turn.  Modifies *STATE* indirectly through RESET-GAME-MAP
  and the SET-* functions."
      (reset-game-map)
      (do ((line (read-line (current-input-port))
                 (read-line (current-input-port)))
           )
        ((string=? (substring line 0 2) "go") #t)
        (case (string->symbol (first line))
          ((f) (set-food line))
          ((w) (set-water line))
          ((a) (set-ant line))
          ((d) (set-dead line))
          ((h) (set-hill line)))))
    
    (define (reset-some-state )
      "Sets ENEMY-ANTS, MY-ANTS and FOOD to empty."
      (set! enemy-ants empty)
      (set! my-ants empty)
      (set! food empty)
      )
    
    
    (define (parse-game-state )
      "Calls either PARSE-TURN or PARSE-GAME-PARAMETERS depending on the line
  on standard input.  Modifies *STATE* and returns T if the game has ended,
  otherwise NIL."
      (set! turn-start-time (current-milliseconds))
      (reset-some-state)
      (do ((line (read-line (current-input-port)) (read-line (current-input-port))))
        ((> (string-length line) 0)
         (cond [(string=? (substring line 0 3) "end")
                (parse-turn) #t]
               [(string=? (substring line 0 6) "turn 0")
                (set! turn 0)
                (setup) #f]
               [(string=? (substring line 0 5) "turn ")
                (set! turn (string->number (second (regexp-split " +" line))))
                (parse-turn) #f]))
        ))
    
    (define (turn-time-remaining )
      "Returns the turn time remaining in milliseconds."
      (- (+ turn-start-time (hash-ref params 'turn-time))
         (current-milliseconds)))
    
    #|
    (define (user-interrupt arg)
      (declare (ignore arg))
      (format *debug-io* "~&User interrupt. Aborting...~%")
      (quit))
    |#
    ));;end class definition

;; helper functions


;;; Globals
    
(define *STATE* (new state%))

