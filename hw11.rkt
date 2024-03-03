;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To  run  the  game,  press  the "Run"  button at the      ;;
;; top right of DrRacket, and enter (pipe-fantasy GS-TASK-7) ;;
;; to play the demo.                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; TASK 1

(define-struct pipe [top bot left right starting?])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.
;; starting? is #true if the pipe is a starting pipe.

(define SPIPE-L (make-pipe #f #f #t #f #t))
(define SPIPE-R (make-pipe #f #f #f #t #t))
(define SPIPE-T (make-pipe #t #f #f #f #t))
(define SPIPE-B (make-pipe #f #t #f #f #t))

(define PIPE-LR (make-pipe #f #f #t #t #f))
(define PIPE-TB (make-pipe #t #t #f #f #f))
(define PIPE-TL (make-pipe #t #f #t #f #f))
(define PIPE-TR (make-pipe #t #f #f #t #f))
(define PIPE-BL (make-pipe #f #t #t #f #f))
(define PIPE-BR (make-pipe #f #t #f #t #f))
(define PIPE-TBLR (make-pipe #t #t #t #t #f))

(define (pipe-temp p)
  (... (pipe-top p) ...
       (pipe-bot p) ...
       (pipe-left p) ...
       (pipe-right p) ...))

;; TASK 3.2

; A Direction is one of:
; - "up"
; - "down"
; - "left"
; - "right"
; Interpretation: Represents a direction
(define DIR-UP "up")
(define DIR-DOWN "down")
(define DIR-LEFT "left")
(define DIR-RIGHT "right")
(define (dir-temp dir)
  (... (cond [(string=? DIR-UP dir) ...]
             [(string=? DIR-DOWN dir) ...]
             [(string=? DIR-LEFT dir) ...]
             [(string=? DIR-RIGHT dir) ...]) ...))

;; reverse-direction: Direction -> Direction
;; produce the reverse direction of the given direction

(check-expect (reverse-direction DIR-UP) DIR-DOWN)
(check-expect (reverse-direction DIR-DOWN) DIR-UP)
(check-expect (reverse-direction DIR-LEFT) DIR-RIGHT)
(check-expect (reverse-direction DIR-RIGHT) DIR-LEFT)

(define (reverse-direction dir)
  (cond
    [(string=? dir DIR-UP) DIR-DOWN]
    [(string=? dir DIR-DOWN) DIR-UP]
    [(string=? dir DIR-LEFT) DIR-RIGHT]
    [(string=? dir DIR-RIGHT) DIR-LEFT]))

;; TASK 2

(define ALL-PIPES (list PIPE-LR PIPE-TB PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TBLR))

;; TASK 3

;; pipe->image: Pipe Integer Integer Boolean Direction -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length
;; If filled? then draw the pipe with goo.
(define (pipe->image pipe tile-side-length pipe-width filled?)
  (local [(define PIPE-COLOR (if filled? "green" "black"))
          (define CENTER (square pipe-width "solid" PIPE-COLOR))
          (define PIPE-HOR (rectangle (* tile-side-length (/ 2 3)) pipe-width "solid" PIPE-COLOR))
          (define PIPE-VER (rectangle pipe-width (* tile-side-length (/ 2 3)) "solid" PIPE-COLOR))
          (define EMPTY (square (sub1 tile-side-length) 0 "white"))
          (define TILE (if filled? EMPTY (square (sub1 tile-side-length) "solid" "gray")))]
    (overlay/align "right" "middle"
     (if (pipe-right pipe) PIPE-HOR EMPTY)
     (overlay/align "left" "middle"
      (if (pipe-left pipe) PIPE-HOR EMPTY)
      (overlay/align "middle" "top"
       (if (pipe-top pipe) PIPE-VER EMPTY)
       (overlay/align "middle" "bottom"
        (if (pipe-bot pipe) PIPE-VER EMPTY)
        (overlay CENTER TILE)))))))

;;TASK 4

(define-struct pipe-cord [pipe posx posy])
; A Pipe-Cord is a (make-pipe-cord Pipe Integer Integer)
; -where pipe is the type of pipe being referenced
; -posx is the x coordinate, or column of the pipe
; -posy is the y coordinate, or the row of the pipe

(define PC1 (make-pipe-cord PIPE-TB 0 0))
(define PC2 (make-pipe-cord PIPE-TBLR 1 1))
(define PC3 (make-pipe-cord PIPE-BR 4 2))

(define (pc-temp pc)
  (... (pipe-cord-pipe pc) ...
       (pipe-cord-posx pc)...
       (pipe-cord-posy pc)))

;A ListOfPC is either a:
; - '()
; - (cons Pipe-Cord ListOfPC)
;Interpretation: a list of Pipe-Cords that each contain a pipe and its coordinates
(define LPC-1 (cons PC1 '()))
(define LPC-2 (cons PC2 '()))
(define LPC-3 (cons PC2 (cons PC3 LPC-1)))

(define (lpc-temp l)
  (... cond
       [(empty? l) ...]
       [(cons? l) (... (first l) ...
                       (lpc-temp (rest l)) ...)]))

(define-struct grid [size list-pipes])
; A Grid is a (make-grid Integer [ListOf Pipe-Cord])
; -where size is the length of the grid, in terms of rows and columns
; -and list-pipes is a list of Pipe-Cord, where each Pipe has an assigned coordinate

(define GRID-0 (make-grid 0 '()))
(define GRID-1 (make-grid 2 (list PC1 PC2)))
(define GRID-2 (make-grid 7 (list PC1 PC2 PC3)))
(define GRID-3 (make-grid 7 (list (make-pipe-cord SPIPE-R 4 3))))

(define (grid-temp g)
  (... (grid-size g) ...
       (lpc-temp (pc-temp (grid-list-pipes g)))))

;;TASK 5

(define STARTING-GRID (make-grid 7 '()))

;;TASK 6

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.

(check-expect (place-pipe GRID-1 PIPE-TBLR 0 1)
              (make-grid
               (grid-size GRID-1)
               (cons (make-pipe-cord PIPE-TBLR 0 1) (grid-list-pipes GRID-1))))
(check-expect (place-pipe GRID-2 PIPE-TR 3 0)
              (make-grid
               (grid-size GRID-2)
               (cons (make-pipe-cord PIPE-TR 3 0) (grid-list-pipes GRID-2))))
(check-expect (place-pipe GRID-2 PIPE-BR 5 4)
              (make-grid
               (grid-size GRID-2)
               (cons (make-pipe-cord PIPE-BR 5 4) (grid-list-pipes GRID-2))))



(define (place-pipe grid pipe row col)
  (local [(define PC-NEW (make-pipe-cord pipe row col))]
    (make-grid (grid-size grid) (cons PC-NEW (grid-list-pipes grid)))))

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.

(check-expect (pipe-at GRID-1 1 1) PIPE-TBLR)
(check-expect (pipe-at GRID-1 5 5) #f)
(check-expect (pipe-at GRID-0 5 5) #f)
(check-expect (pipe-at GRID-2 4 2) PIPE-BR)

(define (pipe-at grid row col)
  (local [(define (is-pipe-at pipe)
            (and (= row (pipe-cord-posx pipe))
                 (= col (pipe-cord-posy pipe))))
          (define FOUND-PIPE (filter is-pipe-at (grid-list-pipes grid)))]
    (if (or (empty? FOUND-PIPE) (empty? (grid-list-pipes grid))) #f
        (first (map pipe-cord-pipe FOUND-PIPE)))))




;; TASK 3.2

(define-struct goo-flow [lopc dir])
; A GooFlow is a (make-goo-flow [ListOf PipeCord] Direction)
; - lopc: The list of pipes that the goo flowed
; - dir: The direction that the goo is currently flowing
; Interpretation: The goo that flows through the pipe in the game
(define EX-GOO-FLOW-0 (make-goo-flow '() DIR-UP))
(define EX-GOO-FLOW-1 (make-goo-flow LPC-1 DIR-DOWN))
(define EX-GOO-FLOW-2 (make-goo-flow LPC-3 DIR-LEFT))
(define (goo-flow-temp gf)
  (... (lpc-temp (goo-flow-lopc gf))
       ... (dir-temp (goo-flow-dir gf)) ...))




;;TASK 7

; draw-empty-grid: Grid Integer -> Image
; Draws the empty grid
(check-error (draw-empty-grid GRID-0 30))
(check-expect (draw-empty-grid GRID-1 30)
              (above (beside (overlay (square 30 "outline" "black") (square 30 "solid" "white"))
                             (overlay (square 30 "outline" "black") (square 30 "solid" "white")))
                     (beside (overlay (square 30 "outline" "black") (square 30 "solid" "white"))
                             (overlay (square 30 "outline" "black") (square 30 "solid" "white")))))
(check-expect (draw-empty-grid GRID-2 30)
              (local
                [(define GRID-ROW
                   (beside
                    (overlay (square 30 "outline" "black") (square 30 "solid" "white"))
                    (overlay (square 30 "outline" "black") (square 30 "solid" "white"))
                    (overlay (square 30 "outline" "black") (square 30 "solid" "white"))
                    (overlay (square 30 "outline" "black") (square 30 "solid" "white"))
                    (overlay (square 30 "outline" "black") (square 30 "solid" "white"))
                    (overlay (square 30 "outline" "black") (square 30 "solid" "white"))
                    (overlay (square 30 "outline" "black") (square 30 "solid" "white"))))]
                (above 
                 GRID-ROW
                 GRID-ROW
                 GRID-ROW
                 GRID-ROW
                 GRID-ROW
                 GRID-ROW
                 GRID-ROW)))

(define (draw-empty-grid grid tile-side-length)
  (local [(define SINGLE-TILE
            (overlay
             (square tile-side-length "outline" "black")
             (square tile-side-length "solid" "white")))
          (define GRID-ROW (apply beside (make-list (grid-size grid) SINGLE-TILE)))]
    (apply above (make-list (grid-size grid) GRID-ROW))))

;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width)
  (local [; draw-pipe PipeCord PipeCord -> Image
          ; Draw a pipe on the grid
          (define (draw-pipe pc-1 pc-2)
            (place-image (pipe->image (pipe-cord-pipe pc-1) tile-side-length pipe-width false)
                         (+ (* (pipe-cord-posx pc-1) tile-side-length) (/ tile-side-length 2))
                         (+ (* (pipe-cord-posy pc-1) tile-side-length) (/ tile-side-length 2))
                         pc-2))]
    (foldr draw-pipe (draw-empty-grid grid tile-side-length) (grid-list-pipes grid))))


;; TASK 4.2

; add-dirs-on-pipe: [ListOf Direction] (Pipe or #false) -> Pipe
; Takes in a list of directions and a pipe, add openings for the pipe according to the direction. If pipe is #false,
; then make a new empty pipe

(check-expect (add-dirs-on-pipe (list DIR-UP DIR-DOWN) PIPE-LR) PIPE-TBLR)
(check-expect (add-dirs-on-pipe (list DIR-LEFT DIR-RIGHT) PIPE-TB) PIPE-TBLR)
(check-expect (add-dirs-on-pipe '() PIPE-TB) PIPE-TB)
(check-expect (add-dirs-on-pipe '() #f) (make-pipe #f #f #f #f #f))

(define (add-dirs-on-pipe dirs on-pipe)
  (cond [(false? on-pipe) (add-dirs-on-pipe dirs (make-pipe #f #f #f #f #f))]
        [(empty? dirs) on-pipe]
        [(cons? dirs)
         (cond [(string=? (first dirs) DIR-UP)
                (add-dirs-on-pipe (rest dirs) (make-pipe #t (pipe-bot on-pipe) (pipe-left on-pipe) (pipe-right on-pipe) (pipe-starting? on-pipe)))]
               [(string=? (first dirs) DIR-DOWN)
                (add-dirs-on-pipe (rest dirs) (make-pipe (pipe-top on-pipe) #t (pipe-left on-pipe) (pipe-right on-pipe) (pipe-starting? on-pipe)))]
               [(string=? (first dirs) DIR-LEFT)
                (add-dirs-on-pipe (rest dirs) (make-pipe (pipe-top on-pipe) (pipe-bot on-pipe) #t (pipe-right on-pipe) (pipe-starting? on-pipe)))]
               [(string=? (first dirs) DIR-RIGHT)
                (add-dirs-on-pipe (rest dirs) (make-pipe (pipe-top on-pipe) (pipe-bot on-pipe) (pipe-left on-pipe) #t (pipe-starting? on-pipe)))])]))

; placed-pipe-at : GooFlow Number Number -> (Pipe or #false)
; Find the placed pipe at the specific position. Return #false if not foun
(check-expect (placed-pipe-at EX-GOO-FLOW-1 1 1) #f)
(check-expect (placed-pipe-at EX-GOO-FLOW-1 0 0) PIPE-TB)
(check-expect (placed-pipe-at EX-GOO-FLOW-2 4 2) PIPE-BR)
(define (placed-pipe-at gf x y)
  (local [(define TEMP (memf (lambda (p) (and (= x (pipe-cord-posx p)) (= y (pipe-cord-posy p)))) (goo-flow-lopc gf)))]
    (if (false? TEMP)
        #f
        (pipe-cord-pipe (first TEMP)))))


;; grid-goo-propagate : GooFlow Grid -> GooFlow
;; Moves the goo forward by one tile. If the goo is stuck, produces the same goo.

(check-expect (grid-goo-propagate
               (make-goo-flow (list (make-pipe-cord SPIPE-R 1 5))
                              DIR-RIGHT)
               (make-grid 7 (list
                             (make-pipe-cord PIPE-LR 2 5)
                             (make-pipe-cord PIPE-TL 3 5)
                             (make-pipe-cord PIPE-TB 3 4))))
              (make-goo-flow (list
                              (make-pipe-cord PIPE-LR 2 5)
                              (make-pipe-cord SPIPE-R 1 5)) DIR-RIGHT))

(check-expect (grid-goo-propagate
               (make-goo-flow (list
                               (make-pipe-cord PIPE-LR 2 5)
                               (make-pipe-cord SPIPE-R 1 5)) DIR-RIGHT)
               (make-grid 7 (list
                             (make-pipe-cord PIPE-LR 2 5)
                             (make-pipe-cord PIPE-TL 3 5)
                             (make-pipe-cord PIPE-TB 3 4))))
              (make-goo-flow (list
                              (make-pipe-cord PIPE-TL 3 5)
                              (make-pipe-cord PIPE-LR 2 5)
                              (make-pipe-cord SPIPE-R 1 5)) DIR-RIGHT))

(check-expect (grid-goo-propagate
               (make-goo-flow (list
                               (make-pipe-cord PIPE-TL 3 5)
                               (make-pipe-cord PIPE-LR 2 5)
                               (make-pipe-cord SPIPE-R 1 5)) DIR-RIGHT)
               (make-grid 7 (list
                             (make-pipe-cord PIPE-LR 2 5)
                             (make-pipe-cord PIPE-TL 3 5)
                             (make-pipe-cord PIPE-TB 3 4))))
              (make-goo-flow (list
                              (make-pipe-cord PIPE-TB 3 4)
                              (make-pipe-cord PIPE-TL 3 5)
                              (make-pipe-cord PIPE-LR 2 5)
                              (make-pipe-cord SPIPE-R 1 5)) DIR-UP))

(check-expect (grid-goo-propagate
               (make-goo-flow (list
                               (make-pipe-cord PIPE-TB 3 4)
                               (make-pipe-cord PIPE-TL 3 5)
                               (make-pipe-cord PIPE-LR 2 5)
                               (make-pipe-cord SPIPE-R 1 5)) DIR-UP)
               (make-grid 7 (list
                             (make-pipe-cord PIPE-LR 2 5)
                             (make-pipe-cord PIPE-TL 3 5)
                             (make-pipe-cord PIPE-TB 3 4))))
              (make-goo-flow (list
                              (make-pipe-cord PIPE-TB 3 4)
                              (make-pipe-cord PIPE-TL 3 5)
                              (make-pipe-cord PIPE-LR 2 5)
                              (make-pipe-cord SPIPE-R 1 5)) DIR-UP))

(check-expect (grid-goo-propagate
               (make-goo-flow (list
                               (make-pipe-cord PIPE-TB 3 4)
                               (make-pipe-cord PIPE-TL 3 5)
                               (make-pipe-cord PIPE-LR 2 5)
                               (make-pipe-cord SPIPE-R 1 5)) DIR-UP)
               (make-grid 7 (list
                             (make-pipe-cord PIPE-LR 2 5)
                             (make-pipe-cord PIPE-LR 3 3)
                             (make-pipe-cord PIPE-TL 3 5)
                             (make-pipe-cord PIPE-TB 3 4))))
              (make-goo-flow (list
                              (make-pipe-cord PIPE-TB 3 4)
                              (make-pipe-cord PIPE-TL 3 5)
                              (make-pipe-cord PIPE-LR 2 5)
                              (make-pipe-cord SPIPE-R 1 5)) DIR-UP))

(define (grid-goo-propagate goo-flow grid) 
  (local
    [
     (define DIRECTION (goo-flow-dir goo-flow))
     (define CURR-PIPE (pipe-cord-pipe (first (goo-flow-lopc goo-flow))))
     
     ;; can-move? Pipe Direction -> GooFlow
     ;; checks if the goo can propogate, then produces either same or new GooFlow
     (define (can-move? CURR-PIPE direction)
       (local
         [
          (define X (pipe-cord-posx (first (goo-flow-lopc goo-flow))))
          (define Y (pipe-cord-posy (first (goo-flow-lopc goo-flow))))

          ;;create-gf : [Number -> Number] String Direction
          ;;creates a GooFlow based on appropriate coordinate value that changes and direction
          (define (create-gf func val dir) 
            (cond [(string=? "X" val)
                   (make-goo-flow (cons
                                   (make-pipe-cord
                                    (local [(define TO-PIPE (pipe-at grid (func X) Y))]
                                      (if (and (pipe-top TO-PIPE) (pipe-bot TO-PIPE) (pipe-left TO-PIPE) (pipe-right TO-PIPE))
                                          (add-dirs-on-pipe (list (reverse-direction dir) dir) (placed-pipe-at goo-flow (func X) Y))
                                          TO-PIPE))
                                    (func X)
                                    Y) (goo-flow-lopc goo-flow)) dir)]
                  [(string=? "Y" val)
                   (make-goo-flow (cons
                                   (make-pipe-cord
                                    (local [(define TO-PIPE (pipe-at grid X (func Y)))]
                                      (if (and (pipe-top TO-PIPE) (pipe-bot TO-PIPE) (pipe-left TO-PIPE) (pipe-right TO-PIPE))
                                          (add-dirs-on-pipe (list (reverse-direction dir) dir) (placed-pipe-at goo-flow X (func Y)))
                                          TO-PIPE))
                                    X
                                    (func Y)) (goo-flow-lopc goo-flow)) dir)]))

          ;;check-next : Pipe Direction -> GooFlow
          ;;checks if a different direction should be tried, or if same GooFlow should be returned
          (define (check-next cp dir)
            (if (or (string=? (next-direction cp dir) dir) (string=? (reverse-direction (next-direction cp dir)) DIRECTION))
                goo-flow
                (can-move? cp (next-direction cp dir))))]
            
    
         (cond
           [(string=? direction DIR-UP)
            (if
             (and (pipe-top CURR-PIPE)
                  (if (pipe? (pipe-at grid X (sub1 Y)))
                      (pipe-bot (pipe-at grid X (sub1 Y)))
                      #f)
                  (>= Y 1))
             (create-gf sub1 "Y" DIR-UP)             
             (check-next CURR-PIPE direction))]
           [(string=? direction DIR-DOWN)
            (if
             (and (pipe-bot CURR-PIPE)
                  (if (pipe? (pipe-at grid X (add1 Y)))
                      (pipe-top (pipe-at grid X (add1 Y)))
                      #f)
                  (<= Y (- (grid-size grid) 2)))
             (create-gf add1 "Y" DIR-DOWN)
             (check-next CURR-PIPE direction))]
           [(string=? direction DIR-LEFT)
            (if
             (and (pipe-left CURR-PIPE)
                  (if (pipe? (pipe-at grid (sub1 X) Y))
                      (pipe-right (pipe-at grid (sub1 X) Y))
                      #f)
                  (>= X 1))
             (create-gf sub1 "X" DIR-LEFT)
             (check-next CURR-PIPE direction))]
           [(string=? direction DIR-RIGHT)
            (if
             (and (pipe-right CURR-PIPE)
                  (if (pipe? (pipe-at grid (add1 X) Y))
                      (pipe-left (pipe-at grid (add1 X) Y))
                      #f)
                  (<= X (- (grid-size grid) 2)))
             (create-gf add1 "X" DIR-RIGHT)
             (check-next CURR-PIPE direction))])))

     ;;next-direction : Pipe Direction -> Direction
     ;;if a pipe with multiple openings is reached, changes direction to another opening
     (define (next-direction cp dir)
       (cond
         [(and (string=? dir DIR-UP) (pipe-right cp)) DIR-RIGHT]
         [(and (string=? dir DIR-UP) (pipe-left cp)) DIR-LEFT]
         [(and (string=? dir DIR-DOWN) (pipe-right cp)) DIR-RIGHT]
         [(and (string=? dir DIR-DOWN) (pipe-left cp)) DIR-LEFT]
         [(and (string=? dir DIR-LEFT) (pipe-top cp)) DIR-UP]
         [(and (string=? dir DIR-LEFT) (pipe-bot cp)) DIR-DOWN]
         [(and (string=? dir DIR-RIGHT) (pipe-top cp)) DIR-UP]
         [(and (string=? dir DIR-RIGHT) (pipe-bot cp)) DIR-DOWN]
         [else DIRECTION]))]
    
    (can-move? CURR-PIPE DIRECTION)))



;;TASK 8 / 5.2

(define-struct game-state [grid start-pipe goo-flow incoming-pipes num-pr time-until-prop])
; A GameState is (make-game-state Grid Pipe GooFlow [ListOf Pipe])
; - A grid is the grid of the game
; - incoming-pipes represents that the list of pipes that the user will place
; Interpretation: The state of the game.

(define GS-0 (make-game-state GRID-1 SPIPE-R
                              (make-goo-flow (list (make-pipe-cord SPIPE-R 0 0)) DIR-RIGHT) '() 0 140))
(define GS-1 (make-game-state GRID-1 SPIPE-T EX-GOO-FLOW-0 (list PIPE-LR PIPE-BR) 0 140))
(define GS-2 (make-game-state GRID-2 SPIPE-T EX-GOO-FLOW-0 (list PIPE-LR PIPE-BR PIPE-TBLR PIPE-BR) 0 140))
(define GS-3 (make-game-state GRID-0 null null '() 0 140))
(define GS-4 (make-game-state GRID-3 SPIPE-T EX-GOO-FLOW-0 (list PIPE-LR PIPE-BR PIPE-TBLR PIPE-BR) 0 140))
(define GS-5 (make-game-state GRID-3 SPIPE-T EX-GOO-FLOW-1 (list PIPE-LR PIPE-BR PIPE-TBLR PIPE-BR) 0 0))

(define (game-state-temp gs)
  (... (grid-temp (game-state-grid gs)) ...
       (pipe-temp (game-state-start-pipe gs)) ...
       (goo-flow-temp (game-state-goo-flow gs)) ...
       (list-temp (game-state-incoming-pipes gs)) ...
       (game-state-num-pr gs) ...
       (game-state-time-until-prop gs) ...))

(define TILE-SIDE-LENGTH 64)
(define PIPE-WIDTH 16)

;;TASK 6.2

;; gamestate-init : Integer Integer Integer Direction [ListOf Pipe] -> GameState
;; initializes a gamestate based on the following given information:
;; -the grid dimension
;; -the x and y grid coordinates of the starting pipe
;; -the direction of the starting pipe
;; -the incoming pipes list


(check-expect (gamestate-init 7 2 3 DIR-DOWN '())
              (make-game-state
               (place-pipe (make-grid 7 '()) SPIPE-B 2 3)
               SPIPE-B
               (make-goo-flow
                (list (make-pipe-cord SPIPE-B 2 3))
                DIR-DOWN)
               '() 0 140))

(check-expect (gamestate-init 7 3 0 DIR-DOWN (list PIPE-LR PIPE-BR))
              (make-game-state
               (place-pipe (make-grid 7 '()) SPIPE-B 3 0)
               SPIPE-B
               (make-goo-flow
                (list (make-pipe-cord SPIPE-B 3 0))
                DIR-DOWN)
               (list PIPE-LR PIPE-BR) 0 140))

(check-expect (gamestate-init 7 3 2 DIR-RIGHT (list PIPE-LR PIPE-BR PIPE-TBLR PIPE-BR))
              (make-game-state
               (place-pipe (make-grid 7 '()) SPIPE-R 3 2)
               SPIPE-R
               (make-goo-flow
                (list (make-pipe-cord SPIPE-R 3 2))
                DIR-RIGHT)
               (list PIPE-LR PIPE-BR PIPE-TBLR PIPE-BR) 0 140))

(define (gamestate-init size x y dir incoming-lop)
  (local
    [     
     (define START-PIPE (cond [(string=? dir DIR-UP) SPIPE-T]
                              [(string=? dir DIR-DOWN) SPIPE-B]
                              [(string=? dir DIR-LEFT) SPIPE-L]
                              [(string=? dir DIR-RIGHT) SPIPE-R]))
     (define GRID (make-grid size '()))]

    (make-game-state
     (place-pipe GRID START-PIPE x y)
     START-PIPE
     (make-goo-flow
      (list (make-pipe-cord START-PIPE x y))
      dir)
     incoming-lop 0 140)))



;;TASK 9

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(check-expect (place-pipe-on-click GS-0 3 3 "button-down") GS-0)
(check-expect (place-pipe-on-click GS-1 10 23 "button-up") GS-1)
(check-expect (place-pipe-on-click GS-2 65 23 "button-down")
              (make-game-state
               (make-grid 7 (list (make-pipe-cord PIPE-LR 1 0) PC1 PC2 PC3))
               SPIPE-T
               EX-GOO-FLOW-0
               (list PIPE-BR PIPE-TBLR PIPE-BR) 0 140))

(define (place-pipe-on-click gs x y event)
  (local [(define MOUSE-CLICK-TILE-X (floor (/ x TILE-SIDE-LENGTH)))
          (define MOUSE-CLICK-TILE-Y (floor (/ y TILE-SIDE-LENGTH)))
          (define CLICKED-PIPE (pipe-at (game-state-grid gs) MOUSE-CLICK-TILE-X MOUSE-CLICK-TILE-Y))]
    (if (and (mouse=? event "button-down")
             (< x (* (grid-size (game-state-grid gs)) TILE-SIDE-LENGTH))
             (or (false? CLICKED-PIPE) (not (pipe-starting? CLICKED-PIPE)))
             (local [; Number Number -> Boolean
                     ; Check if the pipe at x y is filled.
                     (define (is-pipe-at-filled? x y)
                       (ormap (lambda (p) (and (= (pipe-cord-posx p) x) (= (pipe-cord-posy p) y)))
                              (goo-flow-lopc (game-state-goo-flow gs))))]
               (not (is-pipe-at-filled? MOUSE-CLICK-TILE-X MOUSE-CLICK-TILE-Y))))
        (cond [(empty? (game-state-incoming-pipes gs))
               gs]
              [(cons? (game-state-incoming-pipes gs))
               (make-game-state (place-pipe (game-state-grid gs)
                                            (first (game-state-incoming-pipes gs))
                                            MOUSE-CLICK-TILE-X
                                            MOUSE-CLICK-TILE-Y)
                                (game-state-start-pipe gs)
                                (game-state-goo-flow gs)
                                (rest (game-state-incoming-pipes gs))
                                (if (false? (pipe-at (game-state-grid gs) MOUSE-CLICK-TILE-X MOUSE-CLICK-TILE-Y))
                                    (game-state-num-pr gs)
                                    (add1 (game-state-num-pr gs)))
                                (game-state-time-until-prop gs))])
        gs)))

;;TASK 10

; draw-func: GameState -> Image
; The draw function for big bang.
(check-expect (draw-func GS-1)
              (beside/align "bottom"
                            (grid->image GRID-1 TILE-SIDE-LENGTH PIPE-WIDTH)
                            (above (square TILE-SIDE-LENGTH 0 "white")
                                   (square TILE-SIDE-LENGTH 0 "white")
                                   (pipe->image PIPE-BR TILE-SIDE-LENGTH PIPE-WIDTH false)
                                   (pipe->image PIPE-LR TILE-SIDE-LENGTH PIPE-WIDTH false)
                                   (text "0" 24 "black"))))
(check-expect (draw-func GS-1)
              (beside/align "bottom"
                            (grid->image GRID-1 TILE-SIDE-LENGTH PIPE-WIDTH)
                            (above (square TILE-SIDE-LENGTH 0 "white")
                                   (square TILE-SIDE-LENGTH 0 "white")
                                   (pipe->image PIPE-BR TILE-SIDE-LENGTH PIPE-WIDTH false)
                                   (pipe->image PIPE-LR TILE-SIDE-LENGTH PIPE-WIDTH false)
                                   (text "0" 24 "black"))))
(check-expect (draw-func GS-2)
              (beside/align "bottom"
                            (grid->image GRID-2 TILE-SIDE-LENGTH PIPE-WIDTH)
                            (above (pipe->image PIPE-BR TILE-SIDE-LENGTH PIPE-WIDTH false)
                                   (pipe->image PIPE-TBLR TILE-SIDE-LENGTH PIPE-WIDTH false)
                                   (pipe->image PIPE-BR TILE-SIDE-LENGTH PIPE-WIDTH false)
                                   (pipe->image PIPE-LR TILE-SIDE-LENGTH PIPE-WIDTH false)
                                   (text "0" 24 "black"))))

(define (draw-func gs)
  (local [(define (draw-pipe pipe)
            (pipe->image pipe TILE-SIDE-LENGTH PIPE-WIDTH false))
          (define (draw-pipe-coord pc-1 pc-2)
            (place-image (pipe->image (pipe-cord-pipe pc-1)
                                      TILE-SIDE-LENGTH
                                      PIPE-WIDTH
                                      true)
                         (+ (* (pipe-cord-posx pc-1) TILE-SIDE-LENGTH) (/ TILE-SIDE-LENGTH 2))
                         (+ (* (pipe-cord-posy pc-1) TILE-SIDE-LENGTH) (/ TILE-SIDE-LENGTH 2))
                         pc-2))]
    (beside/align "bottom"
                  (foldr draw-pipe-coord
                         (grid->image (game-state-grid gs) TILE-SIDE-LENGTH PIPE-WIDTH)
                         (goo-flow-lopc (game-state-goo-flow gs)))
                  (above
                   (if (and (cons? (game-state-incoming-pipes gs))
                            (cons? (rest (game-state-incoming-pipes gs)))
                            (cons? (rest (rest (game-state-incoming-pipes gs))))
                            (cons? (rest (rest (rest (game-state-incoming-pipes gs))))))
                       (draw-pipe (fourth (game-state-incoming-pipes gs)))
                       (square TILE-SIDE-LENGTH 0 "white"))
                   (if (and (cons? (game-state-incoming-pipes gs))
                            (cons? (rest (game-state-incoming-pipes gs)))
                            (cons? (rest (rest (game-state-incoming-pipes gs)))))
                       (draw-pipe (third (game-state-incoming-pipes gs)))
                       (square TILE-SIDE-LENGTH 0 "white"))
                   (if (and (cons? (game-state-incoming-pipes gs))
                            (cons? (rest (game-state-incoming-pipes gs))))
                       (draw-pipe (second (game-state-incoming-pipes gs)))
                       (square TILE-SIDE-LENGTH 0 "white"))
                   (if (cons? (game-state-incoming-pipes gs))
                       (draw-pipe (first (game-state-incoming-pipes gs)))
                       (square TILE-SIDE-LENGTH 0 "white"))
                   (text (number->string (get-score gs)) 24 "black")))))

;;TASK 6

;; tick-propagate : GameState -> Gamestate
;;automates the goo propagation based on ticks, or time

;;TODO check expect
(check-expect (tick-propagate GS-0)
              (make-game-state (make-grid 2 (list (make-pipe-cord PIPE-TB 0 0) (make-pipe-cord PIPE-TBLR 1 1)))
                               (make-pipe #false #false #false #true #true)
                               (make-goo-flow
                                (list (make-pipe-cord (make-pipe #false #false #false #true #true) 0 0)) "right") '() 0 139))
(check-expect (tick-propagate GS-2)
              (make-game-state
               (game-state-grid GS-2)
               (game-state-start-pipe GS-2)
               (game-state-goo-flow GS-2)
               (game-state-incoming-pipes GS-2)
               (game-state-num-pr GS-2)
        139))
(check-expect (tick-propagate GS-5)
              (make-game-state
               (make-grid 7 (list (make-pipe-cord (make-pipe #false #false #false #true #true) 4 3)))
               (make-pipe #true #false #false #false #true)
               (make-goo-flow (list (make-pipe-cord PIPE-TB 0 0)) "down")
               (list PIPE-LR PIPE-BR PIPE-TBLR PIPE-BR) 0 0))

(define (tick-propagate gs)
  (local
    [;;gf=? : GooFlow -> Boolean
     ;;determines true if goo does not propagate between 2 GameStates, false if it does propagate
     (define (gf=? gf1 gf2)
       (and (= (pipe-cord-posx (first (goo-flow-lopc gf1)))
               (pipe-cord-posx (first (goo-flow-lopc gf2))))            
            (= (pipe-cord-posy (first (goo-flow-lopc gf1)))
               (pipe-cord-posy (first (goo-flow-lopc gf2))))))]
         
    (cond
      [(> (game-state-time-until-prop gs) 0)
       (make-game-state
        (game-state-grid gs)
        (game-state-start-pipe gs)
        (game-state-goo-flow gs)
        (game-state-incoming-pipes gs)
        (game-state-num-pr gs)
        (sub1 (game-state-time-until-prop gs)))]
      [(= (game-state-time-until-prop gs) 0)
       (if (gf=? (grid-goo-propagate (game-state-goo-flow gs) (game-state-grid gs)) (game-state-goo-flow gs))
           gs
           (make-game-state
            (game-state-grid gs)
            (game-state-start-pipe gs)
            (grid-goo-propagate (game-state-goo-flow gs) (game-state-grid gs))
            (game-state-incoming-pipes gs)
            (game-state-num-pr gs)
            28))])))

    

;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    [on-tick tick-propagate]
    [on-draw draw-func]
    [on-mouse place-pipe-on-click]))

;;TASK 2.3

;;get-score: GameState -> Integer
;;computes the score of the game
(check-expect (get-score GS-0) 50)
(define (get-score gs)
  (* 50 (- (length (goo-flow-lopc (game-state-goo-flow gs))) (game-state-num-pr gs))))




;; Task 9
(define GS-EX-0 (gamestate-init 7 3 2 DIR-RIGHT (list PIPE-LR PIPE-TBLR PIPE-TBLR PIPE-BR)))
(define GS-EX-1 (gamestate-init 5 1 2 DIR-RIGHT
                                (list PIPE-LR PIPE-BL PIPE-BR PIPE-TBLR PIPE-TR PIPE-LR PIPE-TL)))
(define GS-EX-2 (gamestate-init 2 0 0 DIR-RIGHT (list PIPE-BR PIPE-TBLR PIPE-BR)))

(define GS-TASK-7 (make-game-state
                   (place-pipe (make-grid 6
                                          (list
                                           (make-pipe-cord PIPE-BL 2 1)
                                           (make-pipe-cord PIPE-TBLR 2 2)
                                           (make-pipe-cord PIPE-TB 2 3)
                                           (make-pipe-cord PIPE-TL 2 4)
                                           (make-pipe-cord PIPE-TR 1 4)
                                           (make-pipe-cord PIPE-TBLR 1 3)
                                           (make-pipe-cord PIPE-BR 1 2)
                                           (make-pipe-cord PIPE-LR 3 2)
                                           (make-pipe-cord PIPE-TL 4 2)))
                               SPIPE-R 1 1)
                   SPIPE-R
                   (make-goo-flow
                    (list (make-pipe-cord SPIPE-R 1 1))
                    DIR-RIGHT)
                   (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL) 0 140))

;(pipe-fantasy GS-TASK-7)