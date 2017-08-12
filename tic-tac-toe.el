;;; This is a emacs simple version of tic-tac-toe(ttt)
;;; ttt board
;;; 0 | 1 | 2
;;; 3 | 4 | 5
;;; 6 | 7 | 8

;;; start game
(defun tic-tac-toc-start ()
  "Start to play tic-tac-toc."
  (interactive)
  (switch-to-buffer "tic-tac-toe")
  (tic-tac-toe-mode)
  (initialize-board)
  (print-board))

;;; tic-tac-toe mode map
(define-derived-mode tic-tac-toe-mode special-mode "tic-tac-toe"
  "This is tic-tac-toe mode."
  (define-key tic-tac-toe-mode-map (kbd "q") 'close-tic-tac-toe)
  (define-key tic-tac-toe-mode-map (kbd "SPC") 'mark-board))

;;; close buffer
(defun close-tic-tac-toe ()
  "Close tic-tac-toe buffer."
  (interactive)
  (kill-buffer "tic-tac-toe"))

(defconst ttt-size
  3
  "The size of ttt board.")

(defvar ttt-board
  nil
  "A vector that stores ttt board value.")

(defun initialize-board ()
  "Initialize board status."
  (setq ttt-board (make-vector (* ttt-size ttt-size) ?\.)))

(defvar current-player
  ?\X
  "Current player.")

(defconst winning-situation
  [
   [0 1 2] [3 4 5] [6 7 8]
   [0 3 6] [1 4 7] [2 5 8]
   [0 4 8] [2 4 6]
   ])

(defun same-value-by-row (vector)
  "Check if all values are same on board by VECTOR."
  (let ((values (mapcar (lambda (x) (elt ttt-board x)) vector)))
    (and (or (char-equal (elt values 0) ?\X)
             (char-equal (elt values 0) ?\O))
         (and (char-equal (elt values 0) (elt values 1))
              (char-equal (elt values 1) (elt values 2))))
    )
  )

(defun check-winner ()
  "Check if anyone has won the game."
  (or (same-value-by-row (elt winning-situation 0))
      (same-value-by-row (elt winning-situation 1))
      (same-value-by-row (elt winning-situation 2))
      (same-value-by-row (elt winning-situation 3))
      (same-value-by-row (elt winning-situation 4))
      (same-value-by-row (elt winning-situation 5))
      (same-value-by-row (elt winning-situation 6))
      (same-value-by-row (elt winning-situation 7))))

(defun switch-player ()
  "Switch player whenver one plays."
  (set 'current-player
       (if (char-equal current-player ?\X)
           ?\O
         ?\X)
       ))

(defun print-board ()
  "Print tic-tac-toe board."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row ttt-size)
      (dotimes (column ttt-size)
        (insert
         (elt ttt-board
              (+ column
                 (* row ttt-size)))))
      (insert "\n"))))

(defun mark-board ()
  "Mark ttt board depends on current user."
  (interactive)
  (aset ttt-board
        (+
         (* ttt-size (1- (line-number-at-pos)))
         (current-column))
        current-player)
  (when (check-winner)
    (message "Player %s has won the game." current-player)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  (switch-player)
  (print-board))
