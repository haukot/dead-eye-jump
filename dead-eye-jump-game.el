;;; -*- lexical-binding: t -*-

(defvar target-game-buffer "*Target-Game*")

(defface target-face-center
  '((t :background "red"))
  "Face for the center of the target.")
(defface target-face-middle
  '((t :background "yellow"))
  "Face for the middle layer of the target.")
(defface target-face-outer
  '((t :background "green"))
  "Face for the outer layer of the target.")

(defvar target-layer-scores '(10 30 50))
(defvar target-size 7)
(defvar target-template
  (list
   (list 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer    'target-face-outer)
   (list 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer    'target-face-outer)
   (list 'target-face-outer 'target-face-outer 'target-face-middle 'target-face-middle 'target-face-middle 'target-face-outer 'target-face-outer)
   (list 'target-face-outer 'target-face-outer 'target-face-middle 'target-face-center 'target-face-middle 'target-face-outer 'target-face-outer)
   (list 'target-face-outer 'target-face-outer 'target-face-middle 'target-face-middle 'target-face-middle 'target-face-outer 'target-face-outer)
   (list 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer    'target-face-outer)
   (list 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer 'target-face-outer    'target-face-outer)
   ))
(defvar current-target-position nil)
(defvar game-score 0
  "Initial score of the game.")

(defun target-game ()
  "Initialize the target shooting game."
  (interactive)
  (setq game-score 0)  ; Reset the game score.
  (let ((buffer (get-buffer-create target-game-buffer)))
    (switch-to-buffer buffer)
    (target-game-mode)
    (traget-game-start-round)
    (message "Game initialized. Score: %d" game-score)))

(defun traget-game-start-round ()
  "Reset the target game."
  (interactive)
  (target-game-fill-buffer)
  (move-to-window-line 0)
  (beginning-of-line)
  (target-game-draw-target))

(defun target-game-end ()
  "End the target game and clean up."
  (interactive)
  (when (get-buffer target-game-buffer)
    (kill-buffer target-game-buffer))
  (message "Game ended. Final Score: %d" game-score))

(define-derived-mode target-game-mode fundamental-mode "Target-Game"
  "Major mode for playing the target shooting game."
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (setq-local cursor-type 'box))

(defun target-game-fill-buffer ()
  "Fill the current buffer with whitespace to match the window height."
  (erase-buffer)  ; Clear the buffer first to ensure it's clean.
  (let ((lines (window-body-height)))  ; Get the number of lines in the current window.
    (dotimes (_ lines)
      (insert (make-string (- (window-body-width) 10) ?\s) "\n"))))

(defun target-game-draw-target ()
  "Draw a 5x5 target with different colors at a random position in the buffer."
  (let* ((max-line (- (max 1 (line-number-at-pos (point-max))) target-size))
         (line (random max-line))
         (max-column (- (max 1 (window-body-width)) target-size) 1)
         (column (random max-column))
         (target-map (target-template))
    (save-excursion
      (goto-char (point-min))
      (forward-line line)  ; Move down to the random line.
      (move-to-column column)  ; Move to the random column.
      (let ((start (point)))
        (dotimes (i target-size)
          (dotimes (j target-size)
            (let ((face (nth j (nth i target-map))))
              (delete-char 1)
              (backward-char 1)
              (insert (propertize " " 'face face)))
            ;; Move to the next character, if not at the end of the row.
            (when (< j (1- target-size))
              (forward-char 1)))
          ;; After finishing a row, move to the start of the next line.
          (forward-line 1)
          (move-to-column column))
        (overlay-put (make-overlay start (point)) 'target t)))))

(defun target-game-check-hit ()
  "Check if the cursor is within the target and update the score."
  (interactive)
  (let ((x (car current-target-position))
        (y (cadr current-target-position))
        (layer (caddr current-target-position))
        (px (current-column))
        (py (line-number-at-pos (point) t)))
    (when (and (>= px x) (< px (+ x target-size))
               (>= py y) (< py (+ y target-size)))
      (setq game-score (+ game-score (nth layer target-layer-scores)))
      (message "Hit! Score: %d" game-score)
      (erase-buffer)
      (target-game-draw-target))))

;; (provide 'target-game)
