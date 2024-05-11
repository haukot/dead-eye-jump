;;; -*- lexical-binding: t -*-

(defvar target-game-buffer "*Target-Game*")

(defface target-face-center
  '((t :background "#aa00aa"))
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

(defvar current-target-position nil
  "Store the current target's position as (column line layer).")

(defvar game-score 0
  "Initial score of the game.")

(defvar target-game--timer nil)
(defvar target-game-timer-seconds 10
  "Timer for the target game to handle game duration. 0 to endless")

(defun target-game ()
  "Initialize the target shooting game."
  (interactive)
  (setq game-score 0)  ; Reset the game score.
  (let ((buffer (get-buffer-create target-game-buffer)))
    (switch-to-buffer buffer)
    (target-game-mode)
    (target-game-start-round)
    (when target-game--timer
      (cancel-timer target-game--timer))
    (when (> target-game--timer-seconds 0)
      (setq target-game--timer (run-with-timer target-game-timer-seconds nil 'target-game-end)))
    (message "Game initialized. Score: %d" game-score)))

;;; TODO not used yet
(defun target-game-disable ()
  "Disable the target game mode and clean up."
  (interactive)
  (remove-hook 'post-command-hook 'target-game-check-hit t)
  (kill-buffer target-game-buffer))

(defun target-game-start-round ()
  "Reset the target game."
  (interactive)
  (target-game-fill-buffer)
  (move-to-window-line 0)
  (beginning-of-line)
  (target-game-draw-target)
  ;; TODO: почему-то после второго повторения не выбирает мишень
  ;; TODO: why cant use default params?
  ;; (dead-eye-jump 0 0 (frame-pixel-width) (frame-pixel-height) dead-eye-jump-repeats)
  )

(defun target-game-end ()
  "End the target game and clean up."
  (interactive)
  (when target-game--timer
    (cancel-timer target-game--timer)
    (setq target-game--timer nil))
  (setq cursor-type nil)
  (when (get-buffer target-game-buffer)
    (switch-to-buffer target-game-buffer)
    (target-game-fill-buffer)
    (move-to-window-line 0)
    (beginning-of-line)
    (insert (format "Game ended. Final Score: %d" game-score)))
  ;; (kill-buffer target-game-buffer))
  (message "Game ended. Final Score: %d" game-score))
;; TODO: remove hook
;; (remove-hook 'post-command-hook 'target-game-check-hit t))

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
         (max-column (- (max 1 (window-body-width)) target-size 1))
         (column (random max-column))
         (target-map target-template))
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

(defun target-game-get-remaining-time ()
  "Get the remaining time in seconds for the target game timer."
  (if target-game--timer
      (let ((now (current-time))
            (next-activation (timer--time target-game--timer)))
        (floor (- (time-to-seconds next-activation) (time-to-seconds now))))
    0))

(defun target-game-check-hit ()
  "Check if the cursor is on the target and update the score."
  (let ((face (get-char-property (point) 'face))
        (seconds-left (target-game-get-remaining-time))
        points)
    ;; Determine points based on the face at the current point
    (setq points (cond ((eq face 'target-face-center) 50)
                       ((eq face 'target-face-middle) 30)
                       ((eq face 'target-face-outer) 10)
                       (t 0)))
    (when (> points 0)
      (setq game-score (+ game-score points))
      (if (> seconds-left 0)
          (message "Hit! You got %d. Score: %d. Seconds left: %s" points game-score seconds-left)
        (message "Hit! You got %d. Score: %d" points game-score))
      (target-game-start-round)
      (force-mode-line-update))))

(define-derived-mode target-game-mode fundamental-mode "Target-Game"
  "Major mode for playing the target shooting game."
  :lighter " Target-Game"
  ;; (if target-game-mode
  ;;     (progn
        (message "Setting up target game mode...")
        (setq-local cursor-type 'box)
        (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
        (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
        (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
        (add-hook 'post-command-hook 'target-game-check-hit nil t)
        )

;; (provide 'target-game)
