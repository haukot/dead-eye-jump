;;; dead-eye-jump.el --- Hit the target, get the points. -*- lexical-binding: t -*-
;;; Code:

;;* Customization
(defvar dead-eye-jump-game-buffer "*Dead-Eye-Jump-Game*")

(defface dead-eye-jump-game-target-face-center '((t :background "#aa00aa"))
  "Face for the center of the target.")
(defface dead-eye-jump-game-target-face-middle '((t :background "yellow"))
  "Face for the middle layer of the target.")
(defface dead-eye-jump-game-target-face-outer '((t :background "#55ee55"))
  "Face for the outer layer of the target.")

(defvar dead-eye-jump-game--layer-scores '(10 30 50))
(defvar dead-eye-jump-game-target-size 7)
(defvar dead-eye-jump-game-target-template
  (list
   (list 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer    'dead-eye-jump-game-target-face-outer)
   (list 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer    'dead-eye-jump-game-target-face-outer)
   (list 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-middle 'dead-eye-jump-game-target-face-middle 'dead-eye-jump-game-target-face-middle 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer)
   (list 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-middle 'dead-eye-jump-game-target-face-center 'dead-eye-jump-game-target-face-middle 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer)
   (list 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-middle 'dead-eye-jump-game-target-face-middle 'dead-eye-jump-game-target-face-middle 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer)
   (list 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer    'dead-eye-jump-game-target-face-outer)
   (list 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer 'dead-eye-jump-game-target-face-outer    'dead-eye-jump-game-target-face-outer)
   ))

(defvar dead-eye-jump-game--score 0
  "Initial score of the game.")

(defvar dead-eye-jump-game--timer nil)
(defvar dead-eye-jump-game-timer-seconds 60
  "Timer for the target game to handle game duration. 0 to endless")

(defvar dead-eye-jump-game-on-start-round-fun nil
  "Function to call when starting a new round.")
;; (setq dead-eye-jump-game-on-start-round-fun 'dead-eye-jump-game-run-jump)
;; (setq dead-eye-jump-game-on-start-round-fun
;;       (lambda ()
;;         (start-process "game warpd" "*warpd output" "warpd" "--hint2" "--click" "1")
;;         ))
;; (setq dead-eye-jump-game-on-start-round-fun nil)

(defun dead-eye-jump-game-run-jump ()
  (setq dead-eye-jump-show-message nil)
  (call-interactively 'dead-eye-jump)
  (dead-eye-jump-game-check-hit)
  )

;;* Internals
(defun dead-eye-jump-game ()
  "Initialize the target shooting game."
  (interactive)
  (setq dead-eye-jump-game--score 0)  ; Reset the game score.
  (let ((buffer (get-buffer-create dead-eye-jump-game-buffer)))
    (switch-to-buffer buffer)
    (dead-eye-jump-game-mode)
    (dead-eye-jump-game-start-round)
    (when dead-eye-jump-game--timer
      (cancel-timer dead-eye-jump-game--timer))
    (when (> dead-eye-jump-game-timer-seconds 0)
      (setq dead-eye-jump-game--timer (run-with-timer dead-eye-jump-game-timer-seconds nil 'dead-eye-jump-game-end)))
    (add-hook 'post-command-hook 'dead-eye-jump-game-check-hit nil t)
    (message "Game initialized. Score: %d" dead-eye-jump-game--score)))

(defun dead-eye-jump-game-start-round ()
  "Reset the target game."
  (interactive)
  (dead-eye-jump-game-fill-buffer)
  (move-to-window-line 0)
  (beginning-of-line)
  (dead-eye-jump-game-draw-target)
  (when dead-eye-jump-game-on-start-round-fun
    (funcall dead-eye-jump-game-on-start-round-fun))
  )

(defun dead-eye-jump-game-end ()
  "End the target game and clean up."
  (interactive)
  (when dead-eye-jump-game--timer
    (cancel-timer dead-eye-jump-game--timer)
    (setq dead-eye-jump-game--timer nil))
  (setq cursor-type nil)
  (when (get-buffer dead-eye-jump-game-buffer)
    (switch-to-buffer dead-eye-jump-game-buffer)
    (dead-eye-jump-game-fill-buffer)
    (move-to-window-line 0)
    (beginning-of-line)
    (insert (format "Game ended. Final Score: %d" dead-eye-jump-game--score))
    (newline)
    (insert-button "Start Game"
                   'action (lambda (x) (dead-eye-jump-game))
                   'follow-link t
                   'help-echo "Click to start a new game.")
    (beginning-of-line)
    )
  (message "Game ended. Final Score: %d" dead-eye-jump-game--score))

(defun dead-eye-jump-game-fill-buffer ()
  "Fill the current buffer with whitespace to match the window height."
  (erase-buffer)  ; Clear the buffer first to ensure it's clean.
  (let ((lines (window-body-height)))  ; Get the number of lines in the current window.
    (dotimes (_ lines)
      (insert (make-string (window-body-width) ?\s) "\n"))))

(defun dead-eye-jump-game-draw-target ()
  "Draw a 5x5 target with different colors at a random position in the buffer."
  (let* ((max-line (- (max 1 (line-number-at-pos (point-max))) dead-eye-jump-game-target-size 1))
         (line (random max-line))
         (max-column (- (max 1 (window-body-width)) dead-eye-jump-game-target-size 2))
         (column (random max-column))
         (target-map dead-eye-jump-game-target-template))
    (save-excursion
      (goto-char (point-min))
      (forward-line line)
      (move-to-column column)
      (let ((start (point)))
        (dotimes (i dead-eye-jump-game-target-size)
          (dotimes (j dead-eye-jump-game-target-size)
            (unless (or (eobp) (>= (+ (point) 1) (line-end-position)))
              (let ((face (nth j (nth i target-map))))
                (delete-char 1)
                (backward-char 1)
                (insert (propertize " " 'face face)))
              ;; Move to the next character, if not at the end of the row and not at the end of the buffer.
              (when (and (< j (1- dead-eye-jump-game-target-size)) (not (eobp)))
                (forward-char 1))))
          ;; Move to the next line if not at the end of the buffer.
          (unless (or (eobp) (= i (1- dead-eye-jump-game-target-size)))
            (forward-line 1)
            (move-to-column column)))
        (overlay-put (make-overlay start (point)) 'target t)))))

(defun dead-eye-jump-game-get-remaining-time ()
  "Get the remaining time in seconds for the target game timer."
  (if dead-eye-jump-game--timer
      (let ((now (current-time))
            (next-activation (timer--time dead-eye-jump-game--timer)))
        (floor (- (time-to-seconds next-activation) (time-to-seconds now))))
    0))

(setq debug-on-error t)

(defun dead-eye-jump-game-check-hit ()
  "Check if the cursor is on the target and update the score."
  (let ((face (if (eobp) nil ;; TODO: does it do anything?
                 (get-char-property (point) 'face)))
        (seconds-left (dead-eye-jump-game-get-remaining-time))
        points)
    ;; Determine points based on the face at the current point
    (setq points (cond ((eq face 'dead-eye-jump-game-target-face-center) (nth 2 dead-eye-jump-game--layer-scores))
                       ((eq face 'dead-eye-jump-game-target-face-middle) (nth 1 dead-eye-jump-game--layer-scores))
                       ((eq face 'dead-eye-jump-game-target-face-outer) (nth 0 dead-eye-jump-game--layer-scores))
                       (t 0)))
    (when (> points 0)
      (setq dead-eye-jump-game--score (+ dead-eye-jump-game--score points))
      (if (> seconds-left 0)
          (message "Hit! You got %d. Score: %d. Seconds left: %s" points dead-eye-jump-game--score seconds-left)
        (message "Hit! You got %d. Score: %d" points dead-eye-jump-game--score))
      (dead-eye-jump-game-start-round)
      (force-mode-line-update))))

(define-derived-mode dead-eye-jump-game-mode fundamental-mode "Dead-Eye-Jump-Game"
  "Major mode for playing the target shooting game."
  (message "Setting up target game mode...")
  (setq-local cursor-type 'box)
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

(provide 'dead-eye-jump-game)
