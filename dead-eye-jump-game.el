(defvar target-game-buffer "*Target-Game*")
(defvar target-layer-scores '(10 20 30))
(defvar target-size 5)
(defvar current-target-position nil)
(defvar game-score 0)

(defun target-game-init ()
  "Initialize the target shooting game."
  (interactive)
  (setq game-score 0)  ; Reset the game score.
  (switch-to-buffer target-game-buffer)
  (erase-buffer)
  (target-game-mode 1)
  (target-game-draw-target)
  (message "Game initialized. Score: %d" game-score))

(defun target-game-draw-target ()
  "Draw a new target at a random position."
  (let* ((width (window-width))
         (height (window-height))
         (x (random (- width target-size)))
         (y (random (- height target-size)))
         (layer (random (length target-layer-scores))))
    (setq current-target-position (list x y layer))
    (save-excursion
      (goto-char (point-min))
      (forward-line y)
      (forward-char x)
      (dotimes (i target-size)
        (insert (make-string target-size (nth layer '("+" "#" "*"))))
        (insert "\n")
        (forward-line 1)
        (forward-char (- target-size))))))

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

(define-minor-mode target-game-mode
  "A minor mode for playing the target game."
  :lighter " Target-Game"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<left>") 'backward-char)
            (define-key map (kbd "<right>") 'forward-char)
            (define-key map (kbd "<up>") 'previous-line)
            (define-key map (kbd "<down>") 'next-line)
            (define-key map (kbd "SPC") 'target-game-check-hit)
            map)
  (when (not target-game-mode)
    (kill-buffer target-game-buffer)))

(defun target-game-end ()
  "End the target game."
  (interactive)
  (target-game-mode -1)
  (message "Final Score: %d" game-score))

(provide 'target-game)
