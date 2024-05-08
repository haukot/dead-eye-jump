;; (defun jump-to-part (key)
;;   "Jump to a character in the center of a part of the window, divided into 16 parts."
;;   (interactive "cPress key (one of qdrwbashtfup:neoi'): ")
;;   ;; (let* ((window-height (window-height))
;;   ;;        (window-width (window-body-width))
;;   ;;        (rows-per-part (max 1 (/ window-height 4)))
;;   ;;        (cols-per-part (max 1 (/ window-width 4)))
;;   ;;        line col part-index)
;;   (let* ((key (read-char "Press key (one of qdrwbashtfup:neoi'): "))
;;          (window (selected-window))
;;          (window-height (window-height window))
;;          (window-width (window-body-width window))
;;          (rows-per-part (max 1 (/ window-height 4)))
;;          (cols-per-part (max 1 (/ window-width 4)))
;;          part-index line col)

;;     ;; Mapping the keys to parts (0-indexed)
;;     (setq part-index (cl-case key
;;                        (?q 0)   (?d 1)   (?r 2)   (?w 3)
;;                        (?a 4)   (?s 5)   (?h 6)   (?t 7)
;;                        (?f 8)   (?u 9)   (?p 10)  (?: 11)
;;                        (?n 12)  (?e 13)  (?o 14)  (?i 15)
;;                        (otherwise (error "Invalid key! Use one of qdrwbashtfup:neoi'"))))
;;     ;; Calculate line and column based on part index
;;     (setq line (+ (window-start-line window) (* rows-per-part (/ part-index 4))))
;;     (setq col (+ 1 (* cols-per-part (/ part-index 4))))

;;     ;; Move cursor to the calculated position
;;     (select-window window)
;;     (goto-line line)
;;     (move-to-column col)))

;; (defun window-start-line (window)
;;   "Return the line number at the top of WINDOW."
;;   (save-excursion
;;     (goto-char (window-start window))
;;     (line-number-at-pos)))

;;     ;; Calculate line and column based on part index
;;     (setq line (+ 1 (* rows-per-part (/ part-index 4))))
;;     (setq col (+ 1 (* cols-per-part (/ part-index 4))))

;;     ;; Move cursor to the calculated position
;;     (goto-line line)
;;     (move-to-column col)))

;; ;; (defun jump-to-part-of-frame ()
;; ;;   "Jump to a part of the frame as divided into a 4x4 grid based on key pressed."
;; ;;   (interactive)
;; ;;   (let* ((key (read-char "Press one of the keys qdrwashtfup:neoi': "))
;; ;;          (windows (window-list))
;; ;;          (frame-width (frame-width))
;; ;;          (frame-height (frame-total-lines))
;; ;;          (cols-per-part (/ frame-width 4))
;; ;;          (rows-per-part (/ frame-height 4))
;; ;;          window target-row target-col window-edges)

;; ;;     ;; Mapping keys to parts (0-indexed)
;; ;;     (let ((index (cl-case key
;; ;;                    (?q 0)   (?d 1)   (?r 2)   (?w 3)
;; ;;                    (?a 4)   (?s 5)   (?h 6)   (?t 7)
;; ;;                    (?f 8)   (?u 9)   (?p 10)  (?: 11)
;; ;;                    (?n 12)  (?e 13)  (?o 14)  (?i 15)
;; ;;                    (t (error "Invalid key! Use one of qdrwashtfup:neoi'")))))
;; ;;       (setq target-row (* rows-per-part (/ index 4))
;; ;;             target-col (* cols-per-part (/ index 4))))

;; ;;     ;; Find the window containing the target part
;; ;;     (dolist (win windows)
;; ;;       (setq window-edges (window-edges win))
;; ;;       (when (and (>= target-col (nth 0 window-edges))
;; ;;                  (< target-col (nth 2 window-edges))
;; ;;                  (>= target-row (nth 1 window-edges))
;; ;;                  (< target-row (nth 3 window-edges)))
;; ;;         (setq window win)))

;; ;;     ;; Move cursor to the calculated position within the selected window
;; ;;     (when window
;; ;;       (select-window window)
;; ;;       (goto-char (window-start))
;; ;;       (forward-line target-row)
;; ;;       (move-to-column target-col))))

;; ;; (defun jump-to-part-of-frame ()
;; ;;   "Jump to a character in a specific part of the frame, based on key input, considering each window's dimensions."
;; ;;   (interactive)
;; ;;   (let* ((key (read-char "Press one of the keys qdrwashtfup:neoi': "))
;; ;;          (frame-windows (window-list))
;; ;;          (frame-grid-index (cl-case key
;; ;;                              (?q 0) (?d 1) (?r 2) (?w 3)
;; ;;                              (?a 4) (?s 5) (?h 6) (?t 7)
;; ;;                              (?f 8) (?u 9) (?p 10) (?: 11)
;; ;;                              (?n 12) (?e 13) (?o 14) (?i 15)
;; ;;                              (t (error "Invalid key! Use one of qdrwashtfup:neoi'"))))
;; ;;          (grid-row (/ frame-grid-index 4))
;; ;;          (grid-col (/ frame-grid-index 4)))
;; ;;     ;; Iterate through each window to find the target one based on the grid position
;; ;;     (dolist (win frame-windows)
;; ;;       (let* ((win-start (window-start win))
;; ;;              (win-end (window-end win t))
;; ;;              (win-height (count-lines win-start win-end))
;; ;;              (win-width (window-width win))
;; ;;              (rows-per-part (/ win-height 4))
;; ;;              (cols-per-part (/ win-width 4))
;; ;;              (target-row (+ (* rows-per-part grid-row) (/ rows-per-part 2)))
;; ;;              (target-col (+ (* cols-per-part grid-col) (/ cols-per-part 2))))
;; ;;         (when (and (>= target-row 0) (< target-row win-height)
;; ;;                    (>= target-col 0) (< target-col win-width))
;; ;;           (select-window win)
;; ;;           (goto-char win-start)
;; ;;           (forward-line target-row)
;; ;;           (move-to-column target-col)
;; ;;           (return))))))


;; (defun jump-to-frame-part ()
;;   "Jump to the center of a part of the frame divided into 16 parts."
;;   (interactive)
;;   (let* ((key-to-part (read-char "Press one of the keys (qdrwashtfup:neoi'): "))
;;          ;; (total-lines 46);; TODO: может быть разным от размера окна.
;;          ;; (total-columns 170);; TODO: может быть разным от размера окна.
;;          (total-lines (sum-lines (window-list)))
;;          (total-columns (sum-columns (window-list)))
;;          (part-index (key-to-part-index key-to-part))
;;          (row-per-part (/ total-lines 4))
;;          (col-per-part (/ total-columns 4))
;;          (target-line (+ 1 (* row-per-part (/ part-index 4))))
;;          (target-column (+ 1 (* col-per-part (/ part-index 4))))
;;          target-window)

;;     ;; Find the window that contains the target position
;;     (setq target-window (find-target-window target-line target-column))

;;     ;; Move cursor to the calculated position in the found window
;;     (when target-window
;;       (select-window target-window)
;;       (goto-line target-line)
;;       (move-to-column target-column t))))

;; (defun sum-lines (windows)
;;   "Calculate the total number of visible lines across all WINDOWS."
;;   (apply '+ (mapcar (lambda (win) (count-lines (window-start win) (window-end win t))) windows)))

;; (defun sum-columns (windows)
;;   "Calculate the total number of visible columns across all WINDOWS arranged horizontally."
;;   (let ((total-width 0))
;;     (dolist (win windows total-width)
;;       (setq total-width (+ total-width (window-width win))))))


;; (defun find-target-window (line column)
;;   "Find the window that contains the target LINE and COLUMN."
;; (let ((accum-line 0) (accum-column 0))
;;     (catch 'found
;;       (dolist (win (window-list))
;;         (let ((win-lines (count-lines (window-start win) (window-end win t)))
;;               (win-cols (window-width win)))
;;           (setq accum-line (+ accum-line win-lines))
;;           (when (and (<= line accum-line) (<= column accum-column))
;;             (throw 'found win))
;;           (setq accum-column (+ accum-column win-cols)))))))


(defun jump-to-pixel-part ()
  "Jump to the center of a part of the frame divided into 16 parts."
  (interactive)
  (let* ((key-to-part (read-char "Press one of the keys (qdrwashtfup:neoi'): "))
         (width (frame-pixel-width))
         (height (frame-pixel-height))
         (part-index (key-to-part-index key-to-part))
         (coords-x-per-part (/ width 4))
         (coords-y-per-part (/ height 4))
         (target-x (+ (* coords-x-per-part (/ part-index 4)) (/ coords-x-per-part 2)))
         (target-y (+ (* coords-y-per-part (/ part-index 4)) (/ coords-y-per-part 2))))

    (jump-to-pixel target-x target-y)
    ))
(defun jump-to-refined-pixel-part ()
  "Jump to a more refined part of the frame divided initially into 16 parts, then subdivided again."
  (interactive)
  (let* ((first-key (read-char "Press first key (one of qdrwashtfup:neoi'): "))
         (second-key (read-char "Press second key (one of qdrwashtfup:neoi'): "))
         (width (frame-pixel-width))
         (height (frame-pixel-height))
         (first-part-index (key-to-part-index first-key))
         (second-part-index (key-to-part-index second-key))
         ;; Calculate the width and height of each primary part
         (primary-x-per-part (/ width 4))
         (primary-y-per-part (/ height 4))
         ;; Determine the top-left corner of the first selected part
         (base-x (* primary-x-per-part (/ first-part-index 4)))
         (base-y (* primary-y-per-part (/ first-part-index 4)))
         ;; Calculate the width and height of each subdivided part
         (sub-x-per-part (/ primary-x-per-part 4))
         (sub-y-per-part (/ primary-y-per-part 4))
         ;; Calculate the center of the subdivided part
         (target-x (+ base-x (* sub-x-per-part (/ second-part-index 4)) (/ sub-x-per-part 2)))
         (target-y (+ base-y (* sub-y-per-part (/ second-part-index 4)) (/ sub-y-per-part 2))))

    ;; Jump to the pixel coordinates corresponding to the center of the selected subdivided part
    (jump-to-pixel target-x target-y)))

(defun window-at-pixel (x y)
  "Return the window at frame pixel coordinates X and Y."
  (catch 'found
    (dolist (window (window-list))
      (let ((edges (window-pixel-edges window)))
        (if (and (>= x (nth 0 edges)) (<= x (nth 2 edges))
                 (>= y (nth 1 edges)) (<= y (nth 3 edges)))
            (throw 'found window))))))

(defcustom my-avy-background nil
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)
(setq my-avy-background t)

(defvar my-avy--overlays-lead nil
  "Hold overlays for leading chars.")
(defvar my-avy--overlays-back nil
  "Hold overlays for when `avy-background' is t.")
(defface my-avy-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defun my-avy--make-backgrounds (wnd-list)
  "Create a dim background overlay for each window on WND-LIST."
  (when my-avy-background
    (setq my-avy--overlays-back
          (mapcar (lambda (w)
                    (let ((ol (make-overlay
                               (window-start w)
                               (window-end w)
                               (window-buffer w))))
                      (overlay-put ol 'face 'my-avy-background-face)
                      (overlay-put ol 'window w)
                      ol))
                  wnd-list))))
;; (my-avy--make-backgrounds (window-list))

(defun my-avy--done ()
  "Clean up overlays."
  (mapc #'delete-overlay my-avy--overlays-back)
  (setq my-avy--overlays-back nil)
  (my-avy--remove-leading-chars))

(defun my-avy--remove-leading-chars ()
  "Remove leading char overlays."
  (mapc #'delete-overlay my-avy--overlays-lead)
  (setq my-avy--overlays-lead nil))


;; (highlight-to-pixel 240 127 ?q)
(defun highlight-to-pixel (x y key)
  "Highlight the character at global frame pixel coordinates X and Y with an overlay showing KEY."
  (interactive)
  (my-avy--make-backgrounds (window-list))
  (let ((target-window (window-at-pixel x y)))
    (if target-window
        (let* ((window-edges (window-pixel-edges target-window))
               (pos-x (- x (first window-edges)))
               (pos-y (- y (second window-edges)))
               (max-x (window-body-width target-window t))
               (max-pos-y (cdr (posn-x-y (posn-at-point (window-end target-window t) nil))))
               ;; может выбраться символ, которые виден наполовину, и у него будет nil?
               (max-y (if (not max-pos-y) 1 max-pos-y))
               (xxx (message "H Value of max-x: %s" max-x))
               (xx (message "H Value of max-x: %s, max-y: %s" max-x max-y))
               (local-x (if (and (>= pos-x 0) (<= pos-x max-x))
                            pos-x
                          max-x))
               (local-y (if (and (>= pos-y 0) (<= pos-y max-y))
                            pos-y
                          max-y))
               (posn (posn-at-x-y local-x local-y target-window)))
          (if posn
              (with-selected-window target-window
                (goto-char (posn-point posn))
                (let ((ov (make-overlay (point) (1+ (point)))))
                  (overlay-put ov 'category 'my-avy-myyyy)
                  (overlay-put ov 'display (propertize (char-to-string key) 'face '(:foreground "red")))
                  (overlay-put ov 'help-echo "Highlighted key")
                  (push ov my-avy--overlays-lead)
                  ))
            (message "No character found at these local pixel coordinates.")))
      (message "No window found at these global pixel coordinates."))))

;; (avy--done)
;; (highlight-to-pixel 100 100 ?q)
;; (highlight-to-pixel 200 300 ?r)
;; (highlight-to-pixel 1910 900 ?k)
;; ;; clear overlay
;; (remove-overlays (point-min) (point-max))

;; (defun jump-to-pixel (x y)
;;   "Jump to the nearest character at global frame pixel coordinates X and Y."
;;   (interactive "nX pixel coordinate: \nnY pixel coordinate: ")
;;   (setq x 60)
;;   (setq y 982)
;;   (setq target-window (window-at-pixel x y))
;;   (setq window-edges (window-pixel-edges target-window))
;;   (posn-at-x-y 60 982 target-window)
;;   (window-body-width target-window t)
;;   (window-body-height target-window t)
;;   (let ((target-window (window-at-pixel x y)))
;;     (if target-window
;;         (let* ((window-edges (window-pixel-edges target-window))
;;                (local-x (- x (first window-edges)))
;;                (local-y (- y (second window-edges)))
;;                (posn (posn-at-x-y local-x local-y target-window)))
;;           (if posn
;;               (progn
;;                 (select-window target-window)
;;                 (goto-char (posn-point posn)))
;;             (message "No character found at these local pixel coordinates.")))
;;       (message "No window found at these global pixel coordinates."))))
(defun jump-to-pixel (x y)
  "Jump to the nearest character at global frame pixel coordinates X and Y."
  (interactive)
  (let ((target-window (window-at-pixel x y)))
    (if target-window
        (let* ((window-edges (window-pixel-edges target-window))
               (pos-x (- x (first window-edges)))
               (pos-y (- y (second window-edges)))
               ;; (window-width (window-body-width target-window t))
               ;; (window-height (window-body-height target-window t))
               ;; Т.к. самый последний символ - не обязательно самый крайний x
               ;; TODO: не будет ли падать в каких-то случаях?
               (max-x (window-body-width target-window t))
               (max-pos-y (cdr (posn-x-y (posn-at-point (window-end target-window t) nil))))
               ;; может выбраться символ, которые виден наполовину, и у него будет nil?
               (max-y (if (not max-pos-y) 1 max-pos-y))
               (xx (message "J Value of max-x: %s, max-y: %s" max-x max-y))
               (local-x (if (and (>= pos-x 0) (<= pos-x max-x))
                            pos-x
                          max-x))
               (local-y (if (and (>= pos-y 0) (<= pos-y max-y))
                            pos-y
                          max-y))
               (posn (posn-at-x-y local-x local-y target-window)))
          (if posn
              (progn
                (select-window target-window)
                (goto-char (posn-point posn)))
            (message "No character found at these local pixel coordinates.")))
      (message "No window found at these global pixel coordinates."))))

(defun key-to-part-index (key keys)
  "Convert a KEY to a corresponding part index using KEYS array."
  (let ((index (cl-position (char-to-string key) keys :test 'string=)))
    (if index
        index
      (error "Invalid key! Use one of %s" (mapconcat 'identity keys ", ")))))

;; Function to highlight keys
(defun highlight-keys
 (base-x base-y x-per-part y-per-part keys)
 (dotimes (index 16)
   (let* ((key (nth index keys))
          (row (/ index 4))
          (col (mod index 4))
          (center-x (+ base-x (* col x-per-part) (/ x-per-part 2)))
          (center-y (+ base-y (* row y-per-part) (/ y-per-part 2))))
     (message "highlight-to-pixel %d %d %s" center-x center-y key)
     (highlight-to-pixel center-x center-y (string-to-char key)))))
(highlight-to-pixel 60 982 "n")

(defun highlight-refined-pixel-parts ()
  "Highlight and jump to a more refined part of the frame, divided initially into 16 parts, then subdivided again."
  (interactive)
  (let* ((keys '("q" "d" "r" "w" "a" "s" "h" "t" "f" "u" "p" ":" "n" "e" "o" "i"))
         (width (frame-pixel-width))
         (height (frame-pixel-height))
         (primary-x-per-part (/ width 4))
         (primary-y-per-part (/ height 4)))
    (message "Value of primary-x-per-part: %d" primary-x-per-part)
    (message "Value of primary-y-per-part: %d" primary-y-per-part)

    ;; Initial highlighting
    (highlight-keys 0 0 primary-x-per-part primary-y-per-part keys)

    (let* ((first-key (read-char "Press first key (one of qdrwashtfup:neoi'): "))
           (xxx (message "Value of first-key: %c" first-key))
           (first-part-index (key-to-part-index first-key keys))
           (x4 (message "Value of first-part-index: %d" first-part-index))
           ;; Determine the top-left corner of the first selected part
           (base-x (* primary-x-per-part (mod first-part-index 4)))
           (base-y (* primary-y-per-part (/ first-part-index 4)))
           ;; Calculate the width and height of each subdivided part
           (sub-x-per-part (/ primary-x-per-part 4))
           (sub-y-per-part (/ primary-y-per-part 4))
           (x5 (message "Value of sub-x-per-part: %d" sub-x-per-part))
           )
      (message "Done let*")
      ;; clear first overlay
      (remove-overlays (point-min) (point-max))
      (my-avy--done)
      (message "Done overlay")
      ;; Second highlighting
      (message "Values before highlight: %d %d %d %d %s" base-x base-y sub-x-per-part sub-y-per-part keys)
      (highlight-keys base-x base-y sub-x-per-part sub-y-per-part keys)
      (message "Done hightlight")

      (let* ((second-key (read-char "Press second key (one of qdrwashtfup:neoi'): "))
             (x (message "Value of second-key: %c" second-key))
             (second-part-index (key-to-part-index second-key keys))
             (xx (message "Value of second-part-index: %d" second-part-index))
             ;; Calculate the center of the subdivided part
             (target-x (+ base-x (* sub-x-per-part (mod second-part-index 4)) (/ sub-x-per-part 2)))
             (target-y (+ base-y (* sub-y-per-part (/ second-part-index 4)) (/ sub-y-per-part 2))))
        (message "Value of target-x: %d" target-x)
        (message "Value of target-y: %d" target-y)
        ;; clear second overlay
        (my-avy--done)

        ;; Jump to the final position
        (jump-to-pixel target-x target-y)))))


(setq debug-on-error t)

;;1440 762 120 63 (q d r w a s h t f u p : n e o i)
;;       (remove-overlays (point-min) (point-max))
;;       (avy--done)

(setq keys '("q" "d" "r" "w" "a" "s" "h" "t" "f" "u" "p" ":" "n" "e" "o" "i"))
(highlight-keys 0 0 480 254 keys)
(highlight-keys 0 0 120 63 keys)
(jump-to-pixel 60 982)
(jump-to-pixel 100 560)

;; (car (posn-x-y (posn-at-point (window-end target-window t) nil)))
;; (cdr (posn-x-y (posn-at-point (window-end target-window t) nil)))

(remove-overlays (point-min) (point-max))
(defun remove-overlays-in-all-windows ()
  "Remove all overlays in all buffers displayed in any window."
  (interactive)
  (walk-windows (lambda (window)
                  (with-selected-window window
                    (remove-overlays (point-min) (point-max))))
                nil t))
(remove-overlays-in-all-windows)
(global-set-key (kbd "C-c j") 'highlight-refined-pixel-parts)
