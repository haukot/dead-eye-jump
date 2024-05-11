(defcustom dead-eye-jump-background t
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)
(defcustom dead-eye-jump-keys nil
  "Keys to use for jump"
  :type 'list)

;; Workman keyboard layout keys, could be querty
;; (setq dead-eye-jump-keys '("q" "d"  "r" "w"
;;                            "a" "s"  "h" "t"
;;                            "f" "u"  "p" ":"
;;                            "n" "e"  "o" "i"))

(setq dead-eye-jump-keys '("q" "d"  "f" "u"
                           "r" "w"  "p" ":"
                           "a" "s"  "n" "e"
                           "h" "t"  "o" "i"))

(defcustom dead-eye-jump-repeats 3
  "Number of times to repeat the jump."
  :type 'integer)

(defvar dead-eye-jump--overlays-lead nil
  "Hold overlays for leading chars.")
(defvar dead-eye-jump--overlays-back nil
  "Hold overlays for when `dead-eye-jump-background' is t.")
(defface dead-eye-jump-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defun dead-eye-jump--find-nearest-window-to-pixel (x y)
  "Find the window nearest to the pixel coordinates X and Y."
  (catch 'found
    (let ((nearest-window nil)
          (min-distance most-positive-fixnum))
      (dolist (win (window-list))
        (let* ((edges (window-pixel-edges win))
               (left (nth 0 edges))
               (top (nth 1 edges))
               (right (nth 2 edges))
               (bottom (nth 3 edges))
               (distance (min (abs (- left x))
                              (abs (- right x))
                              (abs (- top y))
                              (abs (- bottom y)))))
          ;; return if find exact match
          (if (and (>= x (nth 0 edges)) (<= x (nth 2 edges))
                   (>= y (nth 1 edges)) (<= y (nth 3 edges)))
              (throw 'found win))
          ;; else seek for min distance
          (when (< distance min-distance)
            (setq min-distance distance)
            (setq nearest-window win))))
      nearest-window)))

(defun dead-eye-jump--make-backgrounds (wnd-list)
  "Create a dim background overlay for each window on WND-LIST."
  (when dead-eye-jump-background
    (setq dead-eye-jump--overlays-back
          (mapcar (lambda (w)
                    (let ((ol (make-overlay
                               (window-start w)
                               (window-end w)
                               (window-buffer w))))
                      (overlay-put ol 'face 'dead-eye-jump-background-face)
                      (overlay-put ol 'window w)
                      ol))
                  wnd-list))))

(defun dead-eye-jump--remove-leading-chars ()
  "Remove leading char overlays."
  (mapc #'delete-overlay dead-eye-jump--overlays-lead)
  (setq dead-eye-jump--overlays-lead nil))

(defun dead-eye-jump--remove-background ()
  "Remove background overlays."
  (mapc #'delete-overlay dead-eye-jump--overlays-back)
  (setq dead-eye-jump--overlays-back nil))

(defun dead-eye-jump--done ()
  "Clean up overlays."
  (dead-eye-jump--remove-background)
  (dead-eye-jump--remove-leading-chars)
  )

(defun dead-eye-jump--action-at-pixel (x y fun)
  "Make an action at the nearest character at global frame pixel coordinates X and Y."
  ;; (interactive)
  (let ((target-window (dead-eye-jump--find-nearest-window-to-pixel x y)))
    (if target-window
        (let* ((window-edges (window-pixel-edges target-window))
               (pos-x (- x (first window-edges)))
               (pos-y (- y (second window-edges)))
               ;; Because last char is not always last x
               ;; TODO: is this robust?
               (max-x (window-body-width target-window t))

               (max-y (- (window-body-height target-window t)
                         (window-mode-line-height target-window)
                         (window-header-line-height target-window)
                         (window-tab-line-height target-window)
                         ))

               ;; (xx (message "J Value of max-x: %s, max-y: %s" max-x max-y)) ;; debug
               (local-x (if (and (>= pos-x 0) (<= pos-x max-x))
                            pos-x
                          max-x))
               (local-y (if (and (>= pos-y 0) (<= pos-y max-y))
                            pos-y
                          max-y))
               (posn (posn-at-x-y local-x local-y target-window)))
          (if posn
              (funcall fun (posn-point posn) target-window)
            (message "No character found at these local pixel coordinates.")))
      (message "No window found at these global pixel coordinates."))))

(defun dead-eye-jump--jump-to-pixel (x y)
  "Jump to the nearest character at global frame pixel coordinates X and Y."
  (interactive)
  (dead-eye-jump--action-at-pixel x y (lambda (pos target-window)
                           (select-window target-window)
                           (goto-char pos))))

(defun dead-eye-jump--highlight-to-pixel (x y key)
  "Highlight the character at global frame pixel coordinates X and Y with an overlay showing KEY."
  (interactive)
  (dead-eye-jump--action-at-pixel x y (lambda (pos target-window)
                         (with-selected-window target-window
                           (unless (eobp)  ; Check if at end of buffer
                             (let* ((ov (make-overlay pos (min (1+ pos) (point-max))))
                                    (char-at-pos (if (>= (1+ pos) (point-max)) " "  ; Use space if at end of buffer
                                                   (buffer-substring-no-properties pos (1+ pos))))
                                    (display-string (cond
                                                     ;; from https://github.com/winterTTr/ace-jump-mode/blob/master/ace-jump-mode.el#L513
                                                     ;; so overlay will not join strings
                                                     ((string-equal char-at-pos "\t")
                                                      (concat (make-string 1 key) (make-string (1- tab-width) ? )))
                                                     ((string-equal char-at-pos "\n")
                                                      (concat (make-string 1 key) "\n"))
                                                     (t
                                                      (concat (make-string 1 key)
                                                              (make-string (max 0 (1- (string-width char-at-pos))) ? ))))))
                               ;; from https://github.com/winterTTr/ace-jump-mode/blob/master/ace-jump-mode.el#L468
                               ;; because sometimes the different
                               ;; window may dispaly the same buffer, in that case,
                               ;; overlay for different window (but the same buffer)
                               ;; will show at the same time on both window
                               ;; So we make it only on the specific window
                               (overlay-put ov 'window target-window)
                               (overlay-put ov 'category 'dead-eye-jump-myyyy)
                               (overlay-put ov 'display (propertize display-string 'face '(:foreground "red")))
                               (overlay-put ov 'help-echo "Highlighted key")
                               ;; TODO: ace-jump writes position metadata right
                               ;; in overlay like (overlay-put ol 'aj-data p)
                               ;; and then jumps by that metadata
                               ;; TODO: we probably could show key in correct place(even
                               ;; if it's empty) with overlay-put overlay 'before-string or 'after-string
                               ;; as in https://github.com/alpaker/fill-column-indicator/blob/master/fill-column-indicator.el#L773
                               (push ov dead-eye-jump--overlays-lead)))))))

(defun dead-eye-jump--key-to-part-index (key keys)
  "Convert a KEY to a corresponding part index using KEYS array."
  (let ((index (cl-position (char-to-string key) keys :test 'string=)))
    (if index
        index
      (error "Invalid key! Use one of %s" (mapconcat 'identity keys ", ")))))

;; Function to highlight keys
(defun dead-eye-jump--highlight-keys (base-x base-y sub-width sub-height keys)
  (dead-eye-jump--make-backgrounds (window-list))
  (dotimes (index 16)
    (let* ((key (nth index keys))
           ;; TODO: unify with jump
           (col (mod index 4))
           (row (/ index 4))
           (center-x (+ base-x (* col sub-width) (/ sub-width 2)))
           (center-y (+ base-y (* row sub-height) (/ sub-height 2))))
      ;; (message "dead-eye-jump--highlight-to-pixel %d %d %s" center-x center-y key) ;; debug
      (dead-eye-jump--highlight-to-pixel center-x center-y (string-to-char key)))))

;;;###autoload
(defun dead-eye-jump (base-x base-y width height level)
  "Recursively highlight and jump to a more refined part of the frame, starting from a given subregion."
  (interactive (list 0 0 (frame-pixel-width) (frame-pixel-height) dead-eye-jump-repeats)) ; Start with full frame and divide it into 16 parts
  ;; (message "dead-eye-jump %d %d %d %d %d" base-x base-y width height level) ;; debug
  (let* ((keys dead-eye-jump-keys)
         (parts-per-side 4)
         (sub-width (max (/ width parts-per-side) 1))
         (sub-height (max (/ height parts-per-side) 1)))

  (unwind-protect
      (progn
        ;; Highlight the current region subdivided by the level
        ;; (message "dead-eye-jump--highlight-keys %d %d %d %d %s" base-x base-y sub-width sub-height keys) ;; debug
        (dead-eye-jump--highlight-keys base-x base-y sub-width sub-height keys)

        (let* ((key (read-char "Press key for next region: "))
               (index (dead-eye-jump--key-to-part-index key keys))
               (col (mod index parts-per-side))
               (row (/ index parts-per-side))
               (new-base-x (+ base-x (* sub-width col)))
               (new-base-y (+ base-y (* sub-height row)))
               )
          ;; Remove the current highlights
          (dead-eye-jump--done)
          (if (= level 1)
              (progn
                ;; Jump to selected subregion
                (let* ((center-x (+ new-base-x (/ sub-width 2)))
                       (center-y (+ new-base-y (/ sub-height 2))))
                  (dead-eye-jump--jump-to-pixel center-x center-y)
                  (dead-eye-jump--done))
                )
            ;; Recursive call
            (dead-eye-jump new-base-x new-base-y sub-width sub-height (- level 1)))
          ))
    (dead-eye-jump--done)
    )
  (dead-eye-jump--done)
  ))

;; technical function
(defun dead-eye-jump--remove-overlays-in-all-windows ()
  "Remove all overlays in all buffers displayed in any window."
  (interactive)
  (walk-windows (lambda (window)
                  (with-selected-window window
                    (remove-overlays (point-min) (point-max))))
                nil t))
;; (dead-eye-jump--remove-overlays-in-all-windows)

;; (setq debug-on-error t)
(global-set-key (kbd "C-j") 'dead-eye-jump)
