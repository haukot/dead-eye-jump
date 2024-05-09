;; https://github.com/abo-abo/avy/blob/master/avy.el
;; https://github.com/winterTTr/ace-jump-mode/blob/master/ace-jump-mode.el#L513

;; https://emacs.stackexchange.com/questions/14920/how-to-determine-the-line-number-of-the-first-visible-line-of-a-window
;; https://emacs.stackexchange.com/questions/10763/how-to-update-window-start-without-calling-redisplay/10768#10768
;; https://stackoverflow.com/questions/23923371/emacs-calculating-new-window-start-end-without-redisplay/24216247#24216247
;; https://emacs.stackexchange.com/questions/3821/a-faster-method-to-obtain-line-number-at-pos-in-large-buffers

(defun window-at-pixel (x y)
  "Return the window at frame pixel coordinates X and Y."
  (catch 'found
    (dolist (window (window-list))
      (let ((edges (window-pixel-edges window)))
        (if (and (>= x (nth 0 edges)) (<= x (nth 2 edges))
                 (>= y (nth 1 edges)) (<= y (nth 3 edges)))
            (throw 'found window))))))

(defcustom dead-eye-jump-background nil
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)
(defcustom dead-eye-jump-keys nil
  "Keys to use for jump"
  :type 'list)
;; Workman layout keys, could be querty
(setq dead-eye-jump-keys '("q" "d" "r" "w" "a" "s" "h" "t" "f" "u" "p" ":" "n" "e" "o" "i"))

(defcustom dead-eye-jump-repeats 3
  "Number of times to repeat the jump."
  :type 'integer)

(defvar dead-eye-jump--overlays-lead nil
  "Hold overlays for leading chars.")
(defvar dead-eye-jump--overlays-back nil
  "Hold overlays for when `avy-background' is t.")
(defface dead-eye-jump-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

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
;; (dead-eye-jump--make-backgrounds (window-list))

(defun dead-eye-jump--done ()
  "Clean up overlays."
  (mapc #'delete-overlay dead-eye-jump--overlays-back)
  (setq dead-eye-jump--overlays-back nil)
  (dead-eye-jump--remove-leading-chars)
  )

(defun dead-eye-jump--remove-leading-chars ()
  "Remove leading char overlays."
  (mapc #'delete-overlay dead-eye-jump--overlays-lead)
  (setq dead-eye-jump--overlays-lead nil))

(defun action-at-pixel (x y fun)
  "Make an action at the nearest character at global frame pixel coordinates X and Y."
  ;; (interactive)
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
               ;; (max-y (cdr (last(posn-x-y (posn-at-point (window-end target-window t) nil)))))
               ;; может выбраться символ, которые виден наполовину, и у него будет nil?
               ;; (max-y (if (not max-pos-y) 1 max-pos-y))

               (max-y (- (window-body-height target-window t)
                         (window-mode-line-height target-window)
                         (window-header-line-height target-window)
                         (window-tab-line-height target-window)
                         ))

               (xx (message "J Value of max-x: %s, max-y: %s" max-x max-y))
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

(defun jump-to-pixel (x y)
  "Jump to the nearest character at global frame pixel coordinates X and Y."
  (interactive)
  (action-at-pixel x y (lambda (pos target-window)
                           (select-window target-window)
                           (goto-char pos))))

(defun highlight-to-pixel (x y key)
  "Highlight the character at global frame pixel coordinates X and Y with an overlay showing KEY."
  (interactive)
  (dead-eye-jump--make-backgrounds (window-list))
  (action-at-pixel x y (lambda (pos target-window)
                         (with-selected-window target-window
                           ;; TODO: это мб не надо?
                           (goto-char pos)
                           (unless (eobp)  ; Check if at end of buffer
                             (let* ((ov (make-overlay (point) (min (1+ (point)) (point-max))))
                                    (char-at-pos (if (eobp) " "  ; Use space if at end of buffer
                                                   (buffer-substring-no-properties (point) (1+ (point)))))
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
                               ;; TODO: в ace-jump записывает прямо в overlay метадату о
                               ;; позиции, и потом прыгает прямо в неё, как (overlay-put ol 'aj-data p)
                               (push ov dead-eye-jump--overlays-lead)))))))

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
;; (highlight-to-pixel 60 982 "n")


;; read key
;; (highlight-keys 0 0 primary-x-per-part primary-y-per-part keys)
;; (first-key (read-char "Press first key (one of qdrwashtfup:neoi'): "))
;; (first-part-index (key-to-part-index first-key keys))
;; (base-x (* primary-x-per-part (mod first-part-index 4)))
;; (base-y (* primary-y-per-part (/ first-part-index 4)))
;;                  (sub-x-per-part (/ primary-x-per-part 4))
;;                  (sub-y-per-part (/ primary-y-per-part 4))
;; (dead-eye-jump--done)

(defun dead-eye-jump (base-x base-y width height level)
  "Recursively highlight and jump to a more refined part of the frame, starting from a given subregion."
  (interactive (list 0 0 (frame-pixel-width) (frame-pixel-height) dead-eye-jump-repeats)) ; Start with full frame and divide it into 16 parts
  (message "dead-eye-jump %d %d %d %d %d" base-x base-y width height level)
  (let* ((keys dead-eye-jump-keys)
         (parts-per-side 4)
         (sub-width (max (/ width parts-per-side) 1))
         (sub-height (max (/ height parts-per-side) 1)))

  (unwind-protect
      (progn
        ;; Highlight the current region subdivided by the level
        (highlight-keys base-x base-y sub-width sub-height keys)

        (let* ((key (read-char "Press key for next region: "))
               (index (key-to-part-index key keys))
               (new-base-x (+ base-x (* sub-width (mod index parts-per-side))))
               (new-base-y (+ base-y (* sub-height (/ index parts-per-side))))
               )
          ;; Remove the current highlights
          (dead-eye-jump--done)
          (if (= level 1)
              (progn
                ;; Jump to selected subregion
                (let* ((center-x (+ new-base-x (/ sub-width 2)))
                       (center-y (+ new-base-y (/ sub-height 2))))
                  (jump-to-pixel center-x center-y))
                (dead-eye-jump--done))
            ;; Recursive call
            (dead-eye-jump new-base-x new-base-y sub-width sub-height (- level 1)))
          ))
    (dead-eye-jump--done)
    )))

;;1440 762 120 63 (q d r w a s h t f u p : n e o i)
;;       (remove-overlays (point-min) (point-max))
;;       (avy--done)

;; (setq keys '("q" "d" "r" "w" "a" "s" "h" "t" "f" "u" "p" ":" "n" "e" "o" "i"))
;; (highlight-keys 0 0 480 254 keys)
;; (highlight-keys 0 0 120 63 keys)
;; (jump-to-pixel 60 982)
;; (jump-to-pixel 100 560)

;; (car (posn-x-y (posn-at-point (window-end target-window t) nil)))
;; (cdr (posn-x-y (posn-at-point (window-end target-window t) nil)))

;; (remove-overlays (point-min) (point-max))

;; (setq debug-on-error t)

(defun remove-overlays-in-all-windows ()
  "Remove all overlays in all buffers displayed in any window."
  (interactive)
  (walk-windows (lambda (window)
                  (with-selected-window window
                    (remove-overlays (point-min) (point-max))))
                nil t))
;; (remove-overlays-in-all-windows)
(global-set-key (kbd "C-c j") 'dead-eye-jump)
