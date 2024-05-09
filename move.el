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
(setq dead-eye-jump-background t)

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
                                                     ;; чтобы оверлеи не джоинили строки
                                                     ((string-equal char-at-pos "\t")
                                                      (concat (make-string 1 key) (make-string (1- tab-width) ? )))
                                                     ((string-equal char-at-pos "\n")
                                                      (concat (make-string 1 key) "\n"))
                                                     (t
                                                      (concat (make-string 1 key)
                                                              (make-string (max 0 (1- (string-width char-at-pos))) ? ))))))
                               (overlay-put ov 'category 'dead-eye-jump-myyyy)
                               (overlay-put ov 'display (propertize display-string 'face '(:foreground "red")))
                               (overlay-put ov 'help-echo "Highlighted key")
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

(defun dead-eye-jump ()
  "Highlight and jump to a more refined part of the frame, divided initially into 16 parts, then subdivided again."
  (interactive)
  (let* ((keys '("q" "d" "r" "w" "a" "s" "h" "t" "f" "u" "p" ":" "n" "e" "o" "i"))
         (width (frame-pixel-width))
         (height (frame-pixel-height))
         (primary-x-per-part (/ width 4))
         (primary-y-per-part (/ height 4)))
    (message "Value of primary-x-per-part: %d" primary-x-per-part)
    (message "Value of primary-y-per-part: %d" primary-y-per-part)

    (unwind-protect
        (progn
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
            (dead-eye-jump--done)
            (message "Done overlay")
            ;; Second highlighting
            (message "Values before highlight: %d %d %d %d %s" base-x base-y sub-x-per-part sub-y-per-part keys)
            (highlight-keys base-x base-y sub-x-per-part sub-y-per-part keys)
            (message "Done hightlight")

            ;; TODO: read-from-minibuffer?
            (let* ((second-key (read-char "Press second key (one of qdrwashtfup:neoi'): "))
                   (x (message "Value of second-key: %c" second-key))
                   (second-part-index (key-to-part-index second-key keys))
                   ;; Determine the top-left corner of the second selected part
                   ;; (second-base-x (* base-x (mod second-part-index 4)))
                   ;; (second-base-y (* base-y (/ second-part-index 4)))
                   (second-base-x (+ base-x (* sub-x-per-part (mod second-part-index 4))))
                   (second-base-y (+ base-y (* sub-y-per-part (/ second-part-index 4))))
                   ;; Calculate the width and height of each subdivided part
                   (third-x-per-part (max (/ sub-x-per-part 4) 1))
                   (third-y-per-part (max (/ sub-y-per-part 4) 1))
                   (xx (message "Value of second-part-index: %d" second-part-index))
                   ;; Calculate the center of the subdivided part
                   )
              ;; clear second overlay
              (dead-eye-jump--done)

              (highlight-keys second-base-x second-base-y third-x-per-part third-y-per-part keys)

              ;; calc third and final target
              (let* ((third-key (read-char "Press third key (one of qdrwashtfup:neoi'): "))
                     (x (message "Value of third-key: %c" third-key))
                     (third-part-index (key-to-part-index third-key keys))
                     (xx (message "Value of third-part-index: %d" third-part-index))
                     ;; Calculate the center of the subdivided part
                     ;; (target-x (+ second-base-x (*  (mod third-part-index 4)) (/ second-target-x 2)))
                     ;; (target-y (+ second-base-y (* second-target-y (/ third-part-index 4)) (/ second-target-y 2))))
                     (final-x (+ second-base-x (* third-x-per-part (mod third-part-index 4)) (/ third-x-per-part 2)))
                     (final-y (+ second-base-y (* third-y-per-part (/ third-part-index 4)) (/ third-y-per-part 2))))

                (dead-eye-jump--done)

                (jump-to-pixel final-x final-y)
                ;; Jump to the final position
                ;; (jump-to-pixel target-x target-y)
                )
              )))
      (dead-eye-jump--done)
      )
    ;; finally in case of error
    ))


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
