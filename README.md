# Dead Eye Jump

Emacs plugin like avy or ace-jump or EasyMotion, but with different mechanic - you select place in full frame based of quadrant of the screen.
So it'll move mechanic action without additional thinking - one place always will be the same keys.
And in 3 keys you could move(almost exactly) where you want on the screen.

# Usage

Add to config
```eslisp
;; optionally add your keys to navigate
;; (setq dead-eye-jump-keys '("q" "w"  "u" "i"
;;                            "e" "r"  "o" "p"
;;                            "a" "s"  "j" "k"
;;                            "d" "f"  "l" ";"))

;; add keybdinging
(global-set-key (kbd "C-j") 'dead-eye-jump)
```

`C-j <char> <char> <char>`

Examples:

`C-j`
![1](./assets/1.png)

Press `a`
![2](./assets/2.png)

Press `h`
![3](./assets/3.png)

Press `u`
![4](./assets/4.png)

# Configuration

```eslisp
(defcustom dead-eye-jump-keys
  '("q" "w"  "u" "i"
    "e" "r"  "o" "p"
    "a" "s"  "j" "k"
    "d" "f"  "l" ";")
  "Keys to use for jump"
  :type 'list)

(defcustom dead-eye-jump-repeats 3
  "Number of times to repeat the jump."
  :type 'integer)

(defcustom dead-eye-jump-columns 4
  "Number of columns to divide the frame into."
  :type 'integer)

(defcustom dead-eye-jump-rows 4
  "Number of rows to divide the frame into."
  :type 'integer)

(defcustom dead-eye-jump-background t
  "When non-nil, a gray background will be added during the selection."
  :type 'boolean)

(defvar dead-eye-jump--overlays-lead nil
  "Hold overlays for leading chars.")

(defvar dead-eye-jump--overlays-back nil
  "Hold overlays for when `dead-eye-jump-background' is t.")

(defface dead-eye-jump-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")
```

# TODO

* Should not count height and width of bars, modelines and so on?
* Make command to stop in middle of seeking(e.g. if first char already on right spot)
* Overlays should be placed in empty places also, padding them with characters. This is because the quadrants should always be clearly visible, otherwise it's unclear where it will send since the point is not in the center.

# Similar packages

* https://github.com/abo-abo/avy
* https://github.com/winterTTr/ace-jump-mode
