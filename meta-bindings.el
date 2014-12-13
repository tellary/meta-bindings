;; This keymap is designed for dvorak layout.
;; It is targeted to maximal use of "<meta>" key as
;; it is easy to press it with thumb -- the biggest finger on hand.
;; "h", "n", "c" and "t" keys together with "<meta>" are used for
;; navigation bindings with directional meaning of
;; "left", "right", "up" and "down" respectively.
;; All other key binding are designed to be the same as in
;; default emacs keymap except that "C-" is replaced with "M-S-" prefix.

;; ## Navigation
(global-set-key "\M-c" 'previous-line)
(global-set-key "\M-t" 'next-line)
(global-set-key "\M-n" 'forward-word)
(define-key markdown-mode-map "\M-n" 'forward-word)
(global-set-key "\M-h" 'backward-word)
(global-set-key "\M-\S-c" 'backward-paragraph)
(global-set-key "\M-\S-t" 'forward-paragraph)
(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)
(define-key markdown-mode-map (kbd "M-<up>") 'backward-paragraph)
(define-key markdown-mode-map (kbd "M-<down>") 'forward-paragraph)
(global-set-key "\M-\S-n" 'forward-char)
(global-set-key "\M-\S-h" 'backward-char)
(global-set-key "\M-a" 'move-beginning-of-line)
(global-set-key "\M-e" 'move-end-of-line)
(global-set-key "\M-\S-a" 'beginning-of-buffer)
(global-set-key "\M-\S-e" 'end-of-buffer)

;; ## Selection and copy, cut and paste
(global-set-key "\M-y" 'yank)
(global-set-key "\M-\S-y" 'yank-pop)
; M-w is bound by default to 'kill-ring-save
(global-set-key "\M-\S-w" 'kill-region)
(global-set-key "\M- " 'set-mark-command)
(global-set-key "\M-d" 'backward-kill-word)
(global-set-key "\M-\S-d" 'backward-delete-char-untabify)

;; ## Search
(global-set-key "\M-s" 'isearch-forward)
(global-set-key "\M-r" 'isearch-backward)
(define-key isearch-mode-map "\M-s" 'isearch-repeat-forward)
(define-key isearch-mode-map "\M-r" 'isearch-repeat-backward)
(define-key isearch-mode-map "\M-g" 'isearch-abort)

;; ## Windows
(defun select-next-window ()
  (interactive)
  (select-window (previous-window)))
(defun select-previous-window ()
  (interactive)
  (select-window (next-window)))
(global-set-key "\M-{" 'select-previous-window)
(global-set-key "\M-}" 'select-next-window)
(define-key markdown-mode-map "\M-{" 'select-previous-window)
(define-key markdown-mode-map "\M-}" 'select-next-window)

;; ## Buffers
(global-set-key "\M-b" 'switch-to-buffer)
(global-set-key "\M-\S-b" 'list-buffers)
(global-set-key "\M-\S-k" 'kill-buffer)

;; ## Quit/Abort
(global-set-key "\M-g" 'keyboard-quit)
(define-key minibuffer-local-map "\M-g" 'abort-recursive-edit)

;; ## Other
(global-set-key "\M--" 'undo)
(global-set-key "\M-\S-s" 'save-buffer)
(global-set-key "\M-k" 'kill-line)
(global-set-key "\M-u" 'universal-argument)
(global-set-key "\M-\S-x\t" 'indent-rigidly)
(global-set-key "\M-f" 'find-file)
