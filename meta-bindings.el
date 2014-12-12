;; ## Navigation
(global-set-key "\M-c" 'previous-line)
(global-set-key "\M-t" 'next-line)
(global-set-key "\M-n" 'forward-word)
(global-set-key "\M-h" 'backward-word)
(global-set-key "\M-\S-c" 'backward-paragraph)
(global-set-key "\M-\S-t" 'forward-paragraph)
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
(global-set-key "\M-{" (lambda () (interactive) (select-window (previous-window))))
(global-set-key "\M-}" (lambda () (interactive) (select-window (next-window))))
;; ## Other
(global-set-key "\M--" 'undo)
(global-set-key "\M-\S-s" 'save-buffer)
(global-set-key "\M-k" 'kill-line)
(global-set-key "\M-g" 'keyboard-quit)

