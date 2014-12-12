;; ## Control
(global-set-key "\M-c" 'previous-line)
(global-set-key "\M-t" 'next-line)
(global-set-key "\M-n" 'forward-char)
(global-set-key "\M-h" 'backward-char)
(global-set-key "\M-\S-c" 'backward-paragraph)
(global-set-key "\M-\S-t" 'forward-paragraph)
(global-set-key "\M-\S-n" 'forward-word)
(global-set-key "\M-\S-h" 'backward-word)
;; ## Copy-paste
(global-set-key "\M-y" 'yank)
(global-set-key "\M-\S-y" 'yank-pop)
;; ## Search
(global-set-key "\M-s" 'isearch-forward)
(global-set-key "\M-r" 'isearch-backward)
;; ## Other
(global-set-key "\M--" 'undo)
(global-set-key "\M-\S-s" 'save-buffer)
(global-set-key "\M-k" 'kill-line)
(global-set-key "\M-q" 'keyboard-quit)

