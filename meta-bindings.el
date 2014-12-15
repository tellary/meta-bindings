;; This keymap is designed for dvorak layout.
;; It is targeted to maximal use of "<meta>" key as
;; it is easy to press it with thumb -- the biggest finger on hand.
;; "h", "n", "c" and "t" keys together with "<meta>" are used for
;; navigation bindings with directional meaning of
;; "left", "right", "up" and "down" respectively.
;; All other key binding are designed to be the same as in
;; default emacs keymap except that "C-" is replaced with "M-S-" prefix.

(setq meta-bindings-map ())

;; ## Navigation
(setq meta-bindings-map 
      (append meta-bindings-map 
              '(
                ("\M-c" previous-line)
                ("\M-t" next-line)
                ("\M-n" forward-word)
                ("\M-h" backward-word)
                ("\M-\S-c" backward-paragraph)
                ("\M-\S-t" forward-paragraph)
                ((kbd "M-<up>") backward-paragraph)
                ((kbd "M-<down>") forward-paragraph)
                ("\M-\S-n" forward-char)
                ("\M-\S-h" backward-char)
                ("\M-a" move-beginning-of-line)
                ("\M-e" move-end-of-line)
                ("\M-\S-a" beginning-of-buffer)
                ("\M-\S-e" end-of-buffer)
                )))

;; ## Selection and copy, cut and paste
(setq meta-bindings-map
      (append meta-bindings-map 
              '(
                ("\M-y" yank)
                ("\M-\S-y" yank-pop)
                ;; M-w is bound by default to 'kill-ring-save
                ("\M-\S-w" kill-region)
                ("\M- " set-mark-command)
                ("\M-d" backward-kill-word)
                ("\M-\S-d" backward-delete-char-untabify)
                ;; M-i is selected to kill forward, because
                ;; it is opposite of M-d.
                ("\M-i" kill-word)
                ("\M-\S-i" delete-forward-char)
                )))

;; ## Search
(global-set-key "\M-s" 'isearch-forward)
(global-set-key "\M-r" 'isearch-backward)
(define-key isearch-mode-map "\M-s" 'isearch-repeat-forward)
(define-key isearch-mode-map "\M-r" 'isearch-repeat-backward)
(define-key isearch-mode-map "\M-g" 'isearch-abort)
(define-key isearch-mode-map "\M- " 'isearch-exit)

;; ## Windows
(defun select-next-window ()
  (interactive)
  (select-window (previous-window)))
(defun select-previous-window ()
  (interactive)
  (select-window (next-window)))
(add-to-list 'meta-bindings-map '("\M-{" select-previous-window))
(add-to-list 'meta-bindings-map '("\M-}" select-next-window))

;; ## Item selection
(add-to-list 'meta-bindings-map '("\M-." outline-next-heading))
(add-to-list 'meta-bindings-map '("\M-," outline-previous-heading))
(setq meta-bindings-diff-mode-map 
      '(
        ("\M-." diff-hunk-next)
        ("\M-," diff-hunk-prev)
        ("\M->" diff-file-next)
        ("\M-<" diff-file-prev)
        ))

;; ## Buffers
(setq meta-bindings-map 
      (append meta-bindings-map
              '(
                ("\M-b" switch-to-buffer)
                ("\M-\S-b" list-buffers)
                ("\M-\S-k" kill-buffer)
                )))

;; ## Quit/Abort
(global-set-key "\M-g" 'keyboard-quit)
(define-key minibuffer-local-map "\M-g" 'abort-recursive-edit)
(define-key minibuffer-local-map "\M- " 'minibuffer-complete-and-exit)

;; ## Other
(global-set-key "\M--" 'undo)
(global-set-key "\M-\S-s" 'save-buffer)
(global-set-key "\M-k" 'kill-line)
(global-set-key "\M-u" 'universal-argument)
(global-set-key "\M-\S-x\t" 'indent-rigidly)
(global-set-key "\M-f" 'find-file)

(defun unbind(mode-map map)
  (dolist (def map)
    (define-key mode-map (eval (car def)) nil)))
(dolist (def meta-bindings-map)
  (global-set-key (eval (car def)) (car (cdr def))))

(add-hook 'diff-mode-hook 
          (lambda () 
            (unbind diff-mode-map meta-bindings-map)
            (dolist (def meta-bindings-map-diff-mode-map)
              (define-key diff-mode-map (eval (car def)) (car (cdr def))))))
(add-hook 'markdown-mode-hook (lambda () (unbind markdown-mode-map meta-bindings-map)))
(add-hook 'nxml-mode-hook (lambda() (unbind nxml-mode-map meta-bindings-map)))
(add-hook 'dired-mode-hook (lambda() (unbind dired-mode-map meta-bindings-map)))