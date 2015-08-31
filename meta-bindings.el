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
                ("\M-\S-v" scroll-up-command)
                ("\M-l" goto-line)
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
                ("\M-\S-i" delete-char)
                )))

;; ## Search
(add-to-list 'meta-bindings-map '("\M-s" isearch-forward-regexp))
(add-to-list 'meta-bindings-map '("\M-r" isearch-backward-regexp))
(setq meta-bindings-isearch-mode-map
      '(
        ("\M-s" isearch-repeat-forward)
        ("\M-r" isearch-repeat-backward)
        ("\M-g" isearch-abort)
        ("\M- " isearch-exit)
        ("\M-w" isearch-yank-word-or-char)
        ))

;; ## Windows
(defun select-next-window ()
  (interactive)
  (select-window (previous-window)))
(defun select-previous-window ()
  (interactive)
  (select-window (next-window)))
(setq meta-bindings-map (append meta-bindings-map
                                '(
                                  ("\M-0" delete-window)
                                  ("\M-1" delete-other-windows)
                                  ("\M-2" split-window-vertically)
                                  ("\M-3" split-window-horizontally)
                                  ("\M-{" select-previous-window)
                                  ("\M-}" select-next-window)
                                  )))


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
;; Setup binding of "\M-\S-k" to say "Done" to current buffer
;; to properly close it when serving emacsclient
(setq meta-bindings-server-mode-map (make-sparse-keymap))
(define-key meta-bindings-server-mode-map "\M-\S-k" 'server-edit)
(add-to-list 'minor-mode-map-alist `(server-buffer-clients . ,meta-bindings-server-mode-map))


;; ## Quit/Abort
(global-set-key "\M-g" 'keyboard-quit)
(setq meta-bindings-minibuffer-local-map
      '(
        ("\M-g" abort-recursive-edit)
        ))

;; ## Completion
(add-to-list 'meta-bindings-map '("\M-\S-g" etags-select-find-tag-at-point))
(setq meta-bindings-etags-select-mode-map
      '(
        ("\M-g" etags-select-quit)
        ))

;; ## History
;; 
;; The reason why history navigation is bound with M-S-<up>/<down>
;; is that these binding are used for paragraph navigation.
;; When I do search, I want to adjust location of cursor a little,
;; I don't need paragraph navigation for that.
;;
;; At the same time using <up> and <down> in history navigation binding
;; is natural.
(setq meta-bindings-minibuffer-local-map
      (append meta-bindings-minibuffer-local-map
              '(
                ("\M-\S-c" previous-history-element)
                ("\M-\S-t" next-history-element)
                )))
(setq meta-bindings-isearch-mode-map
      (append meta-bindings-isearch-mode-map
              '(
                ("\M-\S-c" isearch-ring-retreat)
                ("\M-\S-t" isearch-ring-advance)
                )))

(defun meta-bindings-eshell-is-cmdline() 
  "Check if point is on eshell command line"
  (let ((p (point)))
    (beginning-of-line)
    (let ((result (looking-at eshell-prompt-regexp)))
          (goto-char p)
          result)))

(defun meta-bindings-eshell-previous-input()
  "Cycle eshell history backwards if in command line.
Go backward paragraph if not."
  (interactive)
  (if (eq last-command 'meta-bindings-eshell-previous-input)
      (setq last-command 'eshell-previous-matching-input-from-input))
  (if (meta-bindings-eshell-is-cmdline)
    (if (not (eq last-command 'backward-paragraph))
        (eshell-previous-matching-input-from-input 1)
      (setq this-command 'backward-paragraph))
    (setq this-command 'backward-paragraph)
    (backward-paragraph)))

(defun meta-bindings-eshell-next-input()
  "Cycle eshell history forward if in command line.
Go forward paragraph if not."
  (interactive)
  (if (eq last-command 'meta-bindings-eshell-next-input)
      (setq last-command 'eshell-next-matching-input-from-input))
  (if (meta-bindings-eshell-is-cmdline)
      (if (not (eq last-command 'forward-paragraph))
          (eshell-next-matching-input-from-input 1)
        (setq this-command 'forward-paragraph))
    (setq this-command 'forward-paragraph)
    (forward-paragraph)))

(setq meta-bindings-eshell-mode-map
      '(
        ("\M-\S-r" eshell-previous-matching-input)
        ("\M-\S-s" eshell-next-matching-input)
        ("\M-\S-c" meta-bindings-eshell-previous-input)
        ("\M-\S-t" meta-bindings-eshell-next-input)
        ))

;; ## Git
(setq meta-bindings-git-rebase-mode-map
      '(
        ("\M-\S-c" git-rebase-move-line-up)
        ("\M-\S-t" git-rebase-move-line-down)
        ))
(setq meta-bindings-git-commit-mode-map
      '(
        ("\M-," git-commit-prev-message)
        ("\M-." git-commit-next-message)
        ))

;; ## Shell
(setq meta-bindings-eshell-mode-map
      (append meta-bindings-eshell-mode-map
       '(
         ("\M-a" eshell-bol)
         )))

;; ## Scala
(setq meta-bindings-ensime-inf-mode-map
      '(
        ("\M-\S-c" comint-previous-input)
        ("\M-\S-t" comint-next-input)
        ))

;; ## Node.js
(setq meta-bindings-nodejs-repl-mode-map
      '(
        ("\M-\S-c" comint-previous-input)
        ("\M-\S-t" comint-next-input)
        ))

;; ## Other
(setq meta-bindings-map
      (append meta-bindings-map
              '(
                ("\M--" undo)
                ("\M-\S-s" save-buffer)
                ("\M-k" kill-line)
                ("\M-u" universal-argument)
                ("\M-\S-x\t" indent-rigidly)
                ("\M-f" find-file)
                ("\M-\\" toggle-input-method)
                ("\M-\S-f" auto-revert-tail-mode)
                )))

(defun meta-unbind(mode-map map)
  (dolist (def map)
    (define-key mode-map (eval (car def)) nil)))
(defun meta-bind(mode-map map)
  (dolist (def map)
    (define-key mode-map (eval (car def)) (car (cdr def)))))

(dolist (def meta-bindings-map)
  (global-set-key (eval (car def)) (car (cdr def))))

(meta-unbind minibuffer-local-map meta-bindings-map)
(meta-bind minibuffer-local-map meta-bindings-minibuffer-local-map)
(add-hook 'diff-mode-hook 
          (lambda () 
            (meta-unbind diff-mode-map meta-bindings-map)
            (meta-bind diff-mode-map meta-bindings-diff-mode-map)))
(add-hook 'markdown-mode-hook (lambda () (meta-unbind markdown-mode-map meta-bindings-map)))
(add-hook 'nxml-mode-hook (lambda() (meta-unbind nxml-mode-map meta-bindings-map)))
(add-hook 'dired-mode-hook (lambda() (meta-unbind dired-mode-map meta-bindings-map)))
(add-hook 'grep-mode-hook (lambda() (meta-unbind grep-mode-map meta-bindings-map)))
(add-hook 'isearch-mode-hook
          (lambda()
            (meta-unbind isearch-mode-map meta-bindings-map)
            (meta-bind isearch-mode-map meta-bindings-isearch-mode-map)))
(add-hook 'eshell-mode-hook
          (lambda() 
            (meta-unbind eshell-mode-map meta-bindings-map)
            (meta-bind eshell-mode-map meta-bindings-eshell-mode-map)))
(add-hook 'compilation-mode-hook (lambda() (meta-unbind compilation-mode-map meta-bindings-map)))
(add-hook 'vc-dir-mode-hook (lambda() (meta-unbind vc-dir-mode-map meta-bindings-map)))
(add-hook 'git-rebase-mode-hook
          (lambda()
            (meta-unbind git-rebase-mode-map meta-bindings-map)
            (meta-bind git-rebase-mode-map meta-bindings-git-rebase-mode-map)))
(add-hook 'git-commit-mode-hook
          (lambda()
            (meta-unbind git-commit-mode-map meta-bindings-map)
            (meta-bind git-commit-mode-map meta-bindings-git-commit-mode-map)))
(add-hook 'etags-select-mode-hook 
          (lambda() 
            (meta-unbind etags-select-mode-map meta-bindings-map)
            (meta-bind etags-select-mode-map meta-bindings-etags-select-mode-map)))
(add-hook 'shell-mode-hook (lambda() (meta-unbind shell-mode-map meta-bindings-map)))         
(add-hook 'ensime-inf-mode-hook
          (lambda()
            (meta-unbind ensime-inf-mode-map meta-bindings-map)
            (meta-bind ensime-inf-mode-map meta-bindings-ensime-inf-mode-map)))
(add-hook 'nodejs-repl-mode-hook
          (lambda()
            (meta-unbind nodejs-repl-mode-map meta-bindings-map)
            (meta-bind nodejs-repl-mode-map meta-bindings-ensime-inf-mode-map)))
