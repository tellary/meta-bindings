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
                ("\M-\S-a" beginning-of-buffer)
                ("\M-\S-e" end-of-buffer)
                ("\M-\S-v" scroll-up-command)
                ("\M-l" goto-line)
                )))
(define-key key-translation-map "\M-a" "\C-a")
(define-key key-translation-map "\M-e" "\C-e")

(setq meta-bindings-hexl-mode-map
      '(
        ("\M-\S-n" hexl-forward-char)
        ("\M-\S-h" hexl-backward-char)
        )
      )

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
        ("\M- " isearch-exit)
        ("\M-w" isearch-yank-word-or-char)
        ))
(define-key key-translation-map "\M-g" "\C-g")

;; ## Windows
(defun select-next-window ()
  (interactive)
  (let ((w (next-window nil nil t)))
    (select-window w)
    (select-frame-set-input-focus (window-frame w))))
(defun select-previous-window ()
  (interactive)
  (let ((w (previous-window nil nil t)))
    (select-window w)
    (select-frame-set-input-focus (window-frame w))))
(setq meta-bindings-map (append meta-bindings-map
                                '(
                                  ("\M-0" delete-window)
                                  ("\M-1" delete-other-windows)
                                  ("\M-2" split-window-vertically)
                                  ("\M-3" split-window-horizontally)
                                  ("\M-8" set-80-columns)
                                  ("\M-{" select-previous-window)
                                  ("\M-}" select-next-window)
                                  ((kbd "<left>") select-previous-window)
                                  ((kbd "<right>") select-next-window)
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

;; ## Code navigation and completion
(add-to-list 'meta-bindings-map '("\M-\S-g" lsp-find-definition))
(setq meta-bindings-lsp-mode-map
      '(
        ("\M-\S-f" lsp-describe-thing-at-point)
        ))
(setq meta-bindings-etags-select-mode-map
      '(
        ("\C-g" etags-select-quit)
        ))
(with-eval-after-load 'company
  (define-key company-active-map "\M-g" 'company-abort)
  (define-key company-active-map (kbd "M-t") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "M-c") 'company-select-previous-or-abort)
  (define-key company-active-map "\M-\S-f" 'company-show-doc-buffer)
  (define-key company-active-map "\M-w" 'company-show-location)
  (define-key company-active-map "\M-s" 'company-search-candidates)
  (define-key company-active-map "\M-\S-s" 'company-filter-candidates)

  (define-key company-search-map (kbd "M-t") 'company-select-next-or-abort)
  (define-key company-search-map (kbd "M-c") 'company-select-previous-or-abort)
  (define-key company-search-map (kbd "M-d") 'company-search-delete-char)
  (define-key company-search-map "\M-g" 'company-search-abort)
  (define-key company-search-map "\M-s" 'company-search-repeat-forward)
  (define-key company-search-map "\M-r" 'company-search-repeat-backward)
  (define-key company-search-map "\M-o" 'company-search-toggle-filtering)
  )

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
      '(
        ("\M-\S-c" previous-history-element)
        ("\M-\S-t" next-history-element)
        ))
(setq meta-bindings-isearch-mode-map
      (append meta-bindings-isearch-mode-map
              '(
                ("\M-\S-c" isearch-ring-retreat)
                ("\M-\S-t" isearch-ring-advance)
                )))

(defun meta-bindings-eshell-is-cmdline() 
  "Check if point is on eshell command line"
  (save-excursion
    (forward-line 0)
    (looking-at eshell-prompt-regexp)
    )
  )

(defun meta-bindings-eshell-is-last-cmdline ()
  (and (meta-bindings-eshell-is-cmdline)
       (save-excursion
         (end-of-line)
         (eobp)
         )
       )
  )

(defun meta-bindings-eshell-backward-paragraph ()
  (if (meta-bindings-eshell-is-cmdline)
      (backward-paragraph)
    )
  (backward-paragraph)
  )

(defun meta-bindings-eshell-forward-paragraph ()
  (if (and
       (meta-bindings-eshell-is-cmdline)
       (save-excursion
         (next-line)
         (not (meta-bindings-eshell-is-cmdline))
         )
       )
      (forward-paragraph)
    )
  (forward-paragraph)
  )

(defun meta-bindings-eshell-previous-input()
  "Cycle eshell history backwards if in command line.
Go backward paragraph if not."
  (interactive)
  (message (format "last-command: %s" last-command))
  (if (eq last-command 'meta-bindings-eshell-previous-input)
      (setq last-command 'eshell-previous-matching-input-from-input)
    )
  (if (meta-bindings-eshell-is-last-cmdline)
      (if (not (eq last-command 'backward-paragraph))
          (eshell-previous-matching-input-from-input 1)
        (setq this-command 'backward-paragraph)
        )
    (setq this-command 'backward-paragraph)
    (meta-bindings-eshell-backward-paragraph)
    (if (meta-bindings-eshell-is-cmdline)
        (beginning-of-line)
        )
    )
  )

(defun meta-bindings-eshell-next-input()
  "Cycle eshell history forward if in command line.
Go forward paragraph if not."
  (interactive)
  (if (eq last-command 'meta-bindings-eshell-next-input)
      (setq last-command 'eshell-next-matching-input-from-input)
    )
  (if (meta-bindings-eshell-is-last-cmdline)
      (if (not (eq last-command 'forward-paragraph))
          (eshell-next-matching-input-from-input 1)
        (setq this-command 'forward-paragraph)
        )
    (setq this-command 'forward-paragraph)
    (meta-bindings-eshell-forward-paragraph)
    (if (meta-bindings-eshell-is-cmdline)
        (end-of-line)
      )
    )
  )

(setq meta-bindings-eshell-mode-map
      '(("\M-\S-c" meta-bindings-eshell-previous-input)
        ("\M-\S-t" meta-bindings-eshell-next-input)
        ))
(setq meta-bindings-eshell-hist-mode-map
      '(("\M-\S-r" eshell-previous-matching-input)
        ("\M-\S-s" eshell-next-matching-input)
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

;; Functions to handle prev history or paragraph
(defun meta-bindings-prev-history-or-paragraph-or-prompt
    (is-prompt-f prev-history-f prev-prompt-pos-f prev-paragraph-pos-f)
  (if (funcall is-prompt-f)
      (funcall prev-history-f)
    (let ((prev-prompt-pos
           (or
            (funcall prev-prompt-pos-f)
            0))
          (prev-paragraph-pos
           (or
            (funcall prev-paragraph-pos-f)
            0)))
      (if (> prev-prompt-pos prev-paragraph-pos)
          (goto-char prev-prompt-pos)
        (goto-char prev-paragraph-pos)))))

(defun meta-bindings-next-history-or-paragraph-or-prompt
    (is-prompt-f next-history-f next-prompt-pos-f next-paragraph-pos-f)
  (if (funcall is-prompt-f)
      (funcall next-history-f)
    (let ((next-prompt-pos
           (or
            (funcall next-prompt-pos-f)
            (point-max)))
          (next-paragraph-pos
           (or
            (funcall next-paragraph-pos-f)
            (point-max))))
      (if (< next-prompt-pos next-paragraph-pos)
          (goto-char next-prompt-pos)
        (goto-char next-paragraph-pos)))))

;; ## Common for comint based modes (Scala, Python, R, Node.js)
(defun meta-bindings-comint-is-input-line ()
  (save-excursion
    (search-backward-regexp "^")
    (get-text-property (point) 'field)))

(defun meta-bindings-comint-previous-input ()
  (interactive)
  (meta-bindings-prev-history-or-paragraph-or-prompt
   'comint-after-pmark-p
   (lambda() (comint-previous-input 1))
   (lambda() (save-excursion (comint-previous-prompt 1)))
   (lambda() (save-excursion 
               (backward-paragraph)
               (if (save-excursion
                     (previous-line)
                     (meta-bindings-comint-is-input-line))
                   (progn 
                     (previous-line)
                     (comint-bol)))
               (point)))))

(defun meta-bindings-comint-next-input ()
  (interactive)
  (meta-bindings-next-history-or-paragraph-or-prompt
   'comint-after-pmark-p
   (lambda() (comint-next-input 1))
   (lambda() (save-excursion (comint-next-prompt 1)))
   (lambda() (save-excursion
               (end-of-line)
               (forward-paragraph)
               (point)))))

(setq meta-bindings-comint-mode-map
      '(
        ("\M-\S-c" meta-bindings-comint-previous-input)
        ("\M-\S-t" meta-bindings-comint-next-input)))

;; Reset paragraph settings to defaults to make 
;; `meta-bindings-comint-(next|prev)-input` work with paragraphs
;; properly. An inferior-ess-mode tries to override this.
(add-hook 'inferior-ess-mode-hook
          (lambda()
            (setq paragraph-start 
                  (car (get 'paragraph-start 'standard-value)))
            (setq paragraph-separate 
                  (car (get 'paragraph-separate 'standard-value)))))

;; ## Python
;; Disable compilation-shell-minor-mode to avoid clash
;; for `("\M-{" select-previous-window)` and `("\M-}" select-next-window)`
;; bindings
(add-hook 'compilation-shell-minor-mode-hook
          (lambda() (compilation-shell-minor-mode -1)))

;; ## Haskell
(defun meta-bindings-haskell-interactive-mode-history-previous (arg)
  (interactive "*p")
  (meta-bindings-prev-history-or-paragraph-or-prompt
   'haskell-interactive-at-prompt
   (lambda() (haskell-interactive-mode-history-previous arg))
   (lambda() (save-excursion (haskell-interactive-mode-prompt-previous)))
   (lambda() (save-excursion (backward-paragraph) (point)))))

(defun meta-bindings-haskell-interactive-mode-history-next (arg)
  (interactive "*p")
  (meta-bindings-next-history-or-paragraph-or-prompt
   'haskell-interactive-at-prompt
   (lambda() (haskell-interactive-mode-history-next arg))
   (lambda() (save-excursion (haskell-interactive-mode-prompt-next)))
   (lambda() (save-excursion (forward-paragraph) (point)))))

(setq meta-bindings-haskell-interactive-mode-map
      '(
        ("\M-\S-c" meta-bindings-haskell-interactive-mode-history-previous)
        ("\M-\S-t" meta-bindings-haskell-interactive-mode-history-next)
        ))

;; S-SPC as RET
(define-key key-translation-map (kbd "S-SPC") (kbd "RET"))
(setq meta-bindings-etags-select-mode-map
      (append meta-bindings-etags-select-mode-map
              '(
                ((kbd "RET") etags-select-goto-tag)
                )))

;; ## Other
(keymap-global-unset "M-X")
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

;; mode Agda2
(setq meta-bindings-agda2-mode-map
      '(
        ;; M-a tranlated to C-a
        ("\M-o\C-a" agda2-auto-maybe-all)
        ("\M-<"     agda2-previous-goal)
        ("\M-o\M-c" agda2-make-case)
        ("\M-o\M-d" agda2-infer-type-maybe-toplevel)
        ;; M-e tranlated to C-e
        ("\M-o\C-e" agda2-show-context)
        ("\M->"     agda2-next-goal)
        ("\M-o\M-h" agda2-helper-function-type)
        ("\M-o\M-l" agda2-load)
        ((kbd "M-o RET")  agda2-elaborate-give)
        ("\M-o\M-n" agda2-compute-normalised-maybe-toplevel)
        ("\M-o\M-o" agda2-module-contents-maybe-toplevel)
        ("\M-o\M-r" agda2-refine)
        ("\M-o\M-s" agda2-solve-maybe-all)
        ("\M-o\M-t" agda2-goal-type)
        ("\M-o\M-w" agda2-why-in-scope-maybe-toplevel)
        ("\M-o\M-z" agda2-search-about-toplevel)
        ("\M-o\M- " agda2-give)
        ("\M-o\M-," agda2-goal-and-context)
        ("\M-o\M-." agda2-goal-and-context-and-inferred)
        ("\M-o\M-;" agda2-goal-and-context-and-checked)
        ("\M-o\M-=" agda2-show-constraints)
        ("\M-o\M-?" agda2-show-goals)
        ("\M-,"     agda2-go-back)
        ("\M-."     agda2-goto-definition-keyboard)
        )
      )

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
(meta-bind Buffer-menu-mode-map meta-bindings-map)

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
            (meta-unbind eshell-hist-mode-map meta-bindings-map)
            (meta-bind eshell-mode-map meta-bindings-eshell-mode-map)
            (meta-bind eshell-hist-mode-map
                       meta-bindings-eshell-hist-mode-map)))
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
(add-hook 'lsp-mode-hook 
          (lambda() 
            (meta-unbind lsp-mode-map meta-bindings-map)
            (meta-bind lsp-mode-map meta-bindings-lsp-mode-map)))
(add-hook 'shell-mode-hook 
          (lambda() 
            (meta-unbind shell-mode-map meta-bindings-map)
            (meta-bind shell-mode-map meta-bindings-comint-mode-map)))
(add-hook 'comint-mode-hook
          (lambda()
            (meta-unbind comint-mode-map meta-bindings-map)
            (meta-bind comint-mode-map meta-bindings-comint-mode-map)))
(add-hook 'haskell-interactive-mode-hook
          (lambda()
            (meta-unbind
             haskell-interactive-mode-map
             meta-bindings-map)
            (meta-bind
             haskell-interactive-mode-map
             meta-bindings-haskell-interactive-mode-map)))
(add-hook 'Man-mode-hook
          (lambda () (meta-unbind Man-mode-map meta-bindings-map)))
(add-hook 'hexl-mode-hook
          (lambda ()
            (meta-unbind hexl-mode-map meta-bindings-map)
            (meta-bind   hexl-mode-map meta-bindings-hexl-mode-map)
            (hexl-follow-line)
            (hexl-activate-ruler)
            )
          )
(add-hook 'agda2-mode-hook
          (lambda ()
            (meta-unbind agda2-mode-map meta-bindings-map)
            (meta-bind   agda2-mode-map meta-bindings-agda2-mode-map)
            )
          )
