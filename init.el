;;; EDITOR GENERAL
(setq inhibit-startup-message t)

;; save buffers on close (sessioning)
(setq desktop-path '("~/"))
(desktop-save-mode 1)

;; remember window configuration changes
(winner-mode 1)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(setq set-fringe-mode 4)         ; I hate fringes lol
(menu-bar-mode -1)          ; Disable the menu bar

;; show trailing whitespace
(setq-default show-trailing-whitespace nil)
(defun show-trailing-whitespace-hook ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'text-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'org-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'markdown-mode-hook 'show-trailing-whitespace-hook)

;; prompt y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; font setup
(set-face-attribute 'default nil :font "Iosevka Fixed" :height 180 :weight 'light)

;; setup line numbers
;(global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; relative line numbers
(setq display-line-numbers-type 'relative)

;; line-by-line scrolling
(setq scroll-step            1
      scroll-conservatively  10000)

;; disable bells (distracting)
(setq ring-bell-function 'ignore)

;; delimeter pairing
(electric-pair-mode 1)
(setq electric-pair-delete-adjacent-pairs t)
;; TODO
(setq electric-pair-pairs '((?\" . ?\")
                            (?\{ . ?\})
                            ))

;; use aspell backend
; TODO fix for linux
(setq ispell-program-name "/opt/homebrew/bin/aspell" ; mac specific
      ispell-dictionary "english")


;; my hacky approach to formatting in insert mode
(defun my-backward-kill-word ()
  "Kill words backward my way."
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (if (string-match "^\\s-+$" (buffer-substring (point-at-bol) (point)))
        (kill-region (point-at-bol) (point))
      (backward-kill-word 1))))

(defun my-backward-kill-line ()
  "Easy formatting!"
  (interactive)
  (my-backward-kill-word)
  (backward-delete-char 1)
  ;; (c-electric-backspace 1)
  (insert " "))

(global-set-key [C-backspace] 'my-backward-kill-word)
(global-set-key (kbd "<M-backspace>") 'my-backward-kill-line)

;; backwards-kill-word without copying to kill-ring
;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; for emacs versions >= 27
(setq package-enable-at-startup nil)

;; always use straight.el when invoking use-package
(setq straight-use-package-by-default t)

;; install use-package with straight.el
(straight-use-package 'use-package)
;; always ensure packages
(setq use-package-always-ensure t)

;;;; EDITOR PACKAGES
;;; EVIL
;; evil
(use-package evil
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config ;; tweak evil after loading it
  (evil-mode))

;;; UNDO TREE
;; undo-tree with evil mode https://www.reddit.com/r/emacs/comments/n1pibp/installed_evil_on_emacs_for_windows_redo_not/gwei7fw/
(use-package undo-tree
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; evil collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; evil surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; evil org
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; evil-commentary
(use-package evil-commentary
  :hook
  (evil-mode . evil-commentary-mode))

;; vim-like handling of empty lines
(use-package vi-tilde-fringe
  :hook (prog-mode . vi-tilde-fringe-mode))
;(add-hook 'prog-mode-hook #'vi-tilde-fringe-mode)
;;----


;;; VERTICO
;; vertico - completion engine
; TODO figure out how to get C-w working in vertico minibuffer
(use-package vertico
  :init
  (vertico-mode)
  ; https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t) ; enable cycling for `vertico-next' and `vertico-previous'.

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  )

                                        ; vertico directory extension
                                        ; TODO doesn't work
;; (use-package vertico-directory
;;   :after vertico
;;   :ensure nil
;;   :bind (:map vertico-map
;;               ("RET" . vertico-directory-enter)
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; marginalia
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; consult
;TODO config
(use-package consult)
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; embark
;TODO config
(use-package embark)
(use-package embark-consult)

;;----


;; spelling
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))


;; PROJECT
;; projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/Documents/Code")))


;; treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))
;;----

;; workspaces
(use-package persp-mode)

;; themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ; fix font rendering for treemacs https://github.com/hlissner/doom-emacs/issues/1551#issuecomment-510177597
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;; modeline
(use-package doom-modeline
  ;:init (doom-modeline-mode 1) TODO delete me
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  ; https://github.com/hlissner/doom-emacs/blob/master/modules/ui/modeline/config.el
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type
        nil ; wtf? must have even number of args https://emacs.stackexchange.com/a/28288
    ))


;; highlight todos
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (markdown-mode . hl-todo-mode)
         (org-mode . hl-todo-mode))
  :config
  ; https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/hl-todo/config.el
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))


;; rainbow delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (markdown-mode . rainbow-delimiters-mode)
         (org-mode . rainbow-delimiters-mode)))

;; highlight numbers
(use-package highlight-numbers
  :hook ((prog-mode . highlight-numbers-mode)
         (markdown-mode . highlight-numbers-mode)
         (org-mode . highlight-numbers-mode)))


;; highlight quoted
; TODO not working, I don't think it's in melpa?
(use-package highlight-quoted
  :hook (emacs-lisp-mode-hook 'highlight-quoted-mode))

;; undo-tree - undo history represented as a tree, with evil integration
(use-package undo-tree
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))


;; anzu - show number of matches with a search
(use-package evil-anzu
  ;:after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :after evil
  :config
  (global-anzu-mode +1))


;; solaire mode - distinguish minibuffers from "real" buffers
(use-package solaire-mode
  :init (solaire-global-mode 1))


;; magit
(use-package magit)
(use-package magit-todos)
(use-package evil-magit)

; TODO
(use-package git-gutter-fringe)

;; org-roam 2
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/Google/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle) ; TODO hotkeys
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

; TODO org agenda install needed?
(setq org-directory "~/Documents/Google/org")
(setq org-agenda-files '("~/Documents/Google/org/roam/agenda")) ; https://stackoverflow.com/a/11384907


;; deft
(use-package deft)
(setq deft-recursive t)
;(setq deft-directory org-roam-directory)
(setq deft-directory "~/Documents/Google/org/roam")


;; which-key
(use-package which-key
    :config
    (which-key-mode))


;; helpful
(use-package helpful
  :config
  ;; redefine help keys to use helpful functions instead of vanilla
  ;; https://github.com/Wilfred/helpful#usage
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))


;; general
;; doomesque hotkeys using spacebar as prefix
(use-package general
  :config
  (general-evil-setup t)
  (defconst my-leader "SPC")
  (general-create-definer my-leader-def
    :prefix my-leader)
  (my-leader-def
    :states '(normal visual)
    ; TODO add the doom hotkeys that i use
    ;; map universal argument to SPC-u
    "u" '(universal-argument :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")
    ;"SPC" '(find-file :which-key "Find file") ; TODO change to find file in project
    "." '(find-file :which-key "Find file")

    ;; editor
    "e" '(:ignore t :which-key "Editor")
    "eu" '(undo-tree-visualize :which-key "undo-tree-visualize")

    ;; help
    "h" '(:ignore t :which-key "Help")
    "hf" '(helpful-callable :which-key "describe-function")
    "hk" '(helpful-key :which-key "describe-key")
    "hv" '(helpful-variable :which-key "describe-variable")
    "hm" '(describe-mode :which-key "describe-mode")

    ;; git
    "g" '(:ignore t :which-key "Git") ; prefix
    "gg" '(magit-status :which-key "Git status")))


;;; LANGUAGES
;; lsp
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package consult-lsp)


;; flycheck
(use-package flycheck
  :init
  (global-flycheck-mode) ; TODO should this be :init or :config?
  :config
  (setq flycheck-indication-mode 'right-fringe)) ; TODO change the direction of the arrow and look into margins instead of fringes

;; flycheck-popup-tip
(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))


;; tree sitter
(use-package tree-sitter)
(use-package tree-sitter-langs)
; enable tree sitter syntax highlighting whenever possible https://emacs-tree-sitter.github.io/syntax-highlighting/
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


;; smartparens
; TODO not working?
;(use-package smartparens
;  :diminish smartparens-mode ;; Do not show in modeline
;  :hook (prog-mode text-mode markdown-mode)
;  :init
;  (require 'smartparens-config)


;; rust
(use-package rustic)

;;----


;; PDF
(use-package pdf-tools)
(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode) ; dark mode

;;----

;; KEYBINDINGS
; more traditional zoom keys
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; remap '?' key to be for enhanced search
;(with-eval-after-load 'evil-maps
;  (define-key evil-normal-state-map (kbd "?") 'consult-line))
(evil-define-key 'normal 'global (kbd "?") 'consult-line)

;; unbind C-z from evil
(evil-define-key '(normal insert visual) 'global (kbd "C-z") 'consult-buffer)
(global-set-key (kbd "C-z") 'consult-buffer) ; TODO not sure if this is needed

;; C-w should always delete the last word unless normal mode in evil
;; this allows me to use C-w to delete words in vertico
(global-set-key (kbd "C-w") 'backward-delete-word)

;; C-v to paste (or "yank" in emacs jargon) from clipboard, useful for minibuffers (such as query-replace and M-x)
(global-set-key (kbd "C-v") 'yank)

;; buffer management
(global-set-key (kbd "C-a") 'bury-buffer)
(global-set-key (kbd "C-S-a") 'unbury-buffer)


;; map the escape key to quit mini buffers instantly
;; https://www.reddit.com/r/emacs/comments/4a0421/any_reason_not_to_just_rebind_keyboardquit_from/d0wo66r/
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; window management - undo last window action
;; taken from https://github.com/hlissner/doom-emacs/blob/develop/modules/editor/evil/config.el
; TODO another way to do this? https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#evil
(evil-define-key 'normal 'global (kbd "C-w C-u") 'winner-undo)
(evil-define-key 'normal 'global (kbd "C-w u") 'winner-undo)

;; flyspell correction hotkey in normal mode
(evil-define-key 'normal 'global (kbd "z =") 'flyspell-correct-wrapper)

;(map! (:after company
;        (:map company-active-map
;          "TAB" #'company-complete-selection
;          [tab] #'company-complete-selection))
;      :m
;      "C-a" #'bury-buffer
;      "C-S-a" #'unbury-buffer)
;(map! :m "C-z" #'buffer-menu)

;----

;; MACOS SPECIFIC CONFIGS
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)


;; TODO
;; projectile default
;; evil hotkeys
;; lang support (look into differences with elisp syntax for doom)
;; org look like doom
;; org link follow
;; flycheck things
;; workspaces https://github.com/hlissner/doom-emacs/tree/master/modules/ui/workspaces
;; git gutter
;; left fringe prettify (I think doom disables it and renders errors in the line using a popup, get that working)
;; solve errors



;; https://stackoverflow.com/a/5058752/11312409
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
