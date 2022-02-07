;;; EDITOR GENERAL
(setq inhibit-startup-message t)

;; indent with spaces
(setq-default indent-tabs-mode nil)

;; save buffers on close (sessioning)
(setq desktop-path '("~/"))
;(desktop-save-mode 1) ; TODO doesn't work with perspective-el? https://github.com/nex3/perspective-el

;; remember window configuration changes
(winner-mode 1)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(setq sentence-end-double-space nil) ; sentences should end with 1 space

;; fringe setup (left . right)
(set-fringe-mode '(4 . 4))

;; setup variable to determine computer specific actions
;; inpsired by https://github.com/jakebox/jake-emacs#custom-variables-registers
(defvar computer nil "Which computer I am on.")
(let ((sys (system-name)))
  (if (string= sys "arch")
      (setq computer 'linux-desktop)
    (setq computer 'mac-laptop)))


;; mac specific titlebar stuff
;; https://emacs.stackexchange.com/a/40777
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))
      (setq ns-use-proxy-icon nil)
      (setq frame-title-format nil)))


;; show trailing whitespace
(setq-default show-trailing-whitespace nil)
(defun show-trailing-whitespace-hook ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'text-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'org-mode-hook 'show-trailing-whitespace-hook)
(add-hook 'markdown-mode-hook 'show-trailing-whitespace-hook)


;; show tabs in the editor
(setq-default whitespace-style '(face tabs tab-mark))
(global-whitespace-mode 1)

;; prompt y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; font setup
(set-face-attribute 'default nil :font "Iosevka Fixed" :height 170 :weight 'light)
;; float height value (1.0) makes fixed-pitch take height 1.0 * height of default
(set-face-attribute 'fixed-pitch nil :font "Iosevka Fixed" :height 1.0 :weight 'light)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 1.0 :weight 'light)

;; setup line numbers
;; do not dynamically resize line number column when a digit needs to be added
(setq display-line-numbers-width-start t)
;; I don't want line numbers in help files, dired, etc.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)


;; automatically refresh dired on changes
;; https://www.reddit.com/r/emacs/comments/1acg6q/comment/c8w2itz/?utm_source=share&utm_medium=web2x&context=3
(add-hook 'dired-mode-hook 'auto-revert-mode)


;; TODO group defuns together
;; revert buffer without confirmation
;; http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))


;; kill all other buffers
;; https://www.emacswiki.org/emacs/KillingBuffers#h5o-2
(defun kill-other-buffers ()
     "Kill all other buffers."
     (interactive)
     (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


;; I use chemacs2 on my desktop, slightly ugly hack to open the correct file in open-init-file
(when (eq computer 'linux-desktop)
  (setq user-init-file "~/.zemacs/init.el"))

;; open init.el
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))


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
(if (eq system-type 'darwin)
  (setq ispell-program-name "/opt/homebrew/bin/aspell" ; mac specific
        ispell-dictionary "english"))

(if (eq system-type 'gnu/linux)
  (setq ispell-program-name "/usr/bin/aspell"
        ispell-dictionary "english"))



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
  ;(insert " ")
  )

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


;; toggle whether a file uses tabs or spaces
;; https://github.com/hlissner/doom-emacs/blob/master/core/autoload/text.el#L293
(defun toggle-indent-style ()
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))

;; toggle horizontal/vertical split
;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))



;; TODO
(defun tab-jump-pair (&optional ARG)
  (interactive "P")
  (let ((delims-close '(")" "]" "}"))
	(delims-open '("(" "[" "{")))
    (if (or (memq (string (following-char)) delims-open) (memq (string (following-char)) delims-close))
	(forward-char)
      (forward-whitespace 1)
      )
      ;; https://stackoverflow.com/questions/16651180/jump-to-the-first-occurrence-of-symbol-in-emacs
      ;; (eval
      ;;  `(progn
      ;;    (search-forward-regexp
      ;; 	   (rx symbol-start ,(thing-at-point 'symbol) symbol-end))
      ;; 	  (beginning-of-thing 'symbol))
      ;;   ) ;; TODO find previous occurence of close delim, find matching closed delim, jump out of it
      ))

;; https://gist.github.com/mads-hartmann/3402786?permalink_comment_id=693878#gistcomment-693878
(defun toggle-maximize-buffer ()
  "Maximize window."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))


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


;; put backup files and auto-save files in their own directory
(use-package no-littering
  :init ;; TODO is this working?
  ;; auto-saves go in another directory
  ;; https://github.com/emacscollective/no-littering#auto-save-settings
  (setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))


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
  (evil-mode)
  ;; highlight the current line (not explicitly evil but whatever)
  (global-hl-line-mode 1))

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
  (evil-org-agenda-set-keys)

  ;; TODO delete me
  ;; unbind the return (enter) key so it becomes org-return
  ;; the return key is not that useful here any
  ;; (general-define-key
  ;;   :states 'motion
  ;;   "RET" nil)
  )

;; evil-commentary
(use-package evil-commentary
  :hook
  (evil-mode . evil-commentary-mode))

;; change numbers with evil
(use-package evil-numbers)

;; show evil actions
(use-package evil-goggles
  :config
  ;(evil-goggles-mode) ;; TODO actions are not being disabled properly
  (setq evil-goggles-duration 0.1)
  ;; disable slow actions
  (setq evil-goggles-enable-change nil)
  (setq evil-goggles-enable-delete nil))

;; vim-like handling of empty lines
(use-package vi-tilde-fringe
  :hook (prog-mode . vi-tilde-fringe-mode))


;; editor config
(use-package editorconfig
  :config
  (editorconfig-mode 1))
;;----


;;; VERTICO
;; vertico - completion engine
(use-package vertico
  :init
  (vertico-mode)
  ; https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
  ;; TODO refactor with general
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t) ; enable cycling for `vertico-next' and `vertico-previous'.
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
(use-package embark
  :config
  ;; let "C-h" after a prefix command bring up a completion search using vertico
  ;; https://www.reddit.com/r/emacs/comments/otjn19/comment/h6vyx9q/?utm_source=share&utm_medium=web2x&context=3
  (setq prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult)

;;----


;; spelling
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))
;(use-package flyspell-correct-popup
;  :after flyspell-correct)
(use-package spell-fu) ; TODO figure out how replacement for ispell-word


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

;; TODO with persp-mode
;(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;  :config (treemacs-set-scope-type 'Perspectives))
;;----


;; tabs
;(use-package centaur-tabs
;  :demand
;  :config
;  (setq centaur-tabs-set-bar 'under)
;  ;; Note: If you're not using Spacmeacs, in order for the underline to display
;  ;; correctly you must add the following line:
;  (setq x-underline-at-descent-line t)
;  (centaur-tabs-mode t))

;(setq tab-bar-show nil)

;; workspaces
;; TODO
;(use-package perspective
;  :hook (kill-emacs . persp-state-save)
;  :config
;  (persp-mode))

(use-package persp-mode
  :config
  (setq persp-nil-name "#1")
  ;(setq persp-interactive-completion-function )

  ;; start persp-mode
  (persp-mode 1))

;; TODO
;; not sure if this is needed, I think persp-names-cache may be sufficient
(defvar persp-stack (car persp-names-cache))
(setq persp-stack (cons (car persp-names-cache) '()))


(defun persp-add-new-anonymous ()
  "Add a new perspective with no name."
  (interactive)
  ;; hacky approach - perspective names are numbers, add 1 to latest persective
  ;; and create new one with that number as the name
  (let* ((last-persp (substring (car (last persp-names-cache)) 1))
         (new-persp (concat "#" (number-to-string (+ (string-to-number last-persp) 1)))))
    (persp-add-new new-persp)
    (add-to-list 'persp-stack new-persp t)))


(defun persp-kill-top ()
  "Kill the perspective at the top of the stack."
  (interactive)
  (if (eq (length persp-names-cache) 1)
      (message "Cannot kill last perspective")
    (let ((last-persp (cons (car (last persp-names-cache)) '()) ))
      (persp-kill last-persp))))

(defun my-persp-switch (NAME)
  "Switch to the perspective with name NAME, if it exists."
  (if (member NAME persp-names-cache)
      (progn
        (persp-switch NAME)
        (message (concat "Persp: " NAME)))
    (message (concat "Invalid persp: " NAME))))

(defun persp-kill-all-except-default ()
  "Switch to persp #1 and kill all other persps."
  (interactive)
  (progn
    (persp-switch persp-nil-name)
    (message (concat "Persp: " persp-nil-name))
    (persp-kill (cdr persp-names-cache))))


;; TODO document functions
;; TODO M-RET open file in new persp in find-file

;; TODO hack together consult preview for persp-switch-to-buffer


;; ace window
(use-package ace-window)


;; Icons
(use-package all-the-icons
  :config
  ;; needed for getting the right side of doom-modeline to render on the screen properly
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/README.org#the-right-side-of-the-modeline-is-cut-off
  (setq all-the-icons-scale-factor 1.1))

;; themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-spacegrey t)

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
        doom-modeline-indent-info t
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type (if (eq system-type 'gnu/linux) 2 0)) ;; TODO breaks on Windows

  :config
  ;; add padding to the right side of the modeline to prevent it from getting cutoff
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/README.org#the-right-side-of-the-modeline-is-cut-off
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  "))) ; <-- added padding here


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
  :hook (LaTeX-mode . rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

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
;(use-package evil-magit)


;; minimap
(use-package minimap
  :config
  (setq minimap-window-location 'right))


; TODO
(use-package git-gutter-fringe)


;; ORG
(use-package org
  :straight nil
  ;; indent hook
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-return-follows-link t))

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

;; prettier headings
(use-package org-superstar)

;; better rendering of things like italics
(use-package org-appear
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t) ;; A default setting that needs to be t for org-appear
  (setq org-appear-autoemphasis t)  ;; Enable org-appear on emphasis (bold, italics, etc)
  (setq org-appear-autolinks nil)
  (setq org-appear-autosubmarkers t)) ;; Enable on subscript and superscript


;; deft
(use-package deft)
(setq deft-recursive t)
;(setq deft-directory org-roam-directory)
(setq deft-directory "~/Documents/Google/org/roam")
;;----


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


;; HYDRA
(use-package hydra)

;;https://github.com/jmercouris/configuration/blob/master/.emacs.d/hydra.el#L89
; window movement / management
(defhydra hydra-window (:hint nil)
   "
Movement      ^Split^            ^Switch^        ^Resize^
----------------------------------------------------------
_h_ ←           _v_ertical         _b_uffer        _M-h_ ←
_j_ ↓           _h_orizontal       _f_ind files    _M-j_ ↓
_k_ ↑           _1_only this       _P_rojectile    _M-k_ ↑
_l_ →           _d_elete           _s_wap          _M-l_ →
_F_ollow        _e_qualize         _[_backward
_q_uit          ^        ^         _]_forward
"
   ;; movement
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("[" previous-buffer)
   ("]" next-buffer)

   ;; resize
   ("M-h" hydra-move-splitter-left)
   ("M-j" hydra-move-splitter-down)
   ("M-k" hydra-move-splitter-up)
   ("M-l" hydra-move-splitter-right)

   ("b" consult-buffer)
   ("f" find-file)
   ("P" projectile-find-file)

   ("F" follow-mode)
   ("s" switch-window-then-swap-buffer)
   ("v" split-window-right)
   ("s" split-window-below)
   ("3" split-window-right)
   ("2" split-window-below)
   ("d" delete-window)
   ("1" delete-other-windows)
   ("e" balance-windows)


   ("q" nil))

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

;; buffer/frame zoom hydra
(defhydra hydra-zoom (:hint nil)
  "
Zoom
-----------------------
_=_   text-scale-increase
_-_   text-scale-decrease
_M-=_ zoom-in
_M--_ zoom-out
_k_ zoom-in
_j_ zoom-out
"
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("M-=" zoom-in)
  ("M--" zoom-out)
  ("k" zoom-in)
  ("j" zoom-out))


;; use hydra to quickly load themes
;; the themes here are (mostly)
;; https://github.com/jakebox/jake-emacs/blob/main/jake-emacs/init.org#hydra
(defhydra hydra-theme (:hint nil)
  "Switch theme"
  ("1" (my-load-theme 'doom-one) "doom-one")
  ("2" (my-load-theme 'doom-dracula) "doom-dracula")
  ("3" (my-load-theme 'doom-nord) "doom-nord")
  ("4" (my-load-theme 'doom-moonlight) "doom-moonlight")
  ("5" (my-load-theme 'doom-gruvbox) "doom-gruvbox")
  ("6" (my-load-theme 'doom-material) "doom-material")
  ("7" (my-load-theme 'doom-palenight) "doom-palenight")
  ("8" (my-load-theme 'doom-spacegrey) "doom-spacegrey")
  ("9" (my-load-theme 'doom-molokai) "doom-molokai"))

(defun my-load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))
;;----


;; general keybindings
;; doomesque hotkeys using spacebar as prefix
(use-package general
  :config
  (general-evil-setup t)
  (defconst my-leader "SPC")
  (general-create-definer my-leader-def
    :prefix my-leader)
  (general-override-mode) ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335
  (my-leader-def
    :states '(motion normal visual treemacs) ;; note the treemacs state
    :keymaps 'override ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335
    ; TODO add the doom hotkeys that i use
    ; TODO ordering in which-key (is it even possible?)

    ;; map universal argument to SPC-u
    "u" '(universal-argument :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")
    "SPC" '(projectile-find-file :which-key "Projectile find file")
    "." '(find-file :which-key "Find file")
    ":" '(execute-extended-command :which-key "M-x")

    ;; editor
    "e" '(:ignore t :which-key "Editor")
    "eu" '(undo-tree-visualize :which-key "undo-tree-visualize")
    "et" '(hydra-theme/body :which-key "hydra-theme") ; not sure if this is the best place for this, perhaps toggles would be more appropriate?

    ;; buffer
    ;"TAB" '(switch-to-prev-buffer :which-key "Prev buffer")
    "b" '(:ignore t :which-key "Buffer")
    ;"bb" ; TODO switch workspace buffer
    "bB" '(consult-buffer :which-key "consult-buffer") ; TODO map to SPC-<
    "b[" '(previous-buffer :which-key "Previous buffer")
    "b]" '(next-buffer :which-key "Next buffer")
    "bd" '(kill-current-buffer :which-key "Kill buffer")
    "bk" '(kill-current-buffer :which-key "Kill buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
    "br" '(revert-buffer-no-confirm :which-key "Revert buffer")
    "bK" '(kill-other-buffers :which-key "Kill other buffers")

    ;; open
    "o" '(:ignore t :which-key "Open")
    "op" '(treemacs :which-key "Treemacs")
    "oP" '(treemacs-find-file :which-key "Treemacs find file")
    "oc" '(open-init-file :which-key "Open init.el")

    ;; project
    "p" '(:ignore t :which-key "Project")
    "pp" '(projectile-switch-project :which-key "Switch Project")

    ;; help
    "h" '(:ignore t :which-key "Help")
    "hf" '(helpful-callable :which-key "describe-function")
    "hk" '(helpful-key :which-key "describe-key")
    "hv" '(helpful-variable :which-key "describe-variable")
    "hm" '(describe-mode :which-key "describe-mode")
    "hF" '(describe-face :which-key "describe-face")

    ;; zoom
    ;; the hydra is nice but the rest is kind of jank, need to pla around with this more
    "=" '(text-scale-increase :which-key "text-scale-increase")
    "-" '(text-scale-decrease :which-key "text-scale-decrease")
    "z" '(:ignore t :which-key "zoom")
    "z=" '(zoom-in :which-key "zoom-in")
    "z-" '(zoom-out :which-key "zoom-out")
    "zz" '(hydra-zoom/body :which-key "hydra-zoom")

    ;; window
    "w" '(:ignore t :which-key "Window")
    "ww" '(hydra-window/body :which-key "hydra-window")
    "wg" '(golden-ratio :which-key "golden-ratio")
    "wt" '(toggle-window-split :which-key "toggle-window-split")
    "wa" '(ace-window :which-key "ace-window")
    "wf" '(toggle-maximize-buffer :which-key "toggle-maximize-buffer")
    ; TODO more window stuff

    ;; toggles
    "t" '(:ignore t :which-key "Toggles")
    "ts" '(flyspell-mode :which-key "flyspell-mode")
    "tc" '(flycheck-mode :which-key "flycheck-mode")
    "tC" '(company-mode :which-key "company-mode") ; not sure how good of a hotkey this is lol
    "tm" '(minimap-mode :which-key "minimap-mode")
    "tg" '(golden-ratio-mode :which-key "golden-ratio-mode")
    "tg" '(evil-goggles-mode :which-key "evil-goggles")
    "tI" '(toggle-indent-style :which-key "Indent style")

    ;; notes
    "n" '(:ignore t :which-key "Notes")
    ;"nr" '(:ignore t :which-key "org-roam")
    "nf" '(org-roam-node-find :which-key "find-node")
    "ni" '(org-roam-node-insert :which-key "insert-node")
    "nt" '(org-roam-dailies-goto-today :which-key "org-roam-dailies-goto-today")

    ;; git
    "g" '(:ignore t :which-key "Git") ; prefix
    "gg" '(magit-status :which-key "Git status"))

  ;; normal/visual mode hotkeys
  (general-define-key
    :states '(normal visual)
    ;; evil numbers
    "g=" 'evil-numbers/inc-at-pt
    "g-" 'evil-numbers/dec-at-pt

    ;; flyspell correct
    "z=" 'flyspell-correct-wrapper)

  ;; visual mode hotkeys
  (general-define-key
    :states 'visual
    ;; also bound to "S", "s" and "c" are the same in visual mode anyways
    "s" 'evil-surround-region)

  ;; insert mode hotkeys
  (general-define-key
    :states 'insert
    "C-SPC" 'company-complete
    "C-v" 'yank ;; C-v should paste clipboard contents
    "TAB" 'tab-jump-pair)


  ;; motion mode hotkeys, inherited by normal/visual
  (general-define-key
    :states 'motion
    "?" 'consult-line

    ;; window management
    "C-w C-u" 'winner-undo
    "C-w u" 'winner-undo
    "C-w a" 'ace-window)

  ;; company
  (general-define-key
    :keymaps '(company-active-map)
    "C-w" nil ; allow C-w to act normally during completion
    "C-h" nil
    "C-n" #'company-select-next
    "C-p" #'company-select-previous
    "<tab>" #'company-complete-selection
    ;TODO
    )

  ;; some emacs hotkeys inside insert mode
  (general-def
    :states '(insert)
    "C-a" 'evil-beginning-of-visual-line
    "C-e" 'evil-end-of-visual-line
    "C-n" 'evil-next-visual-line
    "C-p" 'evil-previous-visual-line)

  ;; key bindings for evil search ('/')
  ;; there could be a better way to do this, but this works so whatever
  (general-define-key
    ;; note that evil-ex-map is different from evil-ex-search-keymap
    :keymaps 'evil-ex-search-keymap
    ;; C-v should paste clipboard contents
    "C-v" 'yank)

  ;; global
  (general-define-key
    ;; more traditional zoom keys
    "C-=" 'text-scale-increase
    "C--" 'text-scale-decrease
    "C-M-=" 'zoom-in
    "C-M--" 'zoom-out

    ;; C-w should always delete the last word unless normal mode in evil
    ;; this allows me to use C-w to delete words in vertico
    "C-w" 'backward-delete-word

     ;; C-v to paste (or "yank" in emacs jargon) from clipboard, useful for minibuffers (such as query-replace and M-x)
    "C-v" 'yank

    ;; buffer management
    "C-a" 'bury-buffer
    "C-S-a" 'unbury-buffer

    ;; quick perspective switching
    "M-1" (lambda () (interactive) (my-persp-switch "#1"))
    "M-2" (lambda () (interactive) (my-persp-switch "#2"))
    "M-3" (lambda () (interactive) (my-persp-switch "#3"))
    "M-4" (lambda () (interactive) (my-persp-switch "#4"))
    "M-5" (lambda () (interactive) (my-persp-switch "#5"))
    "M-6" (lambda () (interactive) (my-persp-switch "#6"))
    "M-7" (lambda () (interactive) (my-persp-switch "#7"))
    "M-8" (lambda () (interactive) (my-persp-switch "#8"))
    "M-9" (lambda () (interactive) (my-persp-switch "#9")))

  ;; magit
  (general-define-key
    ;; https://github.com/emacs-evil/evil-magit/issues/14#issuecomment-626583736
    :keymaps 'transient-base-map
    "<escape>" 'transient-quit-one)
  ;(general-define-key
  ;  :keymaps 'magit-mode-map
  ;  ;; escape key should quit buffer
  ;  "ESC" nil)


  ;; org mode specific evil
  ;; unbind the return (enter) key so it becomes org-return
  ;; the return key is not that useful here any
  ;; TODO this doesn't work
  (general-define-key
    :major-modes 'org-mode
    :states 'motion
    :keymaps 'local ;; only in the buffer
    "RET" nil))


;;; LANGUAGES
;; lsp
;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((lsp-mode . lsp-enable-which-key-integration)
;; 	 ;(rustic-mode . lsp)
;;          ;; for some reason headerline is enabled by default, add a hook to disable it
;;          ;; TODO look into fixing this in lsp-mode
;;          ;; https://emacs.stackexchange.com/a/64988
;;          (lsp-mode . lsp-headerline-breadcrumb-mode))
;;   :commands lsp
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil))

;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; (use-package consult-lsp)


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
;; enable tree sitter syntax highlighting whenever possible https://emacs-tree-sitter.github.io/syntax-highlighting/
; TODO use-package refactor
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
(use-package rustic
  :config
  ;; disable lsp support for now
  (setq rustic-lsp-client nil))


;; latex
(use-package auctex
  :hook (LaTeX-mode . visual-line-mode)
  ;; electric pair mode for $ https://tex.stackexchange.com/a/75884
  :hook (LaTeX-mode . (lambda ()
                       (define-key LaTeX-mode-map (kbd "$") 'self-insert-command))))
(use-package latex-preview-pane)
(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))


;; thrift
(use-package thrift)

;;----

;; AUTOCOMPLETE
;; TODO cycle completion options
;; company
(use-package company
  ;; trying out tab and go mode
  :hook (company-mode . company-tng-mode)
  :config
  ;; https://github.com/company-mode/company-mode/blob/master/company-tng.el#L65
  (setq company-require-match nil))

;; fuzzy autocomplete
;(use-package company-fuzzy
;  :hook (company-mode . company-fuzzy-mode)
;  :init
;  (setq company-fuzzy-sorting-backend 'flx
;        company-fuzzy-prefix-on-top nil
;        company-fuzzy-history-backends '(company-yasnippet)
;        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))

;;----



;; change font size in all buffers (frame)
(use-package zoom-frm
  :config
  ;; more granular zooming
  (setq text-scale-mode-step 1.1))



;; adaptive text wrap
;; taken from https://github.com/hlissner/doom-emacs/blob/master/modules/lang/latex/config.el
(use-package adaptive-wrap
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode) ; work well with visual-line-mode
  :init (setq-default adaptive-wrap-extra-indent 0))


;; PDF
(use-package pdf-tools
  :config
  (pdf-tools-install)
  ; HiDPI support
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)
  ; TODO refactor with :hook
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode) ; dark mode ; TODO I think this is broken?
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode) ; dark mode

  ;; https://www.reddit.com/r/emacs/comments/dgywoo/issue_with_pdfview_midnight_mode/f3hdhf2/
  ;; disable cursor in pdf-view, otherwise there is a distracting border on the pdf
  (add-hook 'pdf-view-mode-hook
    (lambda ()
      (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))
(use-package saveplace-pdf-view
  :after pdf-view)
;;----


;; ranger enhancement for dired
(use-package ranger)


;; golden ratio windows
;; TODO this does not work with evil window switching
(use-package golden-ratio
  :config
  (setq golden-ratio-auto-scale t))



;; KEYBINDINGS
;; more traditional zoom keys
;(global-set-key (kbd "C-=") 'text-scale-increase)
;(global-set-key (kbd "C--") 'text-scale-decrease)
;(global-set-key (kbd "C-M-=") 'zoom-in)
;(global-set-key (kbd "C-M--") 'zoom-out)

;; remap '?' key to be for enhanced search
;(with-eval-after-load 'evil-maps
;  (define-key evil-normal-state-map (kbd "?") 'consult-line))
;; TODO I don't think this should affect dired?
;(evil-define-key 'normal 'global (kbd "?") 'consult-line)

;; unbind C-z from evil
(evil-define-key '(normal insert visual) 'global (kbd "C-z") 'consult-buffer)
(global-set-key (kbd "C-z") 'consult-buffer) ; TODO not sure if this is needed

;; C-w should always delete the last word unless normal mode in evil
;; this allows me to use C-w to delete words in vertico
;(global-set-key (kbd "C-w") 'backward-delete-word)

;; C-v to paste (or "yank" in emacs jargon) from clipboard, useful for minibuffers (such as query-replace and M-x)
;(global-set-key (kbd "C-v") 'yank)

;; buffer management
;(global-set-key (kbd "C-a") 'bury-buffer)
;(global-set-key (kbd "C-S-a") 'unbury-buffer)


;; map the escape key to quit mini buffers instantly
;; https://www.reddit.com/r/emacs/comments/4a0421/any_reason_not_to_just_rebind_keyboardquit_from/d0wo66r/
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; window management - undo last window action
;; taken from https://github.com/hlissner/doom-emacs/blob/develop/modules/editor/evil/config.el
; TODO another way to do this? https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#evil
;(evil-define-key 'normal 'global (kbd "C-w C-u") 'winner-undo)
;(evil-define-key 'normal 'global (kbd "C-w u") 'winner-undo)

;; flyspell correction hotkey in normal mode
;(evil-define-key 'normal 'global (kbd "z =") 'flyspell-correct-wrapper)

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

;; fix path related issues to allow emacs to easily access tools like rg
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))


;; TODO
;; projectile default
;; evil hotkeys
;; lang support (look into differences with elisp syntax for doom)
;; org link follow
;; flycheck things
;; workspaces https://github.com/hlissner/doom-emacs/tree/master/modules/ui/workspaces
;; left fringe prettify (I think doom disables it and renders errors in the line using a popup, get that working)
;; solve errors



;; https://stackoverflow.com/a/5058752/11312409
;(setq custom-file "~/.emacs.d/custom.el")
;(load custom-file)

