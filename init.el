;;; EDITOR GENERAL
(setq inhibit-startup-message t)

;; indent with 4 spaces
(setq-default indent-tabs-mode nil
              tab-width 4)

;; save buffers on close (sessioning)
(setq desktop-path '("~/"))
;(desktop-save-mode 1) ; TODO doesn't work with perspective-el? https://github.com/nex3/perspective-el

;; do not display empty cursor in other windows (especially discracting with hydras)
(setq-default cursor-in-non-selected-windows nil)

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
      (add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start maximized
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


;; I like the behaviour of evil-delete-backward-word over backward-kill-word, this is a small wrapper to make it more usable for me
(defun my-backward-kill-word ()
  "Wrapper around evil-delete-backward-word."
  (interactive)
  (if (or (bolp) (eq (current-column) (current-indentation)))
      ;(delete-indentation)
      (progn
        (evil-delete-backward-word)
        (evil-delete-backward-word))
    (evil-delete-backward-word)))

;; backwards-kill-word without copying to kill-ring
;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

;; this is mainly for letting me use C-w to delete words in vertico buffers (see general.el for hotkeys)
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))


;; (defun my-tab ()
;;   ""
;;   (interactive)
;;   )



;; helper function for adding multiple elements to a list
;; https://emacs.stackexchange.com/a/68048
(defun add-list-to-list (dst src)
  "Similar to `add-to-list', but accepts a list as 2nd argument."
  (set dst
       (append (eval dst) src)))

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
  ""
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

;; useful to have on an easily accessible key, also used a bunch with persp mode hacks
(defun open-scratch-buffer ()
  "Open *scractch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))


;; straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
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


;; HACK load general first to allow use of :general keyword
(use-package general)

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
  (setq evil-normal-state-cursor 'box)
  :config ;; tweak evil after loading it
  (evil-mode)
  ;; highlight the current line (not explicitly evil but whatever)
  (global-hl-line-mode 1))

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
  :after evil
  :config
  (evil-commentary-mode))

;; change numbers with evil
(use-package evil-numbers)

;; show evil actions
(use-package evil-goggles
  :after evil
  :init
  (setq evil-goggles-duration 0.1)
  ;; disable slow actions
  (setq evil-goggles-enable-change nil)
  (setq evil-goggles-enable-delete nil)
  :config
  (evil-goggles-mode) ;; TODO actions are not being disabled properly
  )


;;; UNDO TREE
;; undo-tree with evil mode https://www.reddit.com/r/emacs/comments/n1pibp/installed_evil_on_emacs_for_windows_redo_not/gwei7fw/
(use-package undo-tree
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))


;; vim-like handling of empty lines
(use-package vi-tilde-fringe
  :hook (prog-mode . vi-tilde-fringe-mode))


;; editor config
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; modify tab behaviour
;; TODO use this to write own tab behaviour
(use-package tab-jump-out
  :config
  (add-list-to-list 'tab-jump-out-delimiters '("(" "[" "{" "\\" "<" ">"))
  (tab-jump-out-mode))

;;----


;;; VERTICO
;; vertico - completion engine
(use-package vertico
  :init
  (vertico-mode)
  ; https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
  :general
  (:keymaps 'vertico-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous)
  :config
  (setq vertico-resize nil
        vertico-count 17
        ;; enable cycling for `vertico-next' and `vertico-previous'
        vertico-cycle t))

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
  :general
  ("M-A" 'marginalia-cycle)
  (:keymaps 'minibuffer-local-map
    "M-A" 'marginalia-cycle)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; consult
;TODO config
(use-package consult)

;; consult functions
(defun consult-find-file (DIR)
  "Open file in directory DIR."
  (interactive "DSelect dir: ")
  (let ((selection (completing-read "Find file: " (split-string (shell-command-to-string (concat "find " DIR)) "\n" t))))
    (find-file selection)))

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
(use-package flyspell)
(use-package flyspell-correct
  :after flyspell)
;(use-package flyspell-correct-popup
;  :after flyspell-correct)
(use-package spell-fu) ; TODO figure out how replacement for ispell-word


;; PROJECT
;; projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Documents/Code")))


;; treemacs
;; TODO lots of stuff here can be deleted
(use-package treemacs
  :defer t
  ;:init
  ;(with-eval-after-load 'winum
  ;  (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
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


;; WORKSPACES/PERSPECTIVES
;; my workflow is hacked on top of persp-mode.el (apparently it works better with emacsclient than perspective-el)
(use-package persp-mode
  :config
  (setq persp-nil-name "#1" ;; new default name
        persp-autokill-buffer-on-remove t
        persp-add-buffer-on-after-change-major-mode t)
  ;; start persp-mode
  (persp-mode 1))

;; a bunch of functions to hack my workflow for perspectives
(defun persp-exists (NAME)
  "Return non-nil if NAME is name of existing persp."
  (member NAME persp-names-cache))

;; thanks again doom lol
;; https://github.com/hlissner/doom-emacs/blob/master/modules/ui/workspaces/autoload/workspaces.el#L62
(defun persp-get-current-name ()
  "Wrapper to get current perspective name."
  (interactive)
  (safe-persp-name (get-current-persp)))


;; TODO refactor using index of persp-names-cache
;; create new "anonymous" perspective and switch to it
;; essentially this allows me to quickly create a new perspective without having to name it, which I find cumbersome
(defun persp-add-new-anonymous ()
  "Switch to new perspective."
  (interactive)
  ;; hacky approach - perspective names are numbers, add 1 to latest persective
  ;; and create new one with that number as the name
  (let* ((last-persp (substring (car (last persp-names-cache)) 1))
         (new-persp (concat "#" (number-to-string (+ (string-to-number last-persp) 1)))))
    (progn
      (persp-rename-all-to-index)
      (persp-switch new-persp)
      (clear-window-persp)
      (message (concat "Created and switched to persp " new-persp)))))

(defun persp-add-new-anonymous-scratch-buffer ()
  "Switch to new perspective and open *scratch* buffer."
  (interactive)
  (progn
    (persp-add-new-anonymous)
    (open-scratch-buffer)))

;; kill the perspective most recently created
(defun persp-kill-top ()
  "Kill the perspective at the top of the stack."
  (interactive)
  (if (eq (length persp-names-cache) 1)
      (message "Cannot kill last perspective")
    (let ((last-persp (cons (car (last persp-names-cache)) '()) ))
      (persp-kill last-persp))))

;; wrapper for switching perspectives, only allow switch if NAME is a valid persp
;; TODO refactor using index into persp-names-cache as parameter (or create another function)
(defun my-persp-switch (NAME)
  "Switch to the perspective with name NAME, if it exists."
  (if (persp-exists NAME)
      (if (string= (persp-get-current-name) NAME)
          (message (concat "Already on persp " NAME))
        (progn
          (persp-switch NAME)
          (message (concat "Persp: " NAME))))
    (message (concat "Invalid persp: " NAME))))

;; another wrapper for switching perspectives, except parameterizing index in persp list
(defun my-persp-switch-index (index)
  "Switch to the perspective at index INDEX of persp-names-cache, if it exists."
  (interactive "P")
  (if-let ((name (nth index persp-names-cache)))
      (if (string= name (persp-get-current-name))
          (message (concat "Already on persp " name))
        (progn
          (persp-switch name)
          (message (concat "Persp: " name))))
    (message "Invalid persp")))


;; kill all perspectives and their associated buffers other
(defun persp-kill-all-except-default ()
  "Switch to persp #1 and kill all other persps."
  (interactive)
  (if (eq (length persp-names-cache) 1)
      (message "No other perspectives to kill!")
      (let ((persps-to-kill (cdr persp-names-cache)))
        (progn
          (persp-switch persp-nil-name)
          (persp-kill persps-to-kill)
          (message (concat "Killed persps " (format "%s" persps-to-kill) ", switched to persp " persp-nil-name))))))

(defun persp-kill-all-except-current-and-default ()
  "Kill all perspectives other than the current and the default."
  (interactive)
  (let ((curr (persp-get-current-name)))
    (if (or (eq (length persp-names-cache) 1) (and (eq (length persp-names-cache) 2) (not (string= curr persp-nil-name))) )
        (message "No perspectives to kill!")
      (let ((persps-to-kill (remove curr (cdr persp-names-cache))))
        (progn
          (persp-rename-all-to-index)
          (persp-kill persps-to-kill)
          (message (concat "Killed persps " (format "%s" persps-to-kill))))))))

;; kill current perspective and switch to previous persp
(defun persp-kill-current ()
  "Kill the current perspective (unless it's the protected perspective) and switch to previous perspective."
  (interactive)
  (let ((curr (persp-get-current-name)))
    (if (string= curr persp-nil-name)
        (message (concat "Cannot kill protected perspective " curr))
      (progn
        (persp-switch-prev)
        (persp-kill curr)
        (let ((new-curr (persp-get-current-name)))
          (progn
            (persp-rename-all-to-index)
            (message (concat "Killed persp " curr ", switched to persp " new-curr))))))))

;; TODO this may require some groundwork in the package itself since persp-rename only works on the current
;; iterate through persp names, rename them all to their current index inside persp-names-cache
;; HACK can only rename current persp, so switch to each one before renaming
(defun persp-rename-all-to-index ()
  "Rename all perspectives to match their respective index in persp-names-cache."
  (interactive)
  (let ((persps (cdr persp-names-cache))
        (curr-index (cl-position (persp-get-current-name) persp-names-cache :test 'string=)))
    (progn
      (cl-loop for index from 0
               for persp in persps
               do (progn
                    (persp-switch persp)
                    (let ((new-persp (concat "#" (number-to-string (+ index 2)))))
                      (unless (string= new-persp (persp-get-current-name))
                        (persp-rename new-persp))))))
      (persp-switch (nth curr-index persp-names-cache))))


;; cycle through persps
;; https://github.com/hlissner/doom-emacs/blob/master/modules/ui/workspaces/autoload/workspaces.el#L366
(defun persp-cycle (n)
  "Cycle N perspectives to the right (default) or left."
  (interactive (list 1))
  (let* ((curr (persp-get-current-name))
         (num-persps (length persp-names-cache))
         (index (cl-position curr persp-names-cache :test 'string=))) ;; for whatever reason this sometimes fails, explicitly setting :test seems to fix it though
    (if (eq num-persps 1)
        (message "No other perspectives")
      (let* ((new-index (% (+ index n num-persps) num-persps)) ;; adding num-persps is for handling when n < 0
             (new-persp-name (nth new-index persp-names-cache)))
        (my-persp-switch new-persp-name)))))

(defun persp-switch-next ()
  "Switch to next perspective."
  (interactive)
  (persp-cycle 1))

(defun persp-switch-prev ()
  "Switch to prev perspective."
  (interactive)
  (persp-cycle -1))

;; TODO swap persps function
;; TODO buffer stack doesn't work
;; TODO display current persp in the modeline


;; hydra to quickly switch perspectives
(defhydra hydra-switch-persp (:hint nil)
  "Switch perspective"
  ;; TODO refactor for index switch, may be non-trivial with a bunch of dynamic work to do before hand (use :pre with variables??)
  ("1" (my-persp-switch-index 0) "Persp #1")
  ("2" (my-persp-switch-index 1) "Persp #2")
  ("3" (my-persp-switch-index 2) "Persp #3")
  ("4" (my-persp-switch-index 3) "Persp #4")
  ("5" (my-persp-switch-index 4) "Persp #5")
  ("6" (my-persp-switch-index 5) "Persp #6")
  ("7" (my-persp-switch-index 6) "Persp #7")
  ("8" (my-persp-switch-index 7) "Persp #8")
  ("9" (my-persp-switch-index 8) "Persp #9"))

;; TODO M-RET open file in new persp in find-file
;; TODO hack together consult preview for persp-switch-to-buffer

;;----

;; ace window
(use-package ace-window)


;; icons
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
  (load-theme 'doom-peacock t)

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
  ;; display symlink file paths https://github.com/seagle0128/doom-modeline#faq
  (setq find-file-visit-truename t)

  ;; persp-mode integration
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-display-defualt-persp-name t)

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
          ("XXX" font-lock-constant-face bold)
          ;; for temp comments or TODOs to be deleted
          ("DELETEME" error bold))))


;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (LaTeX-mode . rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))


;; highlight numbers
(use-package highlight-numbers
  :hook ((prog-mode . highlight-numbers-mode)
         (markdown-mode . highlight-numbers-mode)
         (org-mode . highlight-numbers-mode)))


;; undo-tree - undo history represented as a tree, with evil integration
(use-package undo-tree
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))


;; anzu - show number of matches of search in modeline
(use-package evil-anzu
  ;:after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :after evil
  :config
  (global-anzu-mode +1))


;; solaire mode - distinguish minibuffers from "real" buffers
(use-package solaire-mode
  ;;:init (solaire-global-mode 1)
  :config
  ;; solaire mode has issues when using the terminal
  (when (display-graphic-p)
      (solaire-global-mode +1)))


;; magit
;; TODO replicate magit-quit from doom (instead of magit bury buffer on q)
(use-package magit
  ;; refresh status when you save file being tracked in repo
  :hook (after-save . magit-after-save-refresh-status)
  ;; start magit commit in insert mode https://emacs.stackexchange.com/a/20895
  :hook (git-commit-mode . evil-insert-state)
;  :general
;  (:keymaps 'magit-mode-map
;    "q" 'magit-kill-buffers)
  :config
  ;; display magit status in current buffer (no popup) https://stackoverflow.com/a/58554387/11312409
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-auto-revert-mode t
        git-commit-summary-max-length 50))

;; TODO document
;; https://www.manueluberti.eu/emacs/2018/02/17/magit-bury-buffer/
(defun magit-kill-buffers ()
  "Restore window configuration and kill all magit buffers."
  ;(interactive)
  (let ((buffers (magit-mode-get-buffers)))
    ;(magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(defun magit-quit (&optional kill-buffer)
  ""
  (interactive "P")
  (let ((toplevel (magit-toplevel)))
    (progn
      (funcall magit-bury-buffer-function kill-buffer))
      (unless (cl-find-if (lambda (win)
                            (with-selected-window win
                              (and (derived-mode-p 'magit-mode)
                                   (equal magit--default-directory toplevel))))
                          (window-list))
        (magit-kill-buffers))))


;; show todos in magit status
(use-package magit-todos)


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
  (setq org-hide-emphasis-markers t)  ;; A default setting that needs to be t for org-appear
  (setq org-appear-autoemphasis t)    ;; Enable org-appear on emphasis (bold, italics, etc)
  (setq org-appear-autolinks nil)
  (setq org-appear-autosubmarkers t)) ;; Enable on subscript and superscript


;; deft
(use-package deft)
(setq deft-recursive t)
(setq deft-directory "~/Documents/Google/org/roam")
;;----


;; which-key
(use-package which-key
    :config
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (which-key-mode))


;; helpful
(use-package helpful
  :init
  (defvar read-symbol-positions-list nil)
  :config
  ;; redefine help keys to use helpful functions instead of vanilla
  ;; https://github.com/Wilfred/helpful#usage
  ;; TODO refactor with general
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

;; HACK for emacs 29
;; https://github.com/Wilfred/helpful/issues/282#issuecomment-1040416413
(defun helpful--autoloaded-p (sym buf)
  "Return non-nil if function SYM is autoloaded."
  (-when-let (file-name (buffer-file-name buf))
    (setq file-name (s-chop-suffix ".gz" file-name))
    (help-fns--autoloaded-p sym)))

(defun helpful--skip-advice (docstring)
  "Remove mentions of advice from DOCSTRING."
  (let* ((lines (s-lines docstring))
         (relevant-lines
          (--take-while
           (not (or (s-starts-with-p ":around advice:" it)
                    (s-starts-with-p "This function has :around advice:" it)))
           lines)))
    (s-trim (s-join "\n" relevant-lines))))

;; HYDRA
(use-package hydra)

;; window movement/management hydra
;; https://github.com/jmercouris/configuration/blob/master/.emacs.d/hydra.el#L89
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

;; functions for hydra-window
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
_k_   zoom-in
_j_   zoom-out
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
  ("4" (my-load-theme 'doom-peacock) "doom-peacock")
  ("5" (my-load-theme 'doom-gruvbox) "doom-gruvbox")
  ("6" (my-load-theme 'doom-material) "doom-material")
  ("7" (my-load-theme 'doom-palenight) "doom-palenight")
  ("8" (my-load-theme 'doom-spacegrey) "doom-spacegrey")
  ("9" (my-load-theme 'doom-molokai) "doom-molokai"))

;; functions for hydra-theme
(defun my-load-theme (theme)
  "Enhance `load-theme' by first disabling enabled themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))
;;----


;; TODO fix (maybe early init?)
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
    ; TODO add the doom hotkeys that I use
    ; TODO ordering in which-key (is it even possible?)

    ;; map universal argument to SPC-u
    "u" '(universal-argument :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")
    "SPC" '(projectile-find-file :which-key "Projectile find file")
    ;"SPC" '(consult-find-file :which-key "consult-find-file")
    "." '(find-file :which-key "Find file")
    ":" '(execute-extended-command :which-key "M-x")
    "x" '(open-scratch-buffer :which-key "Open scratch buffer")
    "<" '(consult-buffer :which-key "consult-buffer")
    "d" '(dired :which-key "dired")

    ;; editor
    "e" '(:ignore t :which-key "Editor")
    "eu" '(undo-tree-visualize :which-key "undo-tree-visualize")
    "et" '(hydra-theme/body :which-key "hydra-theme") ; not sure if this is the best place for this, perhaps toggles would be more appropriate?
    "er" '(query-replace :which-key "query-replace")

    ;; buffer
    ;"TAB" '(switch-to-prev-buffer :which-key "Prev buffer")
    "b" '(:ignore t :which-key "Buffer")
    ;"bb" ; TODO switch workspace buffer
    "bB" '(consult-buffer :which-key "consult-buffer")
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

    ;; workspaces/perspectives
    "TAB" '(:ignore t :which-key "Perspective")
    "TAB TAB" '(persp-switch :which-key "persp-switch")
    "TAB [" '(persp-switch-prev :which-key "persp-switch-prev")
    "TAB ]" '(persp-switch-next :which-key "persp-switch-next")
    "TAB n" '(persp-add-new-anonymous-scratch-buffer :which-key "persp-add-new-anonymous-scratch-buffer")
    "TAB N" '(persp-add-new-anonymous :which-key "persp-add-new-anonymous")
    "TAB k" '(persp-kill-current :which-key "persp-kill-current")
    "TAB K" '(persp-kill-all-except-default :which-key "persp-kill-all-except-default")
    "TAB h" '(hydra-switch-persp/body :which-key "hydra-switch-persp")
    ;; "TAB 1" '((lambda () (interactive) (my-persp-switch-index 0)) :which-key "Switch to #1")
    ;; "TAB 2" '((lambda () (interactive) (my-persp-switch-index 1)) :which-key "Switch to #2")
    ;; "TAB 3" '((lambda () (interactive) (my-persp-switch-index 2)) :which-key "Switch to #3")
    ;; "TAB 4" '((lambda () (interactive) (my-persp-switch-index 3)) :which-key "Switch to #4")
    ;; "TAB 5" '((lambda () (interactive) (my-persp-switch-index 4)) :which-key "Switch to #5")
    ;; "TAB 6" '((lambda () (interactive) (my-persp-switch-index 5)) :which-key "Switch to #6")
    ;; "TAB 7" '((lambda () (interactive) (my-persp-switch-index 6)) :which-key "Switch to #7")
    ;; "TAB 8" '((lambda () (interactive) (my-persp-switch-index 7)) :which-key "Switch to #8")
    ;; "TAB 9" '((lambda () (interactive) (my-persp-switch-index 8)) :which-key "Switch to #9")


    ;; git
    "g" '(:ignore t :which-key "Git") ; prefix
    "gg" '(magit-status :which-key "Git status"))

  ;; evil bindings
  ;; TODO this is a bit of a mess, I need to go through the state hierarchy to define hotkeys in highest priority
  ;; normal/visual mode hotkeys
  (general-define-key
    :states '(normal visual)
    ;; evil numbers
    "g=" 'evil-numbers/inc-at-pt
    "g-" 'evil-numbers/dec-at-pt

    ;; flyspell correct
    "z=" 'flyspell-correct-wrapper)

  ;; normal mode hotkeys
  (general-define-key
   :states 'normal
   "s" 'avy-goto-char)

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
    ;"TAB" 'tab-jump-pair

    "C-<backspace>" 'my-backward-kill-word)

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
    "TAB" #'company-complete-selection
    "<tab>" #'company-complete-selection
    ;TODO
    )

  ;; some emacs editing hotkeys inside insert mode
  (general-def
    :states '(insert)
    "C-a" 'evil-beginning-of-visual-line
    "C-e" 'evil-end-of-visual-line
    "C-n" 'evil-next-visual-line
    "C-p" 'evil-previous-visual-line)

  ;; unbind C-z from evil
  (general-unbind '(motion insert treemacs) "C-z")

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
    "C-z" 'consult-buffer

    ;; perspective management
    ;; persp cycling
    "C-<tab>" 'persp-switch-next
    "C-<iso-lefttab>" 'persp-switch-prev
    "C-S-<tab>" 'persp-switch-prev


    ;; quick perspective switching
    "M-1" (lambda () (interactive) (my-persp-switch-index 0))
    "M-2" (lambda () (interactive) (my-persp-switch-index 1))
    "M-3" (lambda () (interactive) (my-persp-switch-index 2))
    "M-4" (lambda () (interactive) (my-persp-switch-index 3))
    "M-5" (lambda () (interactive) (my-persp-switch-index 4))
    "M-6" (lambda () (interactive) (my-persp-switch-index 5))
    "M-7" (lambda () (interactive) (my-persp-switch-index 6))
    "M-8" (lambda () (interactive) (my-persp-switch-index 7))
    "M-9" (lambda () (interactive) (my-persp-switch-index 8)))

  ;; magit
  (general-define-key
    ;; https://github.com/emacs-evil/evil-magit/issues/14#issuecomment-626583736
    :keymaps 'transient-base-map
    "<escape>" 'transient-quit-one)

  ;; rebind "q" in magit-status to kill the magit buffers instead of burying them
  (general-define-key
    :states '(normal visual)
    :keymaps 'magit-mode-map
    "q" 'magit-quit)


  ;; org mode specific evil binding
  ;; unbind the return (enter) key so it becomes org-return
  ;; the return key is not that useful here anyways
  (general-define-key
    :states 'motion
    :keymaps 'org-mode-map
    :major-modes t
    "RET" 'org-return))


;;; LANGUAGES
;; lsp
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (rustic-mode . lsp)
         ;; for some reason headerline is enabled by default, add a hook to disable it
         ;; TODO look into fixing this in lsp-mode
         ;; https://emacs.stackexchange.com/a/64988
         (lsp-mode . lsp-headerline-breadcrumb-mode))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package consult-lsp)


;; flycheck
(use-package flycheck
  :init
  (global-flycheck-mode) ; TODO should this be :init or :config?
  :config
  (setq flycheck-indication-mode 'right-fringe)
  ;; change indicator to a left arrow since the fringe is now on the right
  ;; https://github.com/hlissner/doom-emacs/blob/master/modules/ui/vc-gutter/config.el#L120
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

;; flycheck-popup-tip
(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))


;; tree sitter
(use-package tree-sitter
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  ;; enable tree sitter syntax highlighting whenever possible https://emacs-tree-sitter.github.io/syntax-highlighting/
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))


;; rust
(use-package rustic
  :config
  ;; disable lsp support for now TODO
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
  ;:hook (company-mode . company-tng-mode)
  :init
  ;; https://github.com/company-mode/company-mode/blob/master/company-tng.el#L65
  (setq company-require-match nil
        company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        company-backends '(company-capf)

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))
;; TODO common highlighting (probably with company-tooltip-common face) not working

;; fuzzy autocomplete for company
(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

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
;; map the escape key to quit mini buffers instantly
;; https://www.reddit.com/r/emacs/comments/4a0421/any_reason_not_to_just_rebind_keyboardquit_from/d0wo66r/
;; TODO breaks terminal mode
;(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

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


