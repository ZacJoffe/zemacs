;; TODO early init setup https://www.reddit.com/r/emacs/comments/enmbv4/earlyinitel_reduce_init_time_about_02_sec_and/

;;; EDITOR GENERAL
(setq inhibit-startup-message t)

;; some editor settings
(setq-default indent-tabs-mode nil ;; indent with spaces
              tab-width 4          ;; 1 tab <=> 4 spaces
              c-basic-offset 4     ;; indentation for cc modes
              fill-column 80       ;; wrap at 80 characters for auto-fill-mode
              word-wrap t          ;; do not wrap characters in the middle of words
              truncate-lines t)    ;; do not wrap by default

;; save buffers on close (sessioning)
(setq desktop-path '("~/"))
;(desktop-save-mode 1) ; TODO doesn't work with perspective-el? https://github.com/nex3/perspective-el

;; do not display empty cursor in other windows (especially discracting with hydras)
(setq-default cursor-in-non-selected-windows nil)

;; remember window configuration changes
(winner-mode 1)

;; automatically refresh buffers
(global-auto-revert-mode 1)

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
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 1.0 :weight 'light)

;; setup line numbers
;; do not dynamically resize line number column when a digit needs to be added
(setq display-line-numbers-width-start t)
;; I don't want line numbers in help files, dired, etc.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)


;; I use chemacs2 on my desktop, slightly ugly hack to open the correct file in open-init-file
(when (eq computer 'linux-desktop)
  (setq user-init-file "~/.zemacs/init.el"))


;; relative line numbers
(setq display-line-numbers-type 'relative)


;; line-by-line scrolling
(setq scroll-step            1
      scroll-conservatively  10000)
(pixel-scroll-precision-mode 1)

;; disable bells (distracting)
(setq ring-bell-function 'ignore)

;; delimeter pairing
(electric-pair-mode 1)
(setq electric-pair-delete-adjacent-pairs t)
;; TODO
(setq electric-pair-pairs '((?\" . ?\")
                            (?\{ . ?\})
                            ))

;; prevent electric pair mode from being enabled in the mini buffer (for things like consult)
;; https://emacs.stackexchange.com/a/29342
(setq electric-pair-inhibit-predicate (lambda (char) (minibufferp)))

;; use aspell backend
(if (eq system-type 'darwin)
  (setq ispell-program-name "/opt/homebrew/bin/aspell" ; mac specific
        ispell-dictionary "english"))

(if (eq system-type 'gnu/linux)
  (setq ispell-program-name "/usr/bin/aspell"
        ispell-dictionary "english"))

;;;; DEFUNS
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

;; open init.el
(defun open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))


;; inspired by https://owoga.com/how-to-zap-whitespace-in-emacs/
(defun delete-whitespace-left-of-cursor ()
  "Delete all whitespace to the left of cursor's current position."
  (interactive)
  (let ((skip-chars "\t\n\r ")
        (old-point (point)))
    (skip-chars-backward skip-chars)
    (let ((start (point)))
      (delete-region start old-point))))

;; I like the behaviour of evil-delete-backward-word over backward-kill-word, this is a small wrapper to make it more usable for me
(defun my-backward-kill-word ()
  "Wrapper around evil-delete-backward-word."
  (interactive)
  (if (or (bolp) (eq (current-column) (current-indentation)))
      (delete-whitespace-left-of-cursor)
    (evil-delete-backward-word)))

;; basically the same as my-backward-kill-word except it creates a space when merging lines
;; TODO repeated code, this should likely be merged into my-backward-kill-word
(defun my-backward-kill-line ()
  "Same as my-backward-kill-word but inserts a space after merging lines"
  (interactive)
  (if (or (bolp) (eq (current-column) (current-indentation)))
      (progn
        (my-backward-kill-word)
        (insert " "))
    (evil-delete-backward-word)))


;; backward-kill-word without copying to kill-ring
;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

;; this is mainly for allowing me to use C-w to delete words in vertico buffers (see general.el for hotkeys)
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
;;----


;; ADVICE/HOOKS
(defun +tab/jump-out (oldfun &rest args)
  "Forward-char if next-char is a delimiter, otherwise call OLDFUN with ARGS."
  (let ((delimiters '("(" ")" "[" "]" "{" "}" "\\" "<" ">" ";" "|" "`" "'" "\""))
        (next-char (string (char-after))))
    (if (member next-char delimiters)
        (forward-char)
      (apply oldfun args))))

(advice-add 'indent-for-tab-command :around #'+tab/jump-out)
(advice-add 'org-cycle :around #'+tab/jump-out)
;;----


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
  (setq evil-search-module 'evil-search
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t
        evil-want-integration t
        evil-want-keybinding nil
        evil-normal-state-cursor 'box
        evil-ex-search-vim-style-regexp t)
  :config
  ;; highlight the current line (not explicitly evil but whatever)
  (global-hl-line-mode 1)

  ;; HACK prevent evil from moving window location when splitting by forcing a recenter
  ;; also do not switch to new buffer
  (advice-add 'evil-window-vsplit :after (lambda (&rest r) (progn (evil-window-prev 1) (recenter))))
  (advice-add 'evil-window-split :after (lambda (&rest r) (progn (evil-window-prev 1) (recenter))))

  (evil-mode))

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
  (evil-goggles-mode))


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
(use-package consult)

;; integration with flycheck
(use-package consult-flycheck
  :after (consult flycheck))

;; consult functions
(defun consult-find-file (DIR)
  "Open file in directory DIR."
  (interactive "DSelect dir: ")
  (let ((selection (completing-read "Find file: " (split-string (shell-command-to-string (concat "find " DIR)) "\n" t))))
    (find-file selection)))

(defun +consult/ripgrep (DIR)
  "Ripgrep directory DIR."
  (interactive "DSelect dir: ")
  (consult-ripgrep DIR))

;; embark
(use-package embark
  :general
  ("C-l" 'embark-act) ;; TODO C-. not working
  :after which-key ; TODO this is here so I can disable which-key C-h, unsure if it's needed
  :config
  ;; let "C-h" after a prefix command bring up a completion search using consult
  ;; https://www.reddit.com/r/emacs/comments/otjn19/comment/h6vyx9q/?utm_source=share&utm_medium=web2x&context=3
  (setq prefix-help-command #'embark-prefix-help-command
        which-key-use-C-h-commands nil))

(use-package embark-consult
  :after (embark consult))
;;----


;; spelling
(use-package flyspell
  ;; turn on flyspell for magit commit
  :hook (git-commit-setup . git-commit-turn-on-flyspell))

;; spelling correction menu using completing-read (so consult)
(use-package flyspell-correct
  :after flyspell)


;; PROJECT
;; projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/Documents/Code")))


;; treemacs
(use-package treemacs
  :config
  (setq treemacs-follow-after-init t))

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
;; WIP perspective-el migration
;; TODO buffers
;; TODO save persps?
;; TODO emacs server integration
;; TODO winner mode support https://github.com/nex3/perspective-el/issues/137
(use-package perspective
  :config
  ;; TODO change this to "main", create new perspectives based on index
  (setq persp-initial-frame-name "1") ;; "main" perspective
  (persp-mode))

;; https://github.com/hlissner/doom-emacs/blob/master/modules/ui/workspaces/autoload/workspaces.el#L442-L454
(defun +persp--tabline ()
  "Build string containing persp list, with the current persp highlighted."
  (let ((names (persp-names))
        (curr (persp-current-name)))
    (mapconcat
     #'identity
     (cl-loop for index from 0
              for name in names
              collect
              (propertize (format " [%d] %s " (1+ index) name)
                          'face (if (string= curr name)
                                    'highlight
                                  'default)))
     " ")))

(defun +persp--message (msg &optional face-type)
  ""
  (concat (+persp--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" msg)
                      'face face-type))) ;; TODO check for face-type

(defun +persp-message (msg &optional face-type)
  ""
  (message "%s" (+persp--message msg face-type)))

(defun +persp/display ()
  ""
  (interactive)
  (message "%s" (+persp--tabline)))

;; using this naming convention ("+" prefix) to avoid namespace collision during migration
(defun +persp/add-new ()
  "Switch to a new perspective and open scratch buffer."
  (interactive)
  (let ((num-persps (length (persp-names))))
    (persp-switch (number-to-string (+ num-persps 1)))
    (+persp/display)))

;; TODO test this more
(defun +persp/add-new-import-buffer ()
  "Switch to a new perspective, copying over currently selected buffer."
  (interactive)
  (let ((curr-buffer (current-buffer)))
    (+persp/add-new)
    (persp-add-buffer curr-buffer)
    (persp-switch-to-buffer* curr-buffer)))

(defun +persp/kill-top ()
  "Kill the top perspective."
  (interactive)
  (if (eq (length (persp-names)) 1)
      (+persp-message (format "Cannot kill protected persp %s" (persp-current-name)) 'error)
    (let ((last-persp (car (last (persp-names)))))
      (persp-kill last-persp)
      (+persp-message (format "Killed persp %s" last-persp) 'success))))

;; NOTE you can kill default perspective, but I don't see a reason to do that
;; so for now I am going to prevent that through these wrappers
(defun +persp/kill-current ()
  ""
  (interactive)
  (let ((curr (persp-current-name)))
    (if (string= curr persp-initial-frame-name)
        (+persp-message (format "Cannot kill protected persp %s" curr) 'error)
      (persp-prev)
      (persp-kill curr)
      (let ((new-curr (persp-current-name)))
        ;; TODO shift names?
        (+persp-message (format "Killed persp %s" curr) 'success)))))

;; TODO refactor to explicitly switch to (car (persp-names))
(defun +persp/kill-all-except-default ()
  ""
  (interactive)
  (if (eq (length (persp-names)) 1)
      (+persp-message "No persps to kill!" 'warning)
    (let ((persps-to-kill (cdr (persp-names))))
      (mapc (lambda (persp-name) (persp-kill persp-name)) persps-to-kill)
      (if (eq (length persps-to-kill) 1)
          (+persp-message (format "Killed persp %s" (nth 0 persps-to-kill)) 'success)
        (+persp-message (format "Killed persps %s" (mapconcat #'identity persps-to-kill ", ")) 'success)))))


(defun +persp/switch-by-index (index)
  "Switch to perspective at index INDEX, if it exists."
  (interactive "P")
  (if-let* ((persps (persp-names))
            (name (nth index persps)))
      (if (string= name (persp-current-name))
          (+persp-message (concat "Already in " name) 'warning)
        (persp-switch name)
        (+persp/display))
    (+persp-message (format "Invalid persp index %d" (1+ index)) 'error)))

;(autoload 'consult--multi "consult") TODO needed?
;; TODO needs work
(defun +persp--consult-buffer-sources ()
  ""
  (list `(:name ,(persp-current-name)
    :narrow ?b
    :category buffer
    :state    ,#'consult--buffer-state
    :items ,(lambda ()
              (consult--buffer-query
               :sort 'visibility
               :as #'buffer-name
               :predicate (lambda (buf)
                            (member buf (persp-get-buffers))))))))

;; this function is essentially a copy of consult-buffer
(defun +persp/consult-buffer ()
  ""
  (interactive)
  (when-let (buffer (consult--multi (+persp--consult-buffer-sources)
                                    :require-match
                                    (confirm-nonexistent-file-or-buffer)
                                    :prompt "Switch to: "
                                    :history 'consult--buffer-history
                                    :sort nil))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (unless (cdr buffer)
      (consult--buffer-action (car buffer)))))

;; TODO how does doom handle the bury buffer with persp-mode?

;; persp display when cycling through perspectives
(defun +persp--cycle (oldfun)
  "Persp display on persp cycling"
  (if (eq (length (persp-names)) 1)
      (+persp-message "No other persps" 'warning)
    (funcall oldfun)
    (+persp/display)))

(advice-add 'persp-prev :around #'+persp/cycle)
(advice-add 'persp-next :around #'+persp/cycle)


;; OLD

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

;; TODO swap persps function
;; TODO buffer stack doesn't work
;; TODO display current persp in the modeline


;; hydra to quickly switch perspectives
(defhydra hydra-switch-persp (:hint nil)
  "Switch perspective"
  ;; TODO refactor for index switch, may be non-trivial with a bunch of dynamic work to do before hand (use :pre with variables??)
  ("1" (+persp/switch-by-index 0) "Persp #1")
  ("2" (+persp/switch-by-index 1) "Persp #2")
  ("3" (+persp/switch-by-index 2) "Persp #3")
  ("4" (+persp/switch-by-index 3) "Persp #4")
  ("5" (+persp/switch-by-index 4) "Persp #5")
  ("6" (+persp/switch-by-index 5) "Persp #6")
  ("7" (+persp/switch-by-index 6) "Persp #7")
  ("8" (+persp/switch-by-index 7) "Persp #8")
  ("9" (+persp/switch-by-index 8) "Persp #9"))

;; TODO M-RET open file in new persp in find-file
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
        doom-modeline-buffer-file-name-style 'truncate-nil
        doom-modeline-indent-info nil
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type (if (eq system-type 'gnu/linux) 2 0)) ;; TODO breaks on Windows

  :config
  ;; display symlink file paths https://github.com/seagle0128/doom-modeline#faq
  (setq find-file-visit-truename t)

  ;; doom uses the default modeline that is defined here: https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline.el#L90
  ;; as far as I can tell you can't change the ordering of segments without redefining the modeline entirely (segments can be toggled though)
  ;;
  ;; this is a bit messy since I already set a bunch of toggles, TODO need to work on this
  (doom-modeline-def-modeline 'main
    '(bar modals matches buffer-info buffer-position selection-info)
    '(input-method buffer-encoding lsp major-mode process vcs checker " " misc-info "  ")))



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
          ("DELETEME" error bold)
          ("KILLME" error bold)
          ;; for works in progress
          ("WIP" font-lock-keyword-face bold))))


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
;; TODO see how doom does this
(defun magit-kill-buffers ()
  "Restore window configuration and kill all magit buffers."
  ;(interactive)
  (let ((buffers (magit-mode-get-buffers)))
    ;(magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

;; TODO document
;; TODO also kill diff buffer
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

(use-package git-gutter
  :init
  (global-git-gutter-mode 1))

(use-package git-gutter-fringe
  :config
  ;; pretty diff indicators
  ;; https://github.com/hlissner/doom-emacs/blob/master/modules/ui/vc-gutter/config.el#L106
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


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
;; TODO figure out how to reuse window for recursive helps
(use-package helpful
  :init
  (defvar read-symbol-positions-list nil)
  :config
  ;; redefine help keys to use helpful functions instead of vanilla
  ;; https://github.com/Wilfred/helpful#usage
  :general ;; global
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key))

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
;; TODO hotkeys are not displayed correctly
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
   ("r" switch-window-then-swap-buffer)
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
;;; GENERAL.EL

;; https://github.com/hlissner/doom-emacs/blob/master/modules/config/default/config.el#L6
(defvar default-minibuffer-maps
  (append '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map
            read-expression-map))
  "A list of all the keymaps used for the minibuffer.")

;; general keybindings
(use-package general
  :config
  (general-evil-setup t)
  (defconst my-leader "SPC")
  (general-create-definer my-leader-def
    :prefix my-leader)
  (general-override-mode) ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335
  ;; doomesque hotkeys using spacebar as prefix
  (my-leader-def
    :states '(motion normal visual treemacs) ;; note the treemacs state
    :keymaps 'override ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335
    ; TODO ordering in which-key (is it even possible?)

    ;; map universal argument to SPC-u
    "u" '(universal-argument :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")
    "SPC" '(projectile-find-file :which-key "Projectile find file")
    ;"SPC" '(consult-find-file :which-key "consult-find-file")
    "." '(find-file :which-key "Find file")
    "," '(+persp/consult-buffer :which-key "+persp/consult-buffer")
    ":" '(execute-extended-command :which-key "M-x")
    "x" '(open-scratch-buffer :which-key "Open scratch buffer")
    "<" '(consult-buffer :which-key "consult-buffer")
    "d" '(dired :which-key "dired")
    "/" '(+consult/ripgrep :which-key "+consult/ripgrep")

    ;; editor
    "e" '(:ignore t :which-key "Editor")
    "eu" '(undo-tree-visualize :which-key "undo-tree-visualize")
    "et" '(hydra-theme/body :which-key "hydra-theme") ; not sure if this is the best place for this, perhaps toggles would be more appropriate?
    "er" '(query-replace :which-key "query-replace")
    "ec" '(consult-theme :which-key "consult-theme")

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
    "hw" '(where-is :which-key "where-is")

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
    "tg" '(evil-goggles-mode :which-key "evil-goggles")
    "tI" '(toggle-indent-style :which-key "Indent style")
    "tv" '(visual-line-mode :which-key "visual-line-mode")

    ;; notes
    "n" '(:ignore t :which-key "Notes")
    ;"nr" '(:ignore t :which-key "org-roam")
    "nf" '(org-roam-node-find :which-key "find-node")
    "ni" '(org-roam-node-insert :which-key "insert-node")
    "nt" '(org-roam-dailies-goto-today :which-key "org-roam-dailies-goto-today")

    ;; persps
    ;"TAB" '(:ignore t :which-key "Perspective")
    ;"TAB TAB" '(persp-switch :which-key "persp-switch")
    ;"TAB [" '(persp-switch-prev :which-key "persp-switch-prev")
    ;"TAB ]" '(persp-switch-next :which-key "persp-switch-next")
    ;"TAB n" '(persp-add-new-anonymous-scratch-buffer :which-key "persp-add-new-anonymous-scratch-buffer")
    ;"TAB N" '(persp-add-new-anonymous :which-key "persp-add-new-anonymous")
    ;"TAB k" '(persp-kill-current :which-key "persp-kill-current")
    ;"TAB K" '(persp-kill-all-except-default :which-key "persp-kill-all-except-default")
    ;"TAB h" '(hydra-switch-persp/body :which-key "hydra-switch-persp")

    ;; perspectives
    "TAB" '(:ignore t :which-key "Perspective")
    "TAB TAB" '(persp-switch :which-key "persp-switch")
    "TAB [" '(persp-prev :which-key "persp-prev")
    "TAB ]" '(persp-next :which-key "persp-next")
    "TAB n" '(+persp/add-new :which-key "+persp/add-new")
    "TAB N" '(+persp/add-new-import-buffer :which-key "+persp/add-new-import-buffer")
    "TAB k" '(+persp/kill-current :which-key "+persp/kill-current")
    "TAB K" '(+persp/kill-all-except-default :which-key "+persp/kill-all-except-default")
    "TAB h" '(hydra-switch-persp/body :which-key "hydra-switch-persp")


    ;; git
    "g" '(:ignore t :which-key "Git") ; prefix
    "gg" '(magit-status :which-key "Git status"))

  ;; minibuffer keybindings
  (general-define-key
    :keymaps default-minibuffer-maps
    [escape] 'abort-recursive-edit ;; escape should always quit

    "C-a" 'move-beginning-of-line
    "C-e" 'move-end-of-line

    "C-w" 'backward-delete-word
    "C-v" 'yank)

  ;; evil bindings
  ;; TODO this is a bit of a mess, I need to go through the state hierarchy to define hotkeys in highest priority
  ;; normal/visual mode hotkeys
  (general-define-key
    :states '(normal visual)
    ;; evil numbers
    "g=" 'evil-numbers/inc-at-pt
    "g-" 'evil-numbers/dec-at-pt

    ;; flyspell correct
    "z=" 'flyspell-correct-wrapper
    "C-;" 'flyspell-correct-wrapper

    ;; movement
    "C-n" 'evil-next-visual-line ;; TODO should be in motion? doesn't seem to go down to these states?
    "C-p" 'evil-previous-visual-line
    "s" 'avy-goto-char)

  ;; insert mode hotkeys
  (general-define-key
    :states 'insert
    ;"C-SPC" 'company-complete
    "C-SPC" 'completion-at-point ;; bring up corfu completion
    "C-v" 'yank ;; C-v should paste clipboard contents

    "C-<backspace>" 'my-backward-kill-word
    "M-<backspace>" 'my-backward-kill-line

    ;; some emacs editing hotkeys inside insert mode
    "C-a" 'evil-beginning-of-visual-line
    "C-e" 'evil-end-of-visual-line
    "C-n" 'evil-next-visual-line
    "C-p" 'evil-previous-visual-line)

  ;; motion mode hotkeys, inherited by normal/visual
  (general-define-key
    :states 'motion
    "?" 'consult-line

    ;; window management
    "C-w C-u" 'winner-undo
    "C-w u" 'winner-undo

    "C-w a" 'ace-window
    "C-w C-w" 'ace-window
    "C-w w" 'ace-window

    "C-w C-l" 'evil-window-right
    "C-w C-h" 'evil-window-left)


  ;; company
  (general-define-key
    :keymaps '(company-active-map)
    "C-w" nil ; allow C-w to act normally during completion
    "C-h" nil
    "C-n" #'company-select-next
    "C-p" #'company-select-previous
    "TAB" #'company-complete-selection
    "<tab>" #'company-complete-selection)

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

     ;; C-v to paste (or "yank" in emacs jargon) from clipboard, useful for minibuffers (such as query-replace and M-x)
    "C-v" 'yank

    ;; buffer management
    "C-a" 'bury-buffer
    "C-S-a" 'unbury-buffer
    "C-z" 'consult-buffer

    ;; perspective
    ;; persp cycling
    "C-<tab>" 'persp-next
    "C-<iso-lefttab>" 'persp-prev
    "C-S-<tab>" 'persp-prev


    ;; quick perspective switching
    "M-1" (lambda () (interactive) (+persp/switch-by-index 0))
    "M-2" (lambda () (interactive) (+persp/switch-by-index 1))
    "M-3" (lambda () (interactive) (+persp/switch-by-index 2))
    "M-4" (lambda () (interactive) (+persp/switch-by-index 3))
    "M-5" (lambda () (interactive) (+persp/switch-by-index 4))
    "M-6" (lambda () (interactive) (+persp/switch-by-index 5))
    "M-7" (lambda () (interactive) (+persp/switch-by-index 6))
    "M-8" (lambda () (interactive) (+persp/switch-by-index 7))
    "M-9" (lambda () (interactive) (+persp/switch-by-index 8)))

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
;;----


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
  (setq flycheck-indication-mode 'right-fringe
        flycheck-display-errors-delay 0.25)
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
  ;; electric pair mode for '$' https://tex.stackexchange.com/a/75884
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

;; TODO try out corfu
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  :hook (prog-mode . corfu-mode)
  :general
  (:keymaps 'corfu-map
            "C-n" 'corfu-next
            "C-p" 'corfu-previous
            "C-SPC" 'corfu-insert-separator)

  :init
  (corfu-global-mode)
  :config
  ;; HACK evil keymaps seem to take precedence over corfu's map, use advice to fix
  ;; https://github.com/minad/corfu/issues/12#issuecomment-881961510
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map))

;(use-package corfu-doc
;  :after corfu
;  :hook (corfu-mode . corfu-doc-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)
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

;; dired
(use-package dired
  :straight nil
  :general
  (:keymaps 'dired-mode-map
    "c" 'find-file))

;; ranger enhancement for dired
(use-package ranger)
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


