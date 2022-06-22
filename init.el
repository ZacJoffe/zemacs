;; TODO early init setup https://www.reddit.com/r/emacs/comments/enmbv4/earlyinitel_reduce_init_time_about_02_sec_and/
;;; EDITOR GENERAL
(setq inhibit-startup-message t)

;; scratch buffer defaults
(setq initial-major-mode 'text-mode)

;; performance stuff
;; increase amount of data which emacs can read from processes (mainly for LSP mode https://emacs-lsp.github.io/lsp-mode/page/performance/)
(setq read-process-output-max (* 1024 1024) ;; 1mb
      ;; increase the gc threshold
      gc-cons-threshold 100000000)


;; turn off ad-redef warnings https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)


;; empty scratch buffer text
(setq initial-scratch-message "")

;; some editor settings
(setq-default indent-tabs-mode nil    ;; indent with spaces
              tab-width 4             ;; 1 tab <=> 4 spaces
              c-basic-offset 4        ;; indentation for cc modes
              c-default-style "linux" ;; https://en.wikipedia.org/wiki/Indentation_style
              fill-column 80          ;; wrap at 80 characters for auto-fill-mode
              word-wrap t             ;; do not wrap characters in the middle of words
              truncate-lines t)       ;; do not wrap by default

;; properly indent ')' https://stackoverflow.com/q/58844859
;(add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren))
(c-set-offset 'arglist-close 'c-lineup-close-paren)


;; set source directory to view source code of functions defined in C
;; https://github.com/raxod502/radian/blob/develop/emacs/radian.el#L3951
(setq find-function-C-source-directory (expand-file-name "src" source-directory))


;; save buffers on close (sessioning)
(setq desktop-path '("~/"))

;; do not display empty cursor in other windows (especially discracting with hydras)
(setq-default cursor-in-non-selected-windows nil)

;; remember window configuration changes
(winner-mode 1)

;; automatically refresh buffers
(global-auto-revert-mode 1)

;; some sane defaults
(scroll-bar-mode -1) ;; disable visible scrollbar
(tool-bar-mode -1)   ;; disable the toolbar
(tooltip-mode -1)    ;; disable tooltips
(menu-bar-mode -1)   ;; disable the menu bar

;; sentences should end with 1 space
(setq sentence-end-double-space nil)

;; fringe setup (left . right)
(set-fringe-mode '(4 . 4))


;; custom file setup https://diamondbond.neocities.org/emacs.html#orge163cd3
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Write to it if it does not exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Load custom file. Don't hide errors. Hide success message
(load custom-file nil t)


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
  "Hook to enable displaying trailing whitespace."
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
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Iosevka Fixed" :height 240 :weight 'regular)
  (set-face-attribute 'default nil :font "Iosevka Fixed" :height 170 :weight 'light))
;; float height value (1.0) makes fixed-pitch take height 1.0 * height of default
(set-face-attribute 'fixed-pitch nil :font "Iosevka Fixed" :height 1.0 :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "Iosevka" :height 1.0 :weight 'regular)

;; setup line numbers
;; do not dynamically resize line number column when a digit needs to be added
(setq display-line-numbers-width-start t)

;; HACK prevent M-x from shrinking line number width
;; https://github.com/abo-abo/swiper/issues/1940#issuecomment-465374308
(setq display-line-numbers-width 3)

;; I don't want line numbers in help files, dired, etc.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)


;; I use chemacs2 on my desktop, slightly ugly hack to open the correct file in open-init-file
(when (eq computer 'linux-desktop)
  (if (file-exists-p "~/.zemacs/init.el")
      (setq user-init-file "~/.zemacs/init.el")
    (setq user-init-file "~/.emacs.d/init.el")))


;; relative line numbers
(setq display-line-numbers-type 'relative)

;; scrolling config
(setq scroll-step            1 ;; smooth scroll
      scroll-conservatively  10000

      ;; https://github.com/hlissner/doom-emacs/blob/master/core/core-ui.el#L150
      hscroll-margin 2
      hscroll-step 1
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil)

;; smoother scrolling (especially for trackpad) via emacs 29
(pixel-scroll-precision-mode 1)

;; disable bells (distracting)
(setq ring-bell-function 'ignore)

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
     (mapc #'kill-buffer (delq (current-buffer) (buffer-list))))

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
  "Same as my-backward-kill-word, but insert a space after merging lines."
  (interactive)
  (if (or (bolp) (eq (current-column) (current-indentation)))
      (progn
        (my-backward-kill-word)
        (insert " "))
    (evil-delete-backward-word)))


;; backward-kill-word without copying to kill-ring
;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word ARG times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

;; this is mainly for allowing me to use C-w to delete words in vertico buffers (see general.el for hotkeys)
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word ARG times."
  (interactive "p")
  (delete-word (- arg)))


;; https://github.com/hlissner/doom-emacs/blob/master/core/autoload/text.el#L293
(defun toggle-indent-style ()
  "Toggle use of tabs or spaces."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "Indent style changed to %s" (if indent-tabs-mode "tabs" "spaces")))

;; https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  "Toggle horizontal/vertical split."
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

;; useful to have on an easily accessible key, also used a bunch with persp mode hacks
(defun open-scratch-buffer ()
  "Open *scractch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;; TODO FIXME proper indentation in text mode
;; https://stackoverflow.com/a/22109370
(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "(") (looking-at ")"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

;; useful for emacs daemon
(defun +reload-config ()
  "Reload `init.el' without closing Emacs."
  (interactive)
  (load-file user-init-file))
;;----


;; ADVICE/HOOKS
(defun +tab--jump-out (oldfun &rest args)
  "Forward-char if next-char is a delimiter, otherwise call OLDFUN with ARGS."
  (let ((delimiters '("(" ")" "[" "]" "{" "}" "\\" "<" ">" ";" "|" "`" "'" "\""))
        (next-char (string (char-after))))
    (if (member next-char delimiters)
        (forward-char)
      (apply oldfun args))))

(advice-add 'indent-for-tab-command :around #'+tab--jump-out)
(advice-add 'org-cycle :around #'+tab--jump-out)

;; automatically clone the emacs source repo if needed (in source directory)
;; https://github.com/raxod502/radian/blob/develop/emacs/radian.el#L3954
(defun +clone-emacs-source (&rest _)
  "Prompt user to clone Emacs source repository if needed."
  (when (and (not (file-directory-p source-directory))
             (not (get-buffer "*clone-emacs-src*"))
             (yes-or-no-p "Clone Emacs source repository? "))
    (make-directory (file-name-directory source-directory) 'parents)
    (let ((compilation-buffer-name-function
           (lambda (&rest _)
             "*clone-emacs-src*")))
      (save-current-buffer
        (compile
         (format
          "git clone https://github.com/emacs-mirror/emacs.git %s"
          (shell-quote-argument source-directory)))))))

(advice-add 'find-function-C-source :before #'+clone-emacs-source)
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
  :init
  ;; auto-saves go in another directory
  ;; https://github.com/emacscollective/no-littering#auto-save-settings
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        create-lockfiles nil))


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
        evil-search-module 'evil-search
        evil-respect-visual-line-mode t)
  :general
  ;; j and k should operate line-by-line with text wrapping
  (;[remap evil-next-line] 'evil-next-visual-line
   ;[remap evil-previous-line] 'evil-previous-visual-line
   ;; inverse of evil jump backward
   "C-S-o" 'evil-jump-forward)
  :config
  ;; highlight the current line (not explicitly evil but whatever)
  (global-hl-line-mode 1)

  ;; make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  ;; HACK prevent evil from moving window location when splitting by forcing a recenter
  ;; also do not switch to new buffer
  ;(advice-add 'evil-window-vsplit :after (lambda (&rest r) (progn (evil-window-prev 1) (recenter))))
  ;(advice-add 'evil-window-split :after (lambda (&rest r) (progn (evil-window-prev 1) (recenter))))

  ;; HACK
  (advice-add 'evil-window-vsplit :override (lambda (&rest r) (split-window (selected-window) nil 'right)))
  (advice-add 'evil-window-split :override (lambda (&rest r) (split-window (selected-window) nil 'below)))

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


;; editor config
(use-package editorconfig
  :config
  (editorconfig-mode 1))


;; hungry delete
;; TODO broken
;(use-package hungry-delete
;  :general
;  (:keymaps 'hungry-delete-mode-map
;    "<backspace>" 'backward-delete-char-untabify
;    "M-<backspace>" 'hungry-delete-backward)
;  :init
;  (global-hungry-delete-mode 1))


(use-package electric
  :straight (:type built-in)
  :init
  ;; delimeter pairing
  (setq electric-pair-delete-adjacent-pairs t)
  ;; TODO
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})))
  ;(setq-default electric-indent-chars '(?\n ?\^?))

  ;; prevent electric pair mode from being enabled in the mini buffer (for things like consult)
  ;; https://emacs.stackexchange.com/a/29342
  (setq electric-pair-inhibit-predicate (lambda (char) (minibufferp)))
  (electric-pair-mode 1))

;(use-package aggressive-indent
;  :config
;(global-aggressive-indent-mode 1)
;(add-to-list 'aggressive-indent-excluded-modes 'html-mode))
;;----


;;; VERTICO
;; vertico - completion engine
(use-package vertico
  :init
  (vertico-mode)
  ;; https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
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
  ;; enabled right away. NOTE that this forces loading the package.
  (marginalia-mode))

;; consult
(use-package consult
  :config
  ;; TODO consult-line working with evil-ex-search-forward
  ;; https://github.com/abo-abo/swiper/issues/2344#issuecomment-561454647
  ;(dolist (variable '(evil-ex-search-history consult--line-history evil-search-forward-history evil-search-backward-history))
  ;  (defvaralias variable 'regexp-search-ring))
  ;(defvaralias 'consult--line-history 'evil-search-forward-history)
  )

;; integration with flycheck
(use-package consult-flycheck
  :after (consult flycheck))

;; consult functions
(defun +consult/find-file (DIR)
  "Open file in directory DIR."
  (interactive "DSelect dir: ")
  (let ((selection (completing-read "Find file: " (split-string (shell-command-to-string (concat "find " DIR)) "\n" t))))
    (find-file selection)))

(defun +consult/ripgrep (DIR)
  "Ripgrep directory DIR."
  (interactive "DSelect dir: ")
  ;(let ((consult-ripgrep-args "rg --null --multiline --max-columns=1000 --path-separator /\ --smart-case --no-heading --line-number .")))
  (consult-ripgrep DIR))

;; FIXME broken
(defun +consult/search-project ()
  ""
  (interactive)
  (if-let ((dir (projectile-project-root)))
      (+consult/ripgrep dir)
    (call-interactively (+consult/ripgrep))))

(defun +consult/org-roam-ripgrep ()
  "Ripgrep org-directory."
  (interactive)
  (consult-ripgrep org-directory))

;; allow me to use `evil-ex-search-next' and `evil-ex-search-previous' on result from `consult-line'
(defun +consult-line ()
  "Wrapper around `consult-line' that populates evil search history."
  (interactive)
  (consult-line)
  (let ((search-pattern (car consult--line-history)))
    ;; HACK manually set the search pattern and evil ex highlighting
    (setq evil-ex-search-pattern (evil-ex-make-search-pattern search-pattern))
    (evil-ex-search-activate-highlight evil-ex-search-pattern)))


;; embark
(use-package embark
  :general
  ("C-l" 'embark-act)
  ("<mouse-3>" 'embark-act) ;; right click
  (:keymaps 'evil-normal-state-map
   "C-." 'embark-act
   "<mouse-3>" 'embark-act)
  :init
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

(use-package treemacs-magit
  :after (treemacs magit))
;;----


;; WORKSPACES/PERSPECTIVES
;; TODO buffers
;; TODO save persps?
;; TODO emacs server integration
;; TODO winner mode support https://github.com/nex3/perspective-el/issues/137

(use-package perspective
  :config
  (setq persp-initial-frame-name "main" ;; "main" perspective
        persp-sort 'created             ;; do not reorder perspectives (renaming persp sorts list by default)
        persp-suppress-no-prefix-key-warning t)
  ;; HACK the ordering is from most-least recent, which doesn't make much sense - reverse it to fix display
  ;; while not breaking persp functions (like 'persp-prev' and 'persp-next')
  (advice-add #'persp-names :filter-return (lambda (persps) (reverse persps)))
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
  "Build string containig persp tabline and custom message MSG with optional face type FACE-TYPE."
  (concat (+persp--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" msg)
                      'face face-type))) ;; TODO check for face-type

(defun +persp-message (msg &optional face-type)
  "Display persp tabline and custom message MSG with optional face type FACE-TYPE."
  (message "%s" (+persp--message msg face-type)))

(defun +persp/display ()
  "Display persp tabline at the bottom of the screen."
  (interactive)
  (message "%s" (+persp--tabline)))

;; the new persp name is generated by adding 1 to the maximum of each persp name (as a num) and its length.
;; this allows the new perspective to appear at the end of the persp list (persp names are sorted), and guarantees
;; that the new name is unique (i.e. will not switch to an existing persp)
(defun +persp--add-new ()
  "Switch to a new perspective and open scratch buffer."
  (let* ((persps (persp-names))
         (num-persps (length persps))
         ;; if there exist a persp with the name of a number, the new name will be 1+ that num
         ;; otherwise, make the name 0
         (new-persp-name (number-to-string
                          ;; NOTE `string-to-number' returns 0 if the number cannot be converted
                          (let ((max-persp-name (apply #'max (mapcar #'string-to-number persps))))
                            (if (eq max-persp-name 0)
                                1)
                            (1+ max-persp-name)))))
    (persp-switch new-persp-name)))

(defun +persp/add-new ()
  "Switch to a new perspective and open scratch buffer."
  (interactive)
  (+persp--add-new)
  (+persp/display))

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
  "Kill the current persp."
  (interactive)
  (let ((curr (persp-current-name)))
    (if (string= curr persp-initial-frame-name)
        (+persp-message (format "Cannot kill protected persp %s" curr) 'error)
      (persp-prev)
      (persp-kill curr)
      (let ((new-curr (persp-current-name)))
        (+persp-message (format "Killed persp %s" curr) 'success)))))

;; TODO refactor to explicitly switch to (car (persp-names))
;; TODO reset name of default
(defun +persp/kill-all-except-default ()
  "Kill all persps other than the default persp."
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
          (+persp-message (concat "Already in persp " name) 'warning)
        (persp-switch name)
        (+persp/display))
    (+persp-message (format "Invalid persp index %d" (1+ index)) 'error)))


;; perspective aware consult-buffer https://github.com/nex3/perspective-el#buffer-switchers
(consult-customize consult--source-buffer :hidden t :default nil)
(add-to-list 'consult-buffer-sources 'persp-consult-source)

;; TODO how does doom handle the bury buffer with persp-mode?

;; persp display when cycling through perspectives
(defun +persp--cycle (oldfun)
  "Persp display on persp cycling."
  (if (eq (length (persp-names)) 1)
      (+persp-message "No other persps" 'warning)
    (funcall oldfun)
    (+persp/display)))

(advice-add 'persp-prev :around #'+persp--cycle)
(advice-add 'persp-next :around #'+persp--cycle)


;; this code is very similar to `persp-names' with `persp-sort' equal to 'access
(defun +persp--last-accessed ()
  "Get the previously accesed persp."
  (interactive)
  (let ((persps (hash-table-values (perspectives-hash))))
    (if (eq (length persps) 1)
        (persp-current-name)
      (nth 1 (mapcar 'persp-name
                     (sort persps (lambda (a b)
                                    (time-less-p (persp-last-switch-time b)
                                                 (persp-last-switch-time a)))))))))

(defun +persp/switch-to-last-accessed ()
  "Switch to the previously accessed persp."
  (interactive)
  (persp-switch (+persp--last-accessed))
  (+persp/display))

;; wrapper around 'persp-rename' to use persp display
(defun +persp/rename (new-name)
  "Switch to persp NEW-NAME."
  (interactive "sNew name: ")
  (let ((curr-name (persp-current-name)))
    (if (not (persp-valid-name-p new-name))
        (+persp-message "Invalid persp name" 'error)
      (if (gethash new-name (perspectives-hash))
          (+persp-message (format "Persp '%s' already exists" new-name) 'error)
        (persp-rename new-name)
        (+persp-message (format "Renamed '%s'->'%s'" curr-name new-name) 'success)))))


;; TODO this is all broken, I think I can fix it with some hacks though
;; https://github.com/nex3/perspective-el/issues/63#issuecomment-322579522
(defun +persp/get-buffers-list ()
  "Get filtered list of buffers, sorted alphabetically."
  (sort
   (cl-remove-if '(lambda (b) (if (stringp b) (string-match "^\[* \]" b) t))
                 (mapcar 'buffer-name (persp-buffers (persp-curr))))
   'string<))

(defun +persp/next-buffer ()
  "Perspective aware `next-buffer'. Don't show internal buffers."
  (interactive)
  (let ((buffername (buffer-name (current-buffer)))
        (bufferlist (+persp/get-buffers-list)))
    (switch-to-buffer (or (cadr (member buffername bufferlist)) (car bufferlist)))))

(defun +persp/previous-buffer ()
  "Perspective aware `previous-buffer'. Don't show internal buffers."
  (interactive)
  (let ((buffername (buffer-name (current-buffer)))
        (bufferlist (reverse (+persp/get-buffers-list))))
    (switch-to-buffer (or (cadr (member buffername bufferlist)) (car bufferlist)))))

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


;; TODO FIXME shows misc info (persps) in dired
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
        ;; default line endings are LF on mac/linux, CRLF on windows
        doom-modeline-default-eol-type (if (or (eq system-type 'gnu/linux) (eq system-type 'darwin)) 0 1))

  :config
  ;; display symlink file paths https://github.com/seagle0128/doom-modeline#faq
  (setq find-file-visit-truename t)

  ;; HACK perspective list is shown in global-mode-string, and the misc-info segment only displays this
  ;; on the active window. This segment is the same as misc-info, without the check for the active window
  ;; so the persp list always shows
  ;; https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-segments.el#L1616-L1621
  (doom-modeline-def-segment +my/misc-info
    (when (not doom-modeline--limited-width-p)
      '("" mode-line-misc-info)))

  ;; doom uses the default modeline that is defined here: https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline.el#L90
  ;; as far as I can tell you can't change the ordering of segments without redefining the modeline entirely (segments can be toggled though)
  ;;
  ;; this is a bit messy since I already set a bunch of toggles, TODO need to work on this
  (doom-modeline-def-modeline 'main
    '(bar modals matches buffer-info buffer-position selection-info)
    ;'(buffer-encoding lsp major-mode process vcs checker " " +my/misc-info "  ")))
    '(buffer-encoding lsp major-mode process vcs checker " ")))


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
  :hook ((prog-mode . highlight-numbers-mode)))


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
  :config
  (when (display-graphic-p)
      (solaire-global-mode +1)))


;; GIT
;; magit
(use-package magit
  ;; refresh status when you save file being tracked in repo
  :hook (after-save . magit-after-save-refresh-status)
  ;; start magit commit in insert mode https://emacs.stackexchange.com/a/20895
  :hook (git-commit-mode . evil-insert-state)
  :config
  ;; display magit status in current buffer (no popup) https://stackoverflow.com/a/58554387/11312409
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-auto-revert-mode t
        git-commit-summary-max-length 50))

;; magit functions
;; inpsired by https://www.manueluberti.eu/emacs/2018/02/17/magit-bury-buffer/
(defun magit-kill-buffers ()
  "Restore window configuration and kill all magit buffers."
  (let ((buffers (magit-mode-get-buffers)))
    (mapc #'kill-buffer buffers)))

;; TODO also kill diff buffer
(defun +magit/quit (&optional kill-buffer)
  "Bury the current magit Buffer.
If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
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

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))
;;----


;; terminal emulator
(use-package vterm
  ;; disable hl line mode in terminal
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  ;; do not allow backspace on terminal prompt
  (setq comint-prompt-read-only t))


;; ediff
(use-package ediff
  :straight (:type built-in)
  :config
  ;; sane configs
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-highlight-all-diffs nil
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; restore window after ediff https://emacs.stackexchange.com/a/17089
  (defvar ediff--saved-wconf nil)
  :hook (ediff-before-setup . (lambda () (setq ediff--saved-wconf (current-window-configuration))))
  :hook (ediff-quit . (lambda () (when (window-configuration-p ediff--saved-wconf)
                         (set-window-configuration ediff--saved-wconf)))))

;; markdown
(use-package markdown-mode
  ;:straight nil
  :config
  ;; syntax highlighting in code blocks
  (setq markdown-fontify-code-blocks-natively t))


;; ORG
(use-package org
  ;; HACK (?) prevents needed `org-reload' to fix org agenda (which seems to break org mode)
  ;; https://www.reddit.com/r/emacs/comments/rr203h/using_straightel_and_usepackage_to_configure_org/hqdzpc5/
  :straight (:type built-in)
  ;:hook (org-mode . org-indent-mode)  ;; indent org stuff
  :hook (org-mode . visual-line-mode) ;; wrap lines
  :hook (org-mode . flyspell-mode)    ;; spelling
  :hook (org-tab-first-hook . +org-indent-maybe-h) ;; doom's TAB key behaviour
  :config
  (setq org-agenda-span 10 ; https://stackoverflow.com/a/32426234
        org-agenda-start-on-weekday nil
        org-agenda-files '("~/Documents/Google/org/roam/agenda") ;; https://stackoverflow.com/a/11384907
        org-agenda-window-setup 'current-window ;; open agenda in current window
        org-todo-keywords '((sequence "TODO(t)" "EXAM(e)" "WAIT(w)" "|" "DONE(d)" "KILL(k)" "SKIPPED(s)" "LATE(s)"))
        org-return-follows-link t
        org-directory "~/Documents/Google/org"
        org-src-tab-acts-natively t ;; https://stackoverflow.com/a/27236621/11312409
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0 ;; https://www.reddit.com/r/orgmode/comments/mobien/org_mode_code_block_indentation/gu3jjkg/
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t)

  ;; use minted to handle code listings https://emacs.stackexchange.com/a/27984
  (setq org-latex-listings 'minted
    org-latex-packages-alist '(("" "minted"))
    org-latex-pdf-process
    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
)

;; small snippet that I use often in org
(defun +org-insert-source-block ()
  "Insert code block, force insert mode."
  (interactive)
  (insert "#+BEGIN_SRC

#+END_SRC")
  (forward-line -1)
  (goto-char (line-end-position))
  (evil-insert 0))


;; https://github.com/doomemacs/doomemacs/blob/master/modules/lang/org/autoload/org.el#L402
(defun +org-indent-maybe-h ()
  "Indent the current item (header or item), if possible.
Made for `org-tab-first-hook' in evil-mode."
  (interactive)
  (cond ((not (and (bound-and-true-p evil-local-mode)
                   (evil-insert-state-p)))
         nil)
        ((and (bound-and-true-p org-cdlatex-mode)
              (or (org-inside-LaTeX-fragment-p)
                  (org-inside-latex-macro-p)))
         nil)
        ((org-at-item-p)
         (if (eq this-command 'org-shifttab)
             (org-outdent-item-tree)
           (org-indent-item-tree))
         t)
        ((org-at-heading-p)
         (ignore-errors
           (if (eq this-command 'org-shifttab)
               (org-promote)
             (org-demote)))
         t)
        ((org-in-src-block-p t)
         (save-window-excursion
           (org-babel-do-in-edit-buffer
            (call-interactively #'indent-for-tab-command)))
         t)
        ((and (save-excursion
                (skip-chars-backward " \t")
                (bolp))
              (org-in-subtree-not-table-p))
         (call-interactively #'tab-to-tab-stop)
         t)))

;; org-roam 2
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/Google/org/roam"))
  :config
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

(defvar +org-roam-dailies-directory (concat org-roam-directory "/daily"))

;; TODO FIXME broken?
;(use-package org-roam-protocol
;  :straight nil
;  :after org-protocol)

;; prettier headings
(use-package org-superstar)

;; better rendering of things like italics
;(use-package org-appear
;  :commands (org-appear-mode)
;  :hook (org-mode . org-appear-mode)
;  :init
;  (setq org-hide-emphasis-markers t)  ;; A default setting that needs to be t for org-appear
;  (setq org-appear-autoemphasis t)    ;; Enable org-appear on emphasis (bold, italics, etc)
;  (setq org-appear-autolinks nil)
;  (setq org-appear-autosubmarkers t)) ;; Enable on subscript and superscript

;; FIXME I am essentially trying to create a consult interface for previewing files in a directory
;; TODO preview org dailies with consult
;(defun +org--consult-dailies-sources ()
;  ""
;  `(:name "dailies"
;    :narrow ?f
;    :category file
;    :face consult-file
;    :items
;          ))
;;----


;; which-key
(use-package which-key
    :init
    (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
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
;; TODO hydra posframe glitches when used
;(use-package hydra-posframe
;  :straight (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
;  :hook (after-init . hydra-posframe-enable))


;; simple hydra for resizing windows
(defhydra hydra-window (:hint nil)
  "
^Movement^  ^Resize^
---------------------------------------
_h_ ←       _H_ X←
_j_ ↓       _J_ X↓
_k_ ↑       _K_ X↑
_l_ →       _L_ X→
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right))

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
Buffer Zoom
-------------------------
_=_   text-scale-increase
_-_   text-scale-decrease
_r_   reset text scale

Frame Zoom
-------------------------
_M-=_ zoom-in
_M--_ zoom-out
_k_   zoom-in
_j_   zoom-out
_R_   reset frame zoom
"
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("r" (lambda () (interactive) (text-scale-adjust 0)))

  ("M-=" zoom-in)
  ("M--" zoom-out)
  ("k" zoom-in)
  ("j" zoom-out)
  ("R" (lambda () (interactive) (zoom-in/out 0))))


;; use hydra to quickly load themes
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
  "Disable enabled themes, then load THEME."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))
;;----


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
    :states '(motion normal visual treemacs) ;; NOTE the treemacs state
    :keymaps 'override ;; https://github.com/noctuid/general.el/issues/99#issuecomment-360914335

    ;; map universal argument to SPC-u
    "u" '(universal-argument :which-key "Universal argument")
    ";" '(eval-region :which-key "eval-region")
    "SPC" '(projectile-find-file :which-key "Projectile find file")
    ;"SPC" '(+consult/find-file :which-key "+consult/find-file")
    "." '(find-file :which-key "Find file")
    "," '(consult-buffer :which-key "consult-buffer")
    ":" '(execute-extended-command :which-key "M-x")
    "x" '(open-scratch-buffer :which-key "Open scratch buffer")
    "d" '(dired-jump :which-key "dired-jump")
    "/" '(+consult/ripgrep :which-key "+consult/ripgrep")
    "[" '(persp-prev :which-key "persp-prev")
    "]" '(persp-next :which-key "persp-next")


    ;; editor
    "e" '(:ignore t :which-key "Editor")
    "eu" '(undo-tree-visualize :which-key "undo-tree-visualize")
    "et" '(hydra-theme/body :which-key "hydra-theme") ; not sure if this is the best place for this, perhaps toggles would be more appropriate?
    "er" '(query-replace :which-key "query-replace")
    "ec" '(consult-theme :which-key "consult-theme")

    ;; buffer
    ;"TAB" '(switch-to-prev-buffer :which-key "Prev buffer")
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "consult-buffer")
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
    "wt" '(toggle-window-split :which-key "toggle-window-split")
    "wa" '(ace-window :which-key "ace-window")
    "wr" '(hydra-window/body :which-key "hydra-window")

    ;; toggles
    "t" '(:ignore t :which-key "Toggles")
    "ta" '(corfu-mode :which-key "corfu-mode") ;; 'a' for autocomplete
    "ts" '(flyspell-mode :which-key "flyspell-mode")
    "tf" '(flyspell-mode :which-key "flyspell-mode")
    "tc" '(flycheck-mode :which-key "flycheck-mode")
    "tm" '(minimap-mode :which-key "minimap-mode")
    "tg" '(evil-goggles-mode :which-key "evil-goggles")
    "tI" '(toggle-indent-style :which-key "Indent style")
    "tv" '(visual-line-mode :which-key "visual-line-mode")

    ;; notes/org
    "n" '(:ignore t :which-key "Notes")
    "nf" '(org-roam-node-find :which-key "find-node")
    "ni" '(org-roam-node-insert :which-key "insert-node")
    "nt" '(org-roam-dailies-goto-today :which-key "org-roam-dailies-goto-today")
    "n/" '(+consult/org-roam-ripgrep :which-key "+consult/org-roam-ripgrep")
    "na" '(org-agenda :which-key "org-agenda")

    ;; persps
    "TAB" '(:ignore t :which-key "Perspective")
    "TAB TAB" '(persp-switch :which-key "persp-switch")
    "TAB [" '(persp-prev :which-key "persp-prev")
    "TAB ]" '(persp-next :which-key "persp-next")
    "TAB n" '(+persp/add-new :which-key "+persp/add-new")
    "TAB N" '(+persp/add-new-import-buffer :which-key "+persp/add-new-import-buffer")
    "TAB k" '(+persp/kill-current :which-key "+persp/kill-current")
    "TAB d" '(+persp/kill-current :which-key "+persp/kill-current")
    "TAB K" '(+persp/kill-all-except-default :which-key "+persp/kill-all-except-default")
    "TAB r" '(+persp/rename :which-key "+persp/rename")
    "TAB a" '(+persp/add-buffer-switch :which-key "+persp/add-buffer-switch")

    ;; quick persp switching
    "1" '((lambda () (interactive) (+persp/switch-by-index 0)) :which-key nil)
    "2" '((lambda () (interactive) (+persp/switch-by-index 1)) :which-key nil)
    "3" '((lambda () (interactive) (+persp/switch-by-index 2)) :which-key nil)
    "4" '((lambda () (interactive) (+persp/switch-by-index 3)) :which-key nil)
    "5" '((lambda () (interactive) (+persp/switch-by-index 4)) :which-key nil)
    "6" '((lambda () (interactive) (+persp/switch-by-index 5)) :which-key nil)
    "7" '((lambda () (interactive) (+persp/switch-by-index 6)) :which-key nil)
    "8" '((lambda () (interactive) (+persp/switch-by-index 7)) :which-key nil)
    "9" '((lambda () (interactive) (+persp/switch-by-index 8)) :which-key nil)

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
    "?" '+consult-line

    ;; window management
    "C-w C-u" 'winner-undo
    "C-w u" 'winner-undo

    "C-w a" 'ace-window
    "C-w C-w" 'ace-window
    "C-w w" 'ace-window

    "C-w C-l" 'evil-window-right
    "C-w C-h" 'evil-window-left)

  ;; company
  ;; DELETEME keeping for now to help configure corfu
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
    ;; NOTE evil-ex-map is different from evil-ex-search-keymap
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
    ;"C-a" 'bury-buffer
    ;"C-S-a" 'unbury-buffer
    "C-a" '+persp/previous-buffer
    "C-S-a" '+persp/next-buffer
    ;"C-z" 'consult-buffer

    ;; persp cycling
    "C-<tab>" 'persp-next
    "C-<iso-lefttab>" 'persp-prev
    "C-S-<tab>" 'persp-prev
    "<backtab>" '+persp/switch-to-last-accessed

    ;; quick persp switching
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

  ;; magit keybindings
  ;; TODO refactor within use-package
  (general-define-key
    :states '(normal visual)
    :keymaps 'magit-mode-map
    ;; rebind "q" in magit-status to kill the magit buffers instead of burying them
    "q" '+magit/quit

    ;; persp switching within magit
    "M-1" (lambda () (interactive) (+persp/switch-by-index 0))
    "M-2" (lambda () (interactive) (+persp/switch-by-index 1))
    "M-3" (lambda () (interactive) (+persp/switch-by-index 2))
    "M-4" (lambda () (interactive) (+persp/switch-by-index 3))
    "M-5" (lambda () (interactive) (+persp/switch-by-index 4))
    "M-6" (lambda () (interactive) (+persp/switch-by-index 5))
    "M-7" (lambda () (interactive) (+persp/switch-by-index 6))
    "M-8" (lambda () (interactive) (+persp/switch-by-index 7))
    "M-9" (lambda () (interactive) (+persp/switch-by-index 8)))

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
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun +lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-ui-mode)
         (rustic-mode . lsp))
         (lsp-completion-mode . +lsp-mode-setup-completion)
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-snippet nil)) ;; TODO this is broken

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package consult-lsp)


;; dumb jump (indexless code navigation)
;; TODO configure with lsp
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read ;; use vertico to handle ambiguous cases
        dumb-jump-force-searcher 'rg) ;; force dumb jupm to use ripgrep (NOTE still defaults to git-grep if file is in git project)

  ;; preserve jump list in evil https://skeptric.com/dumbjump/
  (defun evil-set-jump-args (&rest ns) (evil-set-jump))
  (advice-add 'dumb-jump-goto-file-line :before #'evil-set-jump-args))


;; flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)
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


;; python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; emacs-ipython-notebook (jupyter)
(use-package ein)

;; julia
(use-package julia-mode)
(use-package julia-repl)


;; scala
(use-package scala-mode)
(use-package lsp-metals
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . lsp))


;; latex
(use-package auctex
  :hook (LaTeX-mode . visual-line-mode)
  :hook (LaTeX-mode . flyspell-mode)
  ;; electric pair mode for `$' https://tex.stackexchange.com/a/75884
  ;; TODO refactor with general
  :hook (LaTeX-mode . (lambda ()
                        (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)))
  :init
  ;; turn off subscripting and superscripting being rendered explicitly
  (setq tex-fontify-script nil
        font-latex-fontify-script nil))

(use-package latex-preview-pane
  :config
  (setq latex-preview-pane-multifile-mode 'auctex))

;; TODO implement so that frame doesn't get killed
;(defadvice! +latex--dont-reopen-preview-pane-a (fn &rest args)
;    "Once the preview pane has been closed it should not be reopened."
;    :around #'latex-preview-pane-update
;    (letf! (defun init-latex-preview-pane (&rest _)
;             ;; HACK Avoid the function because it tries to delete the current
;             ;;      window, but it's already gone, so it ends up deleting the
;             ;;      wrong window.
;             (setq-local latex-preview-pane-mode nil))
;      (apply fn args)))

(use-package evil-tex
  :hook (LaTeX-mode . evil-tex-mode))


;; edit files from browser in emacs
(use-package atomic-chrome
  :init
  (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-url-major-mode-alist
    '(("overleaf\\.com" . LaTeX-mode))))


;; thrift
(use-package thrift)


;; hex editing
;; TODO
(use-package nhexl-mode
  :hook (nhexl-mode . (lambda () display-line-numbers-mode -1)))


;; json
(use-package json-mode)


;; yaml
(use-package yaml-mode)


;; mermaid graphs
(use-package mermaid-mode
  :hook (mermaid-mode . (lambda () (electric-indent-local-mode -1))))


;; alloy
(use-package alloy-mode
  :straight (alloy-mode :type git :host github :repo "dwwmmn/alloy-mode"))
;;----

;; AUTOCOMPLETE
;; company TODO remove?
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

;; fuzzy autocomplete for company
(use-package company-flx
  :after company
  :hook (company-mode . company-flx-mode))

;; TODO cleanup comments
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;(corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ; (corfu-scroll-margin 5)        ;; Use scroll margin
  ;(corfu-min-width 40)
  ;(corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-echo-documentation t)
  (lsp-completion-provider :none)

  ;; You may want to enable Corfu only for certain modes.
  :hook (prog-mode . corfu-mode)
  :general
  (:keymaps 'corfu-map
   "C-n" 'corfu-next
   "C-j" 'corfu-next
   "C-p" 'corfu-previous
   "C-k" 'corfu-previous
   "C-SPC" 'corfu-insert-separator
   "<tab>" '+corfu-complete-quit
   "<escape>" '+corfu-quit) ;; NOTE also sets functionality of "C-["
  :init
  (global-corfu-mode)
  (defun +lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook (lsp-completion-mode . +lsp-mode-setup-completion)
  :config
  ;; HACK evil keymaps seem to take precedence over corfu's map, use advice to fix
  ;; https://github.com/minad/corfu/issues/12#issuecomment-881961510
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map))

(defun +corfu-quit ()
  "Quit corfu completion, go back to normal mode."
  (interactive)
  (corfu-quit)
  (evil-normal-state))

(defun +corfu-complete-quit ()
  "Corfu complete and quit."
  (interactive)
  (corfu-complete)
  (corfu-quit))

;; icons for corfu
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (setq kind-icon-use-icons nil) ;; text based icons
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
;; (use-package corfu-doc
;;   ;; NOTE 2022-02-05: At the time of writing, `corfu-doc' is not yet on melpa
;;   :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
;;   :after corfu
;;   :hook (corfu-mode . corfu-doc-mode)
;;   :general (:keymaps 'corfu-map
;;                      ;; This is a manual toggle for the documentation popup.
;;                      [remap corfu-show-documentation] #'corfu-doc-toggle ; Remap the default doc command
;;                      ;; Scroll in the documentation window
;;                      "M-n" #'corfu-doc-scroll-up
;;                      "M-p" #'corfu-doc-scroll-down)
;;   :custom
;;   (corfu-doc-delay corfu-auto-delay)
;;   (corfu-doc-max-width 70)
;;   (corfu-doc-max-height 20)

;;   ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
;;   ;; extra-safe that this is set when corfu-doc is loaded. I do not want
;;   ;; documentation shown in both the echo area and in the `corfu-doc' popup.
;;   (corfu-echo-documentation nil))

;; TODO configure
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
  (add-to-list 'completion-at-point-functions #'cape-tex)
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
  :hook (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  :hook (pdf-view-mode . auto-revert-mode) ;; TODO this may be redundant
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  ;; prevent blinking on selection https://github.com/politza/pdf-tools/issues/201#issuecomment-210989952
  :hook (pdf-view-mode . (lambda () (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))
  :config
  (pdf-tools-install)
  ; HiDPI support
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)

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
  :straight (:type built-in)
  :hook (dired-mode . auto-revert-mode)
  :general
  (:keymaps 'dired-mode-map :states 'normal ;; FIXME does this work? or does it break dired
    "H" 'dired-up-directory
    "c" 'find-file
    "?" 'hydra-dired/body)
  :config
  ;; https://github.com/hlissner/doom-emacs/blob/master/modules/emacs/dired/config.el#L3
  (setq dired-auto-revert-buffer t  ; don't prompt to revert; just do it
        dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Where to store image caches
        ;image-dired-dir (concat doom-cache-dir "image-dired/") ;; FIXME
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  )

;; colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; WIP I always forget dired hotkeys, this hydra should be useful to mitigate this
(defhydra hydra-dired (:hint nil :foreign-keys run)
  "
Movement
-----------------------------
_H_: dired-up-directory
_-_: dired-up-directory

Toggles
-----------------------------
_o_: dired-omit-mode

Modify
-----------------------------
_+_: dired-create-directory
_c_: find-file (create file)
_C_: dired-do-copy (copy file)
_R_: dired-do-rename (rename or mv)
_d_: dired-flag-file-deletion
_m_: dired-mark
_u_: dired-unmark
_x_: dired-do-flagged-delete
_q_: quit
_?_: quit
"
  ("H" dired-up-directory)
  ("-" dired-up-directory)

  ("o" dired-omit-mode)

  ("+" dired-create-directory)
  ("c" find-file)
  ("C" dired-do-copy)
  ("R" dired-do-rename)
  ("d" dired-flag-file-deletion)
  ("m" dired-mark)
  ("u" dired-unmark)
  ("x" dired-do-flagged-delete)
  ("q" nil :exit t)
  ("?" nil :exit t)
  ("<escape>" nil :exit t))
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


