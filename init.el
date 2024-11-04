;;; package --- Summary:
;;; Commentary:
;; Emacs 25.1 and newer tested
;;; Code:

(setq package-check-signature nil)

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; set some variables for org eco-system
(defvar org_notes "~/org/notes")

;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)


;; General Tweaks

;; Kill current buffer w/o promp
(defun volatile-kill-buffer ()
   "Kill current buffer unconditionally."
   (interactive)
   (let ((buffer-modified-p nil))
     (kill-buffer (current-buffer))))

(global-set-key (kbd "C-x k") 'volatile-kill-buffer)

;; Moving Lines
;; move line up
(defun my/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))
(global-set-key [(meta shift up)] 'my/move-line-up)

;; move line down
(defun my/move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(global-set-key [(meta shift down)] 'my/move-line-down)

;; Move windows
(global-set-key (kbd "M-s <left>")  'windmove-left)
(global-set-key (kbd "M-s <right>") 'windmove-right)
(global-set-key (kbd "M-s <up>")    'windmove-up)
(global-set-key (kbd "M-s <down>")  'windmove-down)

;; Foculs to new window after split
;; https://stackoverflow.com/a/6465415
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; By default Emacs triggers garbage collection at ~0.8MB which makes
;; startup really slow. Since most systems have at least 64MB of memory,
;; we increase it during initialization.
(setq gc-cons-threshold 128000000)

;; Stop littering undo tree files
(setq undo-tree-auto-save-history nil)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)
;; Overwrite region selected
(delete-selection-mode t)
;; Show column numbers by default
(setq column-number-mode t)
;; Use CUA to delete selections
(setq cua-mode t)
(setq cua-enable-cua-keys nil)
;; Prevent emacs from creating a bckup file filename~
(setq make-backup-files nil)
;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
              search-highlight t) ;hilit matches when searching
;; Highlight the line we are currently on
(global-hl-line-mode t)

;; Disable the toolbar at the top since it's useless
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable Emacs help on translation, e.g. C-x C-h actually
;; can do things now
(define-key key-translation-map [?\C-h] [?\C-?])

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Dont ask to follow symlink in git
(setq vc-follow-symlinks t)

;; Highlight some keywords in prog-mode
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlighting in cmake-mode this way interferes with
            ;; cmake-font-lock, which is something I don't yet understand.
            (when (not (derived-mode-p 'cmake-mode))
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\|DONE\\)"
                  1 font-lock-warning-face t))))))

;; Only allow encrypted auth sources
(setq auth-sources '((:source "~/.authinfo.gpg")))

;; Set Week day start to Monday
(setq calendar-week-start-day 1)

(setq whitespace-style '(face trailing tabs tab-mark))
(add-hook 'prog-mode-hook (lambda () (whitespace-mode t)))

;; Change tab key behavior to insert spaces instead
(setq-default indent-tabs-mode nil)

;; Set the number of spaces that the tab key inserts (usually 2 or 4)
(setq c-basic-offset 2)
;; Set the size that a tab CHARACTER is interpreted as
;; (unnecessary if there are no tab characters in the file!)
(setq tab-width 2)

;; i don't care to see the splash screen
(setq inhibit-splash-screen t)

;; hide the scroll bar
(scroll-bar-mode -1)

(electric-pair-mode 1)

(defmacro with-face (str &rest properties)
  "USED TO SET THE FACE OF STR WITH PROPERTIES."
  `(propertize ,str 'face (list ,@properties)))

(defun sl/make-header ()
  "."
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "[...]")
         )
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat (with-face sl/drop-str
                                 :background "blue"
                                 :weight 'bold
                                 )
                      (with-face (substring sl/header
                                            (+ (- (length sl/header)
                                                  (window-body-width))
                                               (length sl/drop-str))
                                            (length sl/header))
                                 ;; :background "red"
                                 :weight 'bold
                                 )))
          (concat
           (with-face sl/header
                      ;; :background "red"
                      :foreground "red"
                      :weight 'bold)))
      (concat (if window-system ;; in the terminal the green is hard to read
                  (with-face sl/header
                             ;; :background "green"
                             ;; :foreground "black"
                             :weight 'bold
                             :foreground "#8fb28f"
                             )
                (with-face sl/header
                           ;; :background "green"
                           ;; :foreground "black"
                           :weight 'bold
                           :foreground "blue"
                           ))
              (with-face (file-name-nondirectory buffer-file-name)
                         :weight 'bold
                         ;; :background "red"
                         )))))

(defun sl/display-header ()
  "CREATE THE HEADER STRING AND DISPLAY IT."
  ;; the dark blue in the header for which-func is terrible to read.
  ;; however, in the terminal it's quite nice
  (if window-system
      (custom-set-faces
       '(which-func ((t (:foreground "#8fb28f")))))
    (custom-set-faces
     '(which-func ((t (:foreground "blue"))))))
  ;; set the header line
  (setq header-line-format
        (list "-"
              '(which-func-mode ("" which-func-format))
              '("" ;; invocation-name
                (:eval (if (buffer-file-name)
                           (concat "[" (sl/make-header) "]")
                         "[%b]"))))))
;; call the header line update
(add-hook 'buffer-list-update-hook 'sl/display-header)

;; (set-frame-font "IosevkaNerdFont 10" nil t)

;; Set font size. Font size is set to my:font-size/10
(defvar my:font-size 100)

;; dont ask confimation when killing buffer witha process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; disable menu bar
(menu-bar-mode -1)

;; make mode bar small
;; (set-face-attribute 'mode-line nil  :height my:font-size)
;; set the header bar font
;; (set-face-attribute 'header-line nil  :height my:font-size)
;; set default window size and position
(add-to-list 'default-frame-alist '(width  . 110))
(add-to-list 'default-frame-alist '(height . 90))
(add-to-list 'default-frame-alist '(font . "IosevkaNerdFont-10"))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

;; enable line numbers on the lhs
(global-display-line-numbers-mode)
;; enable winner-mode
(winner-mode 1)
;; set the font to size 9 (90/10).
(set-face-attribute 'default nil :height my:font-size)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(which-function-mode 1)
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode
                            c++-mode
                            org-mode
                            c-mode
                            js2-mode
                            cmake-mode
                            rust-mode
                            python-mode
                            emacs-lisp-mode))
  )

;; dont ask for confirmation when exiting emacs with a process
(setq confirm-kill-processes nil)
;; dont ask confimation when killing buffer witha process
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; for frames
(global-set-key (kbd "C-<tab>") 'other-frame)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Copy file name to kill ring
(defun copy-full-path-to-kill-ring()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name)))
  )
(global-set-key (kbd "C-x M-b") 'copy-full-path-to-kill-ring)

;; Display dirs before files in dired
(setq dired-listing-switches "-al --group-directories-first")

;; Change default comments in c++ mode
(add-hook 'c++-mode (lambda () (setq comment-start "/* "
                                     comment-end " */")))

(add-hook 'cc-mode (lambda () (setq comment-start "/* "
                                    comment-end " */")))

;; Tree sitter mode grammer
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)
   (cmake-mode . cmake-ts-mode)
   (c-mode . c-ts-mode)
   (c++-mode . c++-ts-mode)
   ))
(setq treesit-font-lock-level 4)

;; Disable beep sounds
(setq visible-bell 1)
(setq gdb-many-windows t)

;; (setq require-final-newline nil)


;; Install packages

(use-package bind-key
  :ensure t)

;; so we can (require 'use-package) even in compiled emacs to e.g. read docs
(use-package use-package
  :commands use-package-autoload-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :config
  (require-theme 'modus-themes) ; `require-theme' is ONLY for the built-in Modus themes

  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-fringes 'subtle

        modus-themes-completions'((matches . (extrabold))
                                  (selection . (semibold italic text-also)))

        modus-themes-bold-constructs t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        '((fg-line-number-inactive fg-main)
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)
          (bg-mode-line-active bg-main)
          (bg-mode-line-inactive bg-main)

          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          )

        ;; modus-themes-preset-overrides-faint
        )

  ;; Increase modeline height
  (modus-themes-with-colors
    (custom-set-faces
     '(mode-line-active ((t :height 140)))
     '(mode-line-inactive ((t :height 140)))
     ))
  ;; Load the theme of your choice.
  (load-theme 'modus-vivendi t)
  ;; (load-theme 'modus-operandi t)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep allows you to edit all files in a grep result. For example,
;; you can use C-c g or C-c r to search all files in a project, then
;; use C-c C-o to enter ivy-occur mode, followed by 'w' to make
;; the grep results buffer editable, then you can edit the results
;; however you wish.
(use-package wgrep
  :ensure t
  :defer 1)

;; Note that you must have the line:
;;   allow-emacs-pinentry
;; inside ~/.gnupg/gpg-agent.conf and gpg 2.15+ and pinentry 0.9.5+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pinentry
  :ensure t
  :config
  (if (and
       (file-exists-p (file-truename "~/.gnupg/gpg-agent.conf"))
       (string-match ".*allow-emacs-pinentry.*"
                     (with-temp-buffer
                       (insert-file-contents
                        (file-truename "~/.gnupg/gpg-agent.conf"))
                       (buffer-string))))
      (progn
        (setenv "INSIDE_EMACS" (format "%s,comint" emacs-version))
        (pinentry-start))
    (user-error "%s%s"
                "You must have allow-emacs-pinentry in your "
                "~/.gnupg/gpg-agent.conf")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Sitter fold mode for code folding (In place of origami)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treesit-fold
  :load-path "~/.emacs-30.d/treesit-fold")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Sitter Indicators add indicators to gutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treesit-fold-indicators
  :load-path "/.emacs-30.d/treesit-fold")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: when you pause on a keyboard shortcut it provides
;;            suggestions in a popup buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :commands which-key-mode
  :defer 2
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy: always fast jump to char inside the current view buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package avy
  :ensure t
  :bind (("M-c" . avy-goto-char-2)
         ;; ("M-s" . avy-goto-word-1))
         ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use undo-tree to navigate undo history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :defer 1
  :config
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RealGud - https://github.com/realgud/realgud
;; A rewrite of GUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package realgud
  :ensure t
  :after (c-mode-common python-mode)
  :init
  (setenv "TERM" "dumb")
  :config
  (setq realgud:pdb-command-name "python -m pdb")
  (setq realgud-safe-mode nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default python-indent 4)
(setq-default python-indent-offset 4)

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)))

(setq-default pdb-command-name "python -m pdb")
(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "/home/pupil/Documents/projects/envs")
  :config
  (pyvenv-mode 1)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python"))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-c C-f
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-region))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cc-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  :config
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  ;; Compilation command for C/C++
  (defvar my:compile-command "g++ -Wall -Wextra -std=c++17 -o")
  (setq compile-command my:compile-command)
  (custom-set-variables '(c-noise-macro-names '("constexpr")))
  (use-package google-c-style
    :ensure t
    :config
    ;; This prevents the extra two spaces in a namespace that Emacs
    ;; otherwise wants to put... Gawd!
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    ;; Autoindent using google style guide
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)
    )
  )
;; (setq gdb-default-window-configuration-file "~/.emacs.d/my-gdb-many-windows")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cuda mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)
         ("\\.cuh\\'" . cuda-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors  - https://github.com/magnars/multiple-cursors.el
;; Allows you to have multiple cursors on different lines so you can
;; easily edit multiple lines at once.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m e" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this-word)
         ("C-<" . mc/mark-previous-like-this-word)
         )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-done 'time
      ;; org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" .
                                (:foreground "blue" :weight bold))))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(with-eval-after-load "org"
  (progn
    (define-key org-mode-map (kbd "C-c n n") 'org-id-get-create)
    (define-key org-mode-map (kbd "C-x n n") 'org-toggle-narrow-to-subtree)
    (setq org-link-frame-setup '((file . find-file)))
    (setq org-latex-pdf-process (list "latexmk -f -pdf %f"))
    ;; (setq org-latex-create-formula-image-program 'imagemagick)
    (setq org-latex-create-formula-image-program 'dvipng)
    (setq imagemagick '(imagemagick :programs
                                    ("latex" "convert")
                                    :description "pdf > png"
                                    :message "you need to install the programs: latex and imagemagick."
                                    :image-input-type "pdf"
                                    :image-output-type "png"
                                    :image-size-adjust (1.0 . 1.0)
                                    :latex-compiler ("pdflatex -interaction=nonstopmode -output-directory %o %f")
                                    :image-converter ;;on windows we need "magick" command prefix
                                    ("convert -density %D -trim -antialias %f -quality 100 %O")))
    (add-to-list 'org-preview-latex-process-alist imagemagick)
    ;; (setq org-preview-latex-default-process 'imagemagick)
    (plist-put org-format-latex-options :background "Transparent")
    (plist-put org-format-latex-options :scale 1.5)
    ))

;; Academic paper writing
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("apa6"
                 "\\documentclass{apa6}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

;; Task Management
(setq org-capture-templates
      '(("w" "work inbox" entry (file "~/org/todo-work.org")
         "* [work] %?\n%i\n%a")
        ("p" "personal inbox" entry (file "~/org/inbox.org")
         "* [personal] %?\n%i\n%a")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "DONE(d)")
        ))

(setq org-agenda-files
      '("~/org/inbox.org"
        "~/org/todo-work.org"
        "~/org/todo-personal.org"
        "~/org/long-term.org"))

(setq org-refile-targets
      '((nil :maxlevel . 2)
        ("~/org/todo-work.org" :maxlevel . 3)
        ("~/org/todo-personal.org" :maxlevel . 3)))

;; Show the daily agenda by default.
(setq org-agenda-span 'week)

;; Hide tasks that are scheduled in the future.
(setq org-agenda-todo-ignore-scheduled 'future)

;; Use "second" instead of "day" for time comparison.
;; It hides tasks with a scheduled time like "<2020-11-15 Sun 11:30>"
(setq org-agenda-todo-ignore-time-comparison-use-seconds t)

;; Hide the deadline prewarning prior to scheduled date.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

;; Customized view for the daily workflow. (Command: "C-c a n")
(setq org-agenda-custom-commands
  '(("n" "Agenda / INTR / PROG / NEXT"
     ((agenda "" nil)
      (todo "INTR" nil)
      (todo "PROG" nil)
      (todo "NEXT" nil))
     nil)))


;; (setq org-tag-alist '(("important" . ?i)))

(setq org-priority-default 67
      org-priority-lowest 69)

(setq org-enforce-todo-dependencies t)
(setq org-log-into-drawer t)

(setq org-agenda-custom-commands nil)
(add-to-list
 'org-agenda-custom-commands
 '("p" "Eisenhower Agenda (Personal)"
   ((todo "INTR" ((org-agenda-overriding-header "Interrupting with (INTR)")))
    (agenda "" ((org-agenda-overriding-header "Daily Agenda")))
    (tags-todo "important/PROG" ((org-agenda-overriding-header "Q1: Important PROG")))
    (tags-todo "important/NEXT" ((org-agenda-overriding-header "Q2: Important NEXT")))
    (tags-todo "-important/PROG" ((org-agenda-overriding-header "Q3: Unimportant PROG")))
    (tags-todo "-important/NEXT" ((org-agenda-overriding-header "Q4: Unimportant NEXT"))))
   ;; local setting
   ((org-agenda-files '("~/org/todo-personal.org" "~/org/diary.org" "~/org/inbox.org")))
   ))
(add-to-list
 'org-agenda-custom-commands
 '("w" "Eisenhower Agenda (Work)"
   ((todo "INTR" ((org-agenda-overriding-header "Interrupting with (INTR)")))
    (agenda "" ((org-agenda-overriding-header "Daily Agenda")))
    (tags-todo "important/PROG" ((org-agenda-overriding-header "Q1: Important PROG")))
    (tags-todo "important/NEXT" ((org-agenda-overriding-header "Q2: Important NEXT")))
    (tags-todo "-important/PROG" ((org-agenda-overriding-header "Q3: Unimportant PROG")))
    (tags-todo "-important/NEXT" ((org-agenda-overriding-header "Q4: Unimportant NEXT"))))
   ;; local setting
   ((org-agenda-files '("~/org/todo-work.org" "~/org/inbox.org")))
   ))
(add-to-list
 'org-agenda-custom-commands
 '("n" "Eisenhower Agenda (All)"
   ((todo "INTR" ((org-agenda-overriding-header "Interrupting with (INTR)")))
    (agenda "" ((org-agenda-overriding-header "Daily Agenda")))
    (tags-todo "important/PROG" ((org-agenda-overriding-header "Q1: Important PROG")))
    (tags-todo "important/NEXT" ((org-agenda-overriding-header "Q2: Important NEXT")))
    (tags-todo "-important/PROG" ((org-agenda-overriding-header "Q3: Unimportant PROG")))
    (tags-todo "-important/NEXT" ((org-agenda-overriding-header "Q4: Unimportant NEXT"))))
   ))

;; Trying to use the current window as agenda frame.
(setq org-agenda-window-setup 'current-window)

;; Use sticky agenda since I need different agenda views (personal and work) at the same time.
(setq org-agenda-sticky t)

;; Use an indirect buffer after <Tab> (org-agenda-goto) or <Enter> (org-agenda-switch-to).
;;
;; Also see https://emacs.stackexchange.com/a/17822
(advice-add 'org-agenda-goto :after
            (lambda (&rest args)
              (org-tree-to-indirect-buffer)))

(advice-add 'org-agenda-switch-to :after
            (lambda (&rest args)
              (org-tree-to-indirect-buffer)))

;; org-ref
(use-package org-ref
  :ensure t
  ;; :init
  ;; (use-package "org-ref-ivy")
  :config
    ;; again, we can set the default library
    (setq org-ref-default-bibliography "~/zotero/library.bib")
    ;; the default citation type of org-ref is cite:, but i use citet: much more often
    ;; i therefore changed the default type to the latter.
    (setq org-ref-default-citation-link "citet")
    ;; Point To Notes Directory
    (setq org-ref-notes-directory org_notes)
    ;; Enable internal cross referencing
    (setq org-latex-prefer-user-labels t)

    )

;; this is to use pdf-tools instead of doc-viewer
(use-package pdf-tools
  :ensure t
  ;; :pin manual ;; manually update
  ;; :load-path "site-lisp/pdf-tools/lisp"
  :defer t
  :init
  (pdf-loader-install)
  :config
  ;; this means that pdfs are fitted to width by default when you open them
  (setq-default pdf-view-display-size 'fit-width)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; Disablel line numbers mode in pdf-view-mode
  (defun my-disable-line-numbers ()
    "Disable line numbers in pdf-view-mode."
    (display-line-numbers-mode -1))
  (add-hook 'pdf-view-mode-hook 'my-disable-line-numbers)
  )

;; Opening last known position of pdf document
(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"
        use-file-base-name-flag nil
        )
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename org_notes))
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template
   (concat "${type:20} ${title:*} "
           (propertize "${tags:20}" 'face 'org-tag)))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)               ; search with: title, tags, notebook
  ("C-c n F" . my/counsel-rg-org-roam-directory) ; full text search
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ("C-c n r" . org-roam-node-random)
  ("C-c n t" . org-roam-tag-add)
  ("C-c n e" . org-roam-ref-add)
  ("C-c n E" . org-roam-ref-find)
  :map org-mode-map
  ("C-M-i" . completion-at-point))
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode)

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "return the type of node."
    (condition-case nil
        (directory-file-name
         (file-name-directory
          (file-relative-name (org-roam-node-file node) org-roam-directory)))
      (error "")))

  ;; change file-name (slug) creation
  ;; replace whitespace with dashes instead of underscores.
  ;; see
  ;; - https://github.com/org-roam/org-roam/issues/686
  ;; - https://github.com/org-roam/org-roam/pull/1544[[id:2022-06-12t213159.588064][test mest hest]]
  ;; - https://www.reddit.com/r/emacs/comments/veesun/orgroam_is_absolutely_fantastic/
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "return the slug of node."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; combining diacritical marks https://www.unicode.org/charts/pdf/u0300.pdf
                             768 ; u+0300 combining grave accent
                             769 ; u+0301 combining acute accent
                             770 ; u+0302 combining circumflex accent
                             771 ; u+0303 combining tilde
                             772 ; u+0304 combining macron
                             774 ; u+0306 combining breve
                             775 ; u+0307 combining dot above
                             776 ; u+0308 combining diaeresis
                             777 ; u+0309 combining hook above
                             778 ; u+030a combining ring above
                             779 ; u+030b combining double acute accent
                             780 ; u+030c combining caron
                             795 ; u+031b combining horn
                             803 ; u+0323 combining dot below
                             804 ; u+0324 combining diaeresis below
                             805 ; u+0325 combining ring below
                             807 ; u+0327 combining cedilla
                             813 ; u+032d combining circumflex accent below
                             814 ; u+032e combining breve below
                             816 ; u+0330 combining tilde below
                             817 ; u+0331 combining macron below
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric to dashes
                        ("--*" . "-")                   ;; remove sequential dashes
                        ("^-" . "")                     ;; remove starting dashes
                        ("-$" . "")))                   ;; remove ending dashes
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))

  ;; excludes all node with the "no_org_roam" tag from the org-roam database.
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "no_org_roam" (org-get-tags)))))
  (setq org-agenda-hide-tags-regexp "no_org_roam")

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))))

(setq org-roam-capture-templates nil)
(add-to-list 'org-roam-capture-templates
             '("m" "main note" plain "%?"
               :if-new
               (file+head "personal/main/%<%y%m%d>-${slug}.org" "#+title: ${title}\n\n")
               :unnarrowed t))

(add-to-list 'org-roam-capture-templates
             '("r" "reference note" plain "%?" ; (file "~/org/.templates/reference.org")
               :if-new
               (file+head "personal/ref/%<%y%m%d>-${slug}.org" "#+title: ${title}\n\n")
               :unnarrowed t))

;; org roam bibtex
(use-package org-roam-bibtex
  :ensure t
  :after org-roam)

(use-package org-roam-ui
  :disabled
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; org noter
(use-package org-noter
  :ensure t
  :after (:any org pdf-view)
  :config
  ;; open notes in other window
  (setq
   ;; org-noter-notes-window-location 'other-frame
        ;; stop opening new frames please
        org-noter-always-create-frame nil
        ;; See whole frame
        org-noter-hide-other nil
        ;; set path
        org-noter-notes-search-path (list org_notes)
        org-noter-separate-notes-from-heading t
        )
  )


;; Enable vertico
(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ;; ("M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("C-x l" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ("C-c g" . consult-git-grep)
         ("C-c r" . consult-ripgrep)
         ("C-s" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  ;; Project support
  ;; projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package consult-flycheck
  :ensure t)

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :config
  (defun kb/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (setq global-corfu-minibuffer nil)
  :hook
  (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :init
  (global-corfu-mode))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :ensure t
  :bind ("M-p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (setq cape-dabbrev-min-length 2)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

;; Corfu terminal
(use-package corfu-terminal
  :straight (corfu-terminal
             :type git
             :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :ensure t
  :config (unless (display-graphic-p)
            (corfu-terminal-mode +1)))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-upcase-means-case-search t)
  (dabbrev-check-all-buffers nil)
  (dabbrev-check-other-buffers t)
  (dabbrev-friend-buffer-function 'dabbrev--same-major-mode-p)
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Following two functions need for lsp booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  ;; Turn flycheck on everywhere
  (global-flycheck-mode t)
  ;; There are issues with company mode and flycheck in terminal mode.
  ;; This is outlined at:
  ;; https://github.com/abingham/emacs-ycmd
  ;; (when (not (display-graphic-p))
  ;;   (setq flycheck-indication-mode nil))
  (setq-default flycheck-disabled-checkers '(python-pyflakes
                                             python-flake8 python-pycompile))
  )

(use-package lsp-mode
  :ensure t
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  (defun my/lsp-mode-setup-completion()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape capf buster
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
    )
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (python-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (c-ts-mode . lsp)
         (cmake-ts-mode . lsp)
         (rust-ts-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         )
  :config
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-completion-provider :none)
  (defvar lsp-clients-clangd-args '("--clang-tidy"
                                    "--fallback-style=google"
                                    "-j=4"
                                    "--enable-config"
                                    "--suggest-missing-includes"
                                    "--pch-storage=memory"
                                    "--header-insertion=never"))
  :commands lsp)
;; optionally
(use-package lsp-ui
  :straight (lsp-ui
             :type git
             :host github
             :repo "8x4y2z/lsp-ui")
  :ensure t
  :commands lsp-ui-mode)

(use-package consult-lsp
  :ensure t
  :commands consult-lsp-symbols)

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; pyright instead of ms-pyls
(use-package lsp-pyright
  :after lsp-mode
  :ensure t
  :init
  (setq lsp-pyright-multi-root nil
        lsp-pyright-python-search-functions
        '(lsp-pyright--locate-python-python)
        )
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))); or lsp-deferred
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load hungry delete, caus we're lazy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :init
  ;; silence missing function warnings
  (declare-function global-hungry-delete-mode "hungry-delete.el")
  :config
  (global-hungry-delete-mode t)
  )

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package esup
  :ensure t
  :init
  (setq esup-child-max-depth 0)
  ;; Use a hook so the message doesn't get clobbered by other messages.
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (message "Emacs ready in %s with %d garbage collections."
              (format "%.2f seconds"
                      (float-time
                       (time-subtract after-init-time before-init-time)))
              gcs-done))))

(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  :init
  (setq nerd-icons-font-family "IosevkaNerdFont"
	nerd-icons-fonts-subdirectory "~/.local/share/fonts")
  )

(use-package nerd-icons-corfu
  :ensure t
  :after (:all corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  ;; (setq nerd-icons-corfu-mapping
  ;;       '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
  ;;         (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
  ;;         ;; ...
  ;;         (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; Optionally
        ;; Remember to add an entry for `t', the library uses that as default.
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function projectile-mode "projectile.el"))
  :config
  (projectile-mode t)
  (setq projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (defun my:projectile-find-file-dwim ()
    "Find the file in the project using DWIM, if not in the use find-file"
    (interactive)
    (if (projectile-project-p)
        (call-interactively 'projectile-find-file-dwim)
      (call-interactively 'projectile-find-file)))
  (global-set-key (kbd "C-x M-f") 'my:projectile-find-file-dwim)
  (setq projectile-completion-system 'default)
  )

(use-package consult-projectile
  :ensure t
  :after (:all consult projectile)
  :bind (("C-x M-f" . consult-projectile))
  )

;; use doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  )

;; Solarie mode to dim sidears and popups
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package winum (actively maintained replacement of window-numbering)
;; installed from package list. Allows switching between buffers using
;; meta-(# key)
(use-package winum
  :ensure t
  :init
  (defvar winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  :config
  (setq winum-scope 'frame-local)
  (winum-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :after  vertico
  :commands (magit-checkout)
  :bind (("M-g M-s" . magit-status)
         ("M-g M-c" . 'magit-checkout)
         )
  :init
  (use-package forge
    :ensure t
    :after magit)
  :config
  (setq magit-completing-read-function 'completing-read)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diff-hl
;;
;; git-gutter is no longer maintained so use diff-hl instead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         ;; (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         )
  :commands (diff-hl-mode)
  :init
  (use-package diff-hl-amend
    :commands (diff-hl-amend-mode)
    )
  (use-package diff-hl-dired
    :commands (diff-hl-dired-mode)
    )
  (use-package diff-hl-flydiff
    :commands (diff-hl-flydiff-mode)
    )
  (use-package diff-hl-margin
    :commands (diff-hl-margin-mode)
    )
  :config
  ;; use purple to show diffs
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (custom-set-faces
     '(diff-hl-change
       ((t (:background "#5f00af" :foreground "#5f00af")))))
  (diff-hl-margin-mode t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gitignore-mode: highlighting in gitignore files
;; gitignore mode is deprecated in favor of git-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-modes
  :ensure t
  :config
  ;; Set git-attributes-mode for .gitattributes files
  (add-to-list 'auto-mode-alist '("\\.gitattributes\\'" . git-attributes-mode))
  (add-to-list 'auto-mode-alist '("\\.git/info/attributes\\'" . git-attributes-mode))
  (add-to-list 'auto-mode-alist '("\\git/attributes\\'" . git-attributes-mode))

  ;; Set git-config-mode for git config files
  (add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\\.git/config\\'" . gitconfig-mode))
  (add-to-list 'auto-mode-alist '("\\git/config\\'" . gitconfig-mode))

  ;; Set gitignore-mode for git gitignore files
  (add-to-list 'auto-mode-alist '("\\.gitignore\\'" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("\\.git/info/exclude\\'" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("\\git/ignore\\'" . gitignore-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode
  :ensure t
  :mode ("cmakelists.txt" ".cmake")
  :hook (cmake-mode . (lambda ()
                        (add-to-list 'company-backends 'company-cmake)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bazel-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(if (not (file-exists-p "~/.emacs.d/plugins/bazel-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/codesuki/bazel-mode/master/bazel-mode.el"
     "~/.emacs.d/plugins/bazel-mode.el"))
(if (file-exists-p "~/.emacs.d/plugins/bazel-mode.el")
    (use-package bazel-mode
      :mode ("build" "\\.bazel\\'" "\\.bzl'" "workspace\\'")
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(if (not (file-exists-p "~/.emacs.d/plugins/protobuf-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/protocolbuffers/protobuf/master/editors/protobuf-mode.el"
     "~/.emacs.d/plugins/protobuf-mode.el"))
(if (file-exists-p "~/.emacs.d/plugins/protobuf-mode.el")
    (use-package protobuf-mode
      :mode ("\\.proto\\'")
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.imp\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'")
  :init
  (use-package flycheck-rust
    :ensure t
    :after rust-mode)

  :config
  (defun my:rust-mode-hook()
    (set (make-local-variable 'compile-command) "cargo run")
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    )
  (add-hook 'rust-mode-hook 'my:rust-mode-hook)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup dockerfile mode
;; 1. download file from github
;; 2. load mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p "~/.emacs.d/plugins"))
    (make-directory "~/.emacs.d/plugins"))
(if (not (file-exists-p "~/.emacs.d/plugins/dockerfile-mode.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/spotify/dockerfile-mode/master/dockerfile-mode.el"
     "~/.emacs.d/plugins/dockerfile-mode.el"))
(use-package dockerfile-mode
  :mode ("dockerfile"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package: yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; add snippet support to lsp mode
  (setq lsp-enable-snippet t)
  )
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load asm-mode when opening assembly files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package asm-mode
  :mode ("\\.s\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (define-key markdown-mode-map (kbd "M-p") nil)
  (define-key markdown-mode-map (kbd "M-n") nil)
  (setq markdown-command "pandoc -s --mathjax")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lua-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode))
  ;; :config
  ;; (add-hook 'lua-mode-hook #'company-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auctex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  ;; when we byte-compile we need to have the autoloads loaded in order to
  ;; properly get auctex working, otherwise auctex is not loaded correctly
  :init
  (load "auctex-autoloads" nil t)
  :config
  (setq-default tex-auto-save t
                tex-parse-self t
                tex-source-correlate-start-server t)
  (cond
   ((string-equal system-type "windows-nt") ; microsoft windows
    (progn
      (message "windows does not have a pdf viewer set for auctex")))
   ((string-equal system-type "darwin") ; mac os x
    (setq-default
     tex-view-program-list
     '(("skim"
        "/applications/skim.app/contents/sharedsupport/displayline -b -g %n %o %b")
       )
     tex-view-program-selection '((output-pdf "skim"))))
   ((string-equal system-type "gnu/linux") ; linux
    (setq-default tex-view-program-list
                  '(("evince" "evince --page-index=%(outpage) %o"))
                  tex-view-program-selection '((output-pdf "evince")))))
  (add-hook 'latex-mode-hook 'tex-source-correlate-mode)
  (add-hook 'latex-mode-hook 'auto-fill-mode)
  (add-hook 'latex-mode-hook 'flyspell-mode)
  (add-hook 'latex-mode-hook 'flyspell-buffer)
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (setq-default reftex-plug-into-auctex t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent bars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package indent-bars
  ;; :load-path "~/.emacs.d/git/indent-bars"
  :ensure t
  :config
  (require 'indent-bars-ts)
  :custom
  (indent-bars-treesit-support t)
  :hook ((python-mode
          yaml-mode
          c-mode
          c++-mode
          python-ts-mode
          c-ts-mode
          c++-ts-mode
          yaml-ts-mode) . indent-bars-mode)) ; or whichever modes you prefer

;; transpose windows
(use-package transpose-frame
  :ensure t
  )

;; plantuml
(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "/home/pupil/install/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq org-plantuml-jar-path (expand-file-name "/home/pupil/install/plantuml.jar"))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (with-eval-after-load "org"
    (add-to-list
     'org-src-lang-modes '("plantuml" . plantuml))
    )
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (setq plantuml-output-type 'svg)
  (defun my/plantuml-export-to-png ()
  "Export the current PlantUML buffer to a PNG file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (output-file (concat (file-name-base file-name) ".png"))
         (plantuml-command (format "java -jar %s %s -tpng"
                                   plantuml-jar-path
                                   file-name
                                   output-file))
         (compilation-buffer (generate-new-buffer "*PlantUML Export*")))

    ;; Run the PlantUML command
    (with-current-buffer compilation-buffer
      (insert (format "Running command: %s\n\n" plantuml-command))
      (let ((default-directory (or (file-name-directory file-name) default-directory)))
        (call-process-shell-command plantuml-command nil t))

      ;; Notify the user
      (message "PlantUML export to PNG completed. File saved to: %s" output-file)
      (kill-buffer compilation-buffer))))
    (define-key plantuml-mode-map (kbd "C-c C-e") 'my/plantuml-export-to-png)
  )

;; detour for quick navigating
(use-package detour
  :ensure t
  :bind
  (("C-c ." . detour-mark)
  ("C-c ," . detour-back))
  )

;; Expand package for quick selecting region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Whole line or region
;; Used to copy/paste lines when no region is active
(use-package whole-line-or-region
  :ensure t
  :hook(after-init . whole-line-or-region-global-mode)
  :config
  (with-eval-after-load 'whole-line-or-region
    (diminish 'whole-line-or-region-lcal-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm
  :load-path  "/home/pupil/.emacs-30.d/emacs-libvterm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now use multi-vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multi-vterm
  :ensure t)

;; Init ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-noise-macro-names '("constexpr"))
 '(custom-safe-themes
   '("e410458d3e769c33e0865971deb6e8422457fad02bf51f7862fa180ccc42c032"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "#5f00af" :foreground "#5f00af"))))
 '(mode-line ((t :height 140)))
 '(which-func ((t (:foreground "#8fb28f")))))
