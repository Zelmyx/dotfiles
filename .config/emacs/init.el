;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:

;; Save the contents of this file to ~/.config/emacs/init.el and
;; you're ready to boot up Emacs.

;; Hack this file! One of the best ways to get started with Emacs is
;; to look at other peoples' configurations and extract the pieces
;; that work for you. That's where this configuration started. I
;; encourage you to read through the code in this file and explore the
;; functions and variables using the built-in help system (details
;; below). Happy hacking!

;; "C-<chr>  means hold the CONTROL key while typing the character <chr>.
;; Thus, C-f would be: hold the CONTROL key and type f." (Emacs tutorial)
;;
;; - C-h t: Start the Emacs tutorial
;; - C-h o some-symbol: Describe symbol
;; - C-h C-q: Pull up the quick-help cheatsheet

;;; Code:

(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; Initialize the package system
(unless package--initialized
  (package-initialize))

;; Only refresh if `use-package` isn't installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ;; Automatically install packages when needed

;; Java LSP
(add-to-list 'exec-path "~/opt/jdtls/org.eclipse.jdt.ls.product/target/repository/bin/")

;; Performance tweaks for modern machines
(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; Remove extra UI clutter by hiding the scrollbar, menubar, and toolbar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
 
;; Set the font. Note: height = px * 100
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 120)

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organization and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; Automatically insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

;; Prefer spaces to tabs
(setq-default indent-tabs-mode nil)

;; Automatically save your place in files
(save-place-mode t)

;; Save history in minibuffer to keep recent commands easily accessible
(savehist-mode t)

;; Keep track of open files
(recentf-mode t)

;; Keep files up-to-date when they change outside Emacs
(global-auto-revert-mode t)

;; Fit lines to screen size
(global-visual-line-mode t)

;; Display line numbers only when in programming modes
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (column-number-mode)

;; Set warnings to silent
(setq native-comp-async-report-warnings-errors 'silent)

;; Do not enable line numbers for pdf view mode
(require 'display-line-numbers)
(defun display-line-numbers--turn-on ()
  "Turn on `display-line-numbers-mode'."
  (unless (or (minibufferp) (eq major-mode 'pdf-view-mode))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode 1)
(setq  display-line-numbers-type 'absolute)

;; Set conifg directory
(setq user-emacs-directory "~/.config/emacs")
(setq user-init-file (expand-file-name "init.el" user-emacs-directory))

;; The `setq' special form is used for setting variables. Remember
;; that you can look up these variables with "C-h v variable-name".
(setq window-resize-pixelwise t
      frame-resize-pixelwise t
      load-prefer-newer t
      backup-by-copying t
      ;; Backups are placed into your Emacs directory, e.g. ~/.config/emacs/backups
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      ;; I'll add an extra note here since user customizations are important.
      ;; Emacs actually offers a UI-based customization menu, "M-x customize".
      ;; You can use this menu to change variable values across Emacs. By default,
      ;; changing a variable will write to your init.el automatically, mixing
      ;; your hand-written Emacs Lisp with automatically-generated Lisp from the
      ;; customize menu. The following setting instead writes customizations to a
      ;; separate file, custom.el, to keep your init.el clean.
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Bring in package utilities so we can install packages from the web.
(require 'package)

;; Add MELPA, an unofficial (but well-curated) package registry to the
;; list of accepted package registries. By default Emacs only uses GNU
;; ELPA and NonGNU ELPA, https://elpa.gnu.org/ and
;; https://elpa.nongnu.org/ respectively.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Unless we've already fetched (and cached) the package archives,
;; refresh them.
(unless package-archive-contents
  (package-refresh-contents))

;; Add the :vc keyword to use-package, making it easy to install
;; packages directly from git repositories.
;;(unless (package-installed-p 'vc-use-package)
;;  (package-vc-install "https://github.com/slotThe/vc-use-package"))
;;(require 'vc-use-package)

;; A quick primer on the `use-package' function (refer to
;; "C-h f use-package" for the full details).
;;
;; (use-package my-package-name
;;   :ensure t    ; Ensure my-package is installed
;;   :after foo   ; Load my-package after foo is loaded (seldom used)
;;   :init        ; Run this code before my-package is loaded
;;   :bind        ; Bind these keys to these functions
;;   :custom      ; Set these variables
;;   :config      ; Run this code after my-package is loaded

;; A package with a great selection of themes:
;; https://protesilaos.com/emacs/ef-themes
(use-package ef-themes
  :ensure t
  :init
  ;; Override the palette with Oceanic-Material colors
  (setq ef-themes-common-palette-overrides
        '((bg-main    "#282C34")
          (bg-alt     "#1B2B34")
          (fg-main    "#C0C5CE")
          (fg-dim     "#5F676A")
          (cursor     "#6699CC")

          (red        "#EC5F67")
          (red-warmer "#F2777A")

          (green      "#A9B665")
          (green-cooler "#B5BD68")

          (yellow     "#D8A657")
          (yellow-warmer "#F0C674")

          (blue       "#6699CC")
          (blue-warmer "#81A2BE")

          (magenta    "#C594C5")
          (magenta-warmer "#B294BB")

          (cyan       "#62B3B2")
          (cyan-warmer "#8ABEB7")))
  :config
  ;; Pick a base dark theme and apply overrides
  (ef-themes-select 'ef-night))

;; Minibuffer completion is essential to your Emacs workflow and
;; Vertico is currently one of the best out there. There's a lot to
;; dive in here so I recommend checking out the documentation for more
;; details: https://elpa.gnu.org/packages/vertico.html. The short and
;; sweet of it is that you search for commands with "M-x do-thing" and
;; the minibuffer will show you a filterable list of matches.
(use-package vertico
  :ensure t
  :after minibuffer
  :commands vertico-mode
  :init (vertico-mode 1)
  :bind (:map vertico-map
         ("M-RET"   . nil)
         ("M-s"     . nil)
         ("M-i"     . vertico-insert)
         ("C-M-n"   . vertico-next-group)
         ("C-M-p"   . vertico-previous-group)
         ("C-j"     . (lambda () (interactive)
	        	(if minibuffer--require-match
	        	    (minibuffer-complete-and-exit)
	        	  (exit-minibuffer)))))
  :config
  (setq vertico-count 10
        vertico-cycle t
        vertico-resize t)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (advice-add #'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args))))
  :custom
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex)))

;; Orderless configuration
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode 1)
  :bind (:map vertico-map
         ("M-]" . marginalia-cycle)) ;; Not used
  :config
  (pcase-dolist (`(,regexp . ,category)
                 '(("\\burl\\b" . url)
                   ("\\bHistory\\b" . history)
                   ("\\bdefinitions?\\b" . xref-location)
                   ("\\bxref\\b" . xref-location)))
    (setf (alist-get regexp marginalia-prompt-categories
                     nil nil #'equal)
          category)))

;; For -> etc.
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 't '("->" "<-" "=>" "<=" "==" "&&" "||" "->>" "<<-" "!="))
  (global-ligature-mode t))

(use-package ace-window
  :ensure t
  :config
  (setq aw-dispatch-always  t)
  :bind (("M-o" . ace-window)
         ("C-x o" . ace-window)))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-s g"   . consult-grep)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-y" . consult-yank-pop)
         :map ctl-x-r-map
         ("b" . consult-bookmark)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)))

;; All the icons *NOT WORKING*
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :ensure t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Adds intellisense-style code completion at point that works great
;; with LSP via Eglot. You'll likely want to configure this one to
;; match your editing preferences, there's no one-size-fits-all
;; solution.
(use-package corfu
  :ensure t
  :hook (((prog-mode text-mode tex-mode ielm-mode) . corfu-mode))
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("M-RET" . corfu-insert)
         ("M-." . corfu-show-location)
         ("M-." . corfu-info-location)
         ("C-h" . corfu-info-documentation))
  :config
  (setq corfu-auto-prefix 2
        corfu-auto-delay 0.07
        corfu-count 8
        corfu-auto  t
        corfu-cycle t
        corfu-quit-no-match 'separator
        corfu-preselect 'prompt
        corfu-scroll-margin 5))

;; Adds LSP support. Note that you must have the respective LSP
;; server installed on your machine to use it with Eglot. e.g.
;; rust-analyzer to use Eglot with `rust-mode'.
(use-package eglot
  :ensure t
  :commands eglot
  :bind (:map eglot-mode-map
              ("C-h ." . eldoc)
              ("C-c f" . eglot-format-buffer))
  :hook ((eglot-managed-mode . my/eglot-eldoc-settings)
         (prog-mode . eglot-ensure))
  :config
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  (setq eglot-extend-to-xref t)
  (add-to-list 'eglot-server-programs
             '((c-mode c++-mode objc-mode) . ("clangd"))))

;; (use-package eglot-booster
;;   :ensure t
;;   :if (executable-find "emacs-lsp-booster")
;;   :custom (eglot-booster-io-only t)
;;   :after eglot
;;   :init (eglot-booster-mode))

(use-package editorconfig
  :ensure t                ; pull from MELPA
  :hook (prog-mode . editorconfig-mode) ; enable for all programming modes
  :config
  ;; Optional: tighten the search depth (default is 5)
  (setq editorconfig-search-depth 10))

(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo)))

(use-package lsp-mode
  :disabled
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode python-mode) . lsp)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-completion-provider :capf)
  (setq lsp-completion-enable-additional-text-edit t)
  (setq lsp-clients-clangd-args '("--fallback-style=WebKit"))
  (setq lsp-pylsp-plugins-black-enabled t)
  (setq lsp-pylsp-server-command '("/home/edvin/.local/bin/pylsp"))
  )

;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings 
;; recommended by the package author.
(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Nerd icons
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; PDF tools
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;; Minimal Flyspell setup
(setq ispell-program-name "aspell")        ; Use aspell as the spell checker
(setq ispell-dictionary "en_GB")          ; Set the dictionary to British English
(add-hook 'LaTeX-mode-hook 'flyspell-mode) ; Enable Flyspell in LaTeX mode
(setq ispell-local-dictionary-alist
      '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "'" nil ("-d" "en_GB") nil utf-8)))

;; AucTex
(use-package auctex
  :defer t
  :ensure t
  :hook ((LaTeX-mode . turn-on-reftex)    ; Enable RefTeX for citations and cross-references
         (LaTeX-mode . LaTeX-math-mode)  ; Enable math mode for easy math input
         (LaTeX-mode . visual-line-mode) ; Enable visual line wrapping
         (LaTeX-mode . flyspell-mode))   ; Enable spell checking
  :config
  (setq TeX-auto-save t)                 ; Automatically save style information
  (setq TeX-parse-self t)                ; Automatically parse files
  (setq TeX-master nil)                  ; Query for master file
  (setq TeX-PDF-mode t)                  ; Use PDF mode by default
  (setq reftex-plug-into-AUCTeX t)       ; Integrate RefTeX with AUCTeX
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))) ; Use PDF Tools for viewing
  (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (setq TeX-source-correlate-start-server t) ; Start server for PDF Tools
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . org-cdlatex-mode))
  :bind (:map LaTeX-mode-map
         ("C-S-e" . latex-math-from-calc))
  :config
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0) 
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

;; CDLatex settings
(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map 
              ("<tab>" . cdlatex-tab))
  :config
  ;; (setq cdlatex-math-symbol-prefix 180)
  
  (defun my/cdlatex-custom-commands ()
    (add-to-list 'cdlatex-command-alist
                 '("lim" "limits" "\\lim_{? \\to }" cdlatex-position-cursor nil nil t))
    (add-to-list 'cdlatex-command-alist
                 '("in" "in" "? \\in" cdlatex-position-cursor nil nil t))
    (add-to-list 'cdlatex-command-alist
                 '("fin" "for all in" "\\forall ? \\in " cdlatex-position-cursor nil nil t))
    (add-to-list 'cdlatex-math-modify-alist
                 '(?B "\\mathbb" nil t nil nil))
    (add-to-list 'cdlatex-math-modify-alist
                 '(?n "\\lVert ? \\rVert_{}" nil t nil nil))
    (add-to-list 'cdlatex-math-modify-alist
                 '(?a "\\lvert ? \\rvert_{}" nil t nil nil)))
  (add-hook 'cdlatex-mode-hook #'my/cdlatex-custom-commands))

;; Yasnippet settings
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t)
  
  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
           ("<tab>" . yas-next-field-or-cdlatex)
           ("TAB" . yas-next-field-or-cdlatex))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))


;; Array/tabular input with org-tables and cdlatex 
(use-package org-table
  :ensure nil
  :after cdlatex
  :bind (:map orgtbl-mode-map
              ("<tab>" . lazytab-org-table-next-field-maybe)
              ("TAB" . lazytab-org-table-next-field-maybe))
  :init
  (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
  ;; Tabular environments using cdlatex
  (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
                                       "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
                                       "\\begin{bmatrix} ? \\end{bmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
                                       "\\begin{pmatrix} ? \\end{pmatrix}"
                                       lazytab-position-cursor-and-edit
                                       nil nil t))
  (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
                                        "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
                                       lazytab-position-cursor-and-edit
                                       nil t nil))
  :config
  ;; Tab handling in org tables
  (defun lazytab-position-cursor-and-edit ()
    ;; (if (search-backward "\?" (- (point) 100) t)
    ;;     (delete-char 1))
    (cdlatex-position-cursor)
    (lazytab-orgtbl-edit))

  (defun lazytab-orgtbl-edit ()
    (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
    (orgtbl-mode 1)
    (open-line 1)
    (insert "\n|"))

  (defun lazytab-orgtbl-replace (_)
    (interactive "P")
    (unless (org-at-table-p) (user-error "Not at a table"))
    (let* ((table (org-table-to-lisp))
           params
           (replacement-table
            (if (texmathp)
                (lazytab-orgtbl-to-amsmath table params)
              (orgtbl-to-latex table params))))
      (kill-region (org-table-begin) (org-table-end))
      (open-line 1)
      (push-mark)
      (insert replacement-table)
      (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
      (orgtbl-mode -1)
      (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))
  
  (defun lazytab-orgtbl-to-amsmath (table params)
    (orgtbl-to-generic
     table
     (org-combine-plists
      '(:splice t
                :lstart ""
                :lend " \\\\"
                :sep " & "
                :hline nil
                :llend "")
      params)))

  (defun lazytab-cdlatex-or-orgtbl-next-field ()
    (when (and (bound-and-true-p orgtbl-mode)
               (org-table-p)
               (looking-at "[[:space:]]*\\(?:|\\|$\\)")
               (let ((s (thing-at-point 'sexp)))
                 (not (and s (assoc s cdlatex-command-alist-comb)))))
      (call-interactively #'org-table-next-field)
      t))

  (defun lazytab-org-table-next-field-maybe ()
    (interactive)
    (if (bound-and-true-p cdlatex-mode)
        (cdlatex-tab)
      (org-table-next-field))))


;; An extremely feature-rich git client. Activate it with "C-c g".
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package julia-mode
  :ensure t)

;;(use-package ob-julia
;;  :ensure t)

;; Ensure org-mode is loaded using use-package
(use-package org
  :ensure t
  :bind (("C-c l" . #'org-store-link)
         ("C-c a" . #'org-agenda)
         ("C-c c" . #'org-capture)
         ("C-c m" . my/insert-inline-math))
  :config
  (defun my/insert-inline-math ()
    "Insert \"\\(\\)\" and leave the point in the middle."
    (interactive)
    (insert "\\(\\)")
    (backward-char 2))
  
  (custom-set-variables
   '(org-directory "~/org")
   '(org-agenda-files (list org-directory)))

  ;; Org-capture note file
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  ;; Auto turn on cdlatex
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  
  ;; TODO keywords
  (setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
  ;; Enable C support in Org Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)))

  ;; Set the default LaTeX preview process to 'dvisvgm'
  (setq org-preview-latex-default-process 'dvisvgm)

  ;; Set scale for org-mode LaTeX previews
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  
  ;; Define the LaTeX scaling function
  ;; hack to scale latex preview with font size (https://karthinks.com/software/scaling-latex-previews-in-emacs/)
  (defun my/text-scale-adjust-latex-previews (&rest _)
    "Adjust the size of latex preview fragments when changing the buffer's text scale."
    (pcase major-mode
      ('latex-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'category)
                 'preview-overlay)
             (my/text-scale--resize-fragment ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'org-overlay-type)
                 'org-latex-overlay)
             (my/text-scale--resize-fragment ov))))))

  ;; Function to resize fragments
  (defun my/text-scale--resize-fragment (ov)
    (let ((scale-amount (or text-scale-mode-amount 0)))
    (overlay-put
     ov 'display
     (cons 'image
           (plist-put
            (cdr (overlay-get ov 'display))
            :scale (+ 1.0 (* 0.25 scale-amount)))))))

  ;; Hook to adjust LaTeX preview size on text scale
  (add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)

  ;; https://orgmode.org/worg/org-tutorials/org-latex-export.html
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("my-article"
                   "
% -------------------------------------------------
%   Modern Math‑Notes Preamble (pdfLaTeX)
% -------------------------------------------------
\\documentclass[11pt,a4paper]{article}   % base class – feel free to change options

% ---------- Page layout ----------
\\usepackage[margin=1in,headsep=0.2in]{geometry}
\\usepackage{parskip}                    % vertical space between paragraphs, no indent
\\usepackage{microtype}                  % subtle typographic tweaks

% ---------- Fonts ----------
% Choose a clean serif + matching sans‑serif + mono family.
% Options shown: Palatino (mathpazo) or Times (newtx). Uncomment the pair you prefer.

%--- Palatino family -------------------------------------------------
\\usepackage{mathpazo}                   % Palatino text + matching math fonts
\\renewcommand{\\sfdefault}{pplj}         % Palatino‑like sans‑serif
%---------------------------------------------------------------------

%--- Times family ----------------------------------------------------
% \\usepackage{newtxtext,newtxmath}       % Times text + matching math fonts
% \\renewcommand{\\sfdefault}{lmss}        % Latin Modern Sans (good complement)
%---------------------------------------------------------------------

% ---------- Core math packages ----------
\\usepackage{amsmath,amssymb,amsthm}      % classic AMS tools
\\usepackage{mathtools}                  % extensions to amsmath
\\usepackage{bm}                         % bold symbols in math mode
\\usepackage{physics}                    % handy shortcuts (\\dv, \\pdv, \\abs, …)

% ---------- Theorem styles ----------
\\newtheoremstyle{modern}% name
  {6pt}%                Space above
  {6pt}%                Space below
  {\\itshape}%           Body font
  {}%                   Indent amount
  {\\bfseries}%          Head font
  {}%                  Punctuation after head
  {}%                  Space after head
  {}%                   Head spec (empty = “name”)
\\theoremstyle{modern}
\\newtheorem*{theorem}{}
\\newtheorem*{lemma}{}
\\newtheorem*{prop}{}
\\newtheorem*{corollary}{}
\\theoremstyle{definition}
\\newtheorem*{definition}{}
\\newtheorem*{example}{}
\\newtheorem*{exercise}{}
\\theoremstyle{remark}
\\newtheorem*{remark}{}

% ---------- Section formatting ----------
\\usepackage{titlesec}
\\titleformat{\\section}
  {\\normalfont\\Large\\bfseries}{\\thesection}{1em}{}
\\titleformat{\\subsection}
  {\\normalfont\\large\\bfseries}{\\thesubsection}{1em}{}
\\titleformat{\\subsubsection}
  {\\normalfont\\normalsize\\bfseries}{\\thesubsubsection}{1em}{}

% ---------- Hyperlinks ----------
\\usepackage[hidelinks]{hyperref}       % hide default boxes
\\hypersetup{
    colorlinks=true,
    linkcolor=MidnightBlue,
    citecolor=MidnightBlue,
    urlcolor=MidnightBlue,
    pdftitle={My Math Notes},
    pdfauthor={Your Name}
}

% ---------- Clever references ----------
\\usepackage{cleveref}
\\crefname{equation}{eq.}{eqs.}
\\Crefname{equation}{Equation}{Equations}

% ---------- Miscellaneous ----------
\\usepackage{enumitem}
\\setlist[itemize]{noitemsep, topsep=0pt}
\\setlist[enumerate]{label=\\arabic*.}

         [NO-DEFAULT-PACKAGES]
         [PACKAGES]
         [EXTRA]"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  )

;; kanske behöver dessa någon gång
;;(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)
;;Run latex scale function after org latex preview
;;(advice-add 'org-latex-preview :after #'my/text-scale-adjust-latex-previews)
;;(add-hook 'org-latex-preview #'my/text-scale-adjust-latex-previews)

(use-package rainbow-mode
  :ensure t)

;; colours parenthesis different colours to easily distinguish
;; related parenthesis
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
    :ensure t
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.3))

(use-package org-drill
  :ensure t
  :config
  (add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews))

;; fix for org-drill timestamps
;; https://www.reddit.com/r/orgmode/comments/1g51wtc/orgdrill_gives_me_the_error_not_an_org_time_string/
(defun org-drill-time-to-inactive-org-timestamp (time)
  "Convert TIME into org-mode timestamp."
  (format-time-string
   (concat "[" (cdr org-time-stamp-formats) "]")
   time))

(use-package rust-mode
  :ensure t)

(use-package avy
  :ensure t
  :bind (("M-s s" . #'avy-goto-char-timer)
         ("M-s M-s" . #'avy-goto-char-timer)))

(use-package elfeed
  :ensure t)

(use-package calc
  :defer t)

;; this package can bring up helpful UI for many different modes
;; I have only configured it for calc-mode
;; Isearch doesn't work.
;; (use-package casual
;;    :ensure t
;;    :bind (:map
;;           calc-mode-map
;;           ("C-o" . casual-calc-tmenu)
;;           :map
;;           calc-alg-map
;;           ("C-o" . casual-calc-tmenu)
;;           :map
;;           isearch-mode-map
;;           ("C-o" . casual-isearch-tmenu))
;;    :after (calc))

;; C
(c-add-style "effective-c-style"
	     '("gnu"
	       (c-basic-offset . 8)	; Guessed value
	       (c-offsets-alist
		(block-close . 0)	; Guessed value
		(class-close . 0)	; Guessed value
		(defun-block-intro . +)	; Guessed value
		(defun-close . 0)	; Guessed value
		(defun-open . 0)	; Guessed value
		(else-clause . 0)	; Guessed value
		(inclass . +)		; Guessed value
		(statement . 0)		    ; Guessed value
		(statement-block-intro . +) ; Guessed value
		(substatement . +)	    ; Guessed value
		(topmost-intro . 0)	    ; Guessed value
		(access-label . -)
		(annotation-top-cont . 0)
		(annotation-var-cont . +)
		(arglist-close . c-lineup-close-paren)
		(arglist-cont c-lineup-gcc-asm-reg 0)
		(arglist-cont-nonempty . c-lineup-arglist)
		(arglist-intro . c-lineup-arglist-intro-after-paren)
		(block-open . 0)
		(brace-entry-open . 0)
		(brace-list-close . 0)
		(brace-list-entry . 0)
		(brace-list-intro first c-lineup-2nd-brace-entry-in-arglist c-lineup-class-decl-init-+ +)
		(brace-list-open . +)
		(c . c-lineup-C-comments)
		(case-label . 0)
		(catch-clause . 0)
		(class-open . 0)
		(comment-intro . c-lineup-comment)
		(composition-close . 0)
		(composition-open . 0)
		(cpp-define-intro c-lineup-cpp-define +)
		(cpp-macro . -1000)
		(cpp-macro-cont . +)
		(do-while-closure . 0)
		(extern-lang-close . 0)
		(extern-lang-open . 0)
		(friend . 0)
		(func-decl-cont . +)
		(incomposition . +)
		(inexpr-class . +)
		(inexpr-statement . +)
		(inextern-lang . +)
		(inher-cont . c-lineup-multi-inher)
		(inher-intro . +)
		(inlambda . 0)
		(inline-close . 0)
		(inline-open . 0)
		(inmodule . +)
		(innamespace . +)
		(knr-argdecl . 0)
		(knr-argdecl-intro . 5)
		(label . 0)
		(lambda-intro-cont . +)
		(member-init-cont . c-lineup-multi-inher)
		(member-init-intro . +)
		(module-close . 0)
		(module-open . 0)
		(namespace-close . 0)
		(namespace-open . 0)
		(objc-method-args-cont . c-lineup-ObjC-method-args)
		(objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
		(objc-method-intro .
				   [0])
		(statement-case-intro . +)
		(statement-case-open . +)
		(statement-cont . +)
		(stream-op . c-lineup-streamop)
		(string . -1000)
		(substatement-label . 0)
		(substatement-open . +)
		(template-args-cont c-lineup-template-args +)
		(topmost-intro-cont first c-lineup-topmost-intro-cont c-lineup-gnu-DEFUN-intro-cont))))

(defun my-c-mode-common-hook ()
  (c-set-style "effective-c-style")
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun create-header-file ()
  "Automatically generate a header file from the current C source file."
  (interactive)
  (let* ((c-file (buffer-file-name))
         (header-file (concat (file-name-sans-extension c-file) ".h")))
    (find-file header-file)
    (unless (file-exists-p header-file)
      (insert (concat "#ifndef " (upcase (file-name-base header-file)) "_H\n"
                      "#define " (upcase (file-name-base header-file)) "_H\n\n"
                      "#endif /* " (upcase (file-name-base header-file)) "_H */\n"))
      (goto-char (point-min))
      (search-forward "#define")
      (newline))))

