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

;; Display line numbers only when in programming modes
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (column-number-mode)

;; Set warnings to silent
(setq native-comp-async-report-warnings-errors 'silent)

(global-display-line-numbers-mode)
(setq  display-line-numbers-type 'relative)

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
  :config
  (ef-themes-select 'ef-arbutus))

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
  ;; Set orderless as the main completion style
  (completion-styles '(orderless))
  ;; Allow finer control per completion category
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic orderless))    ; File completion uses `basic` + `orderless`
     (command (styles orderless))       ; `M-x` (command) uses `orderless`
     (buffer (styles orderless))        ; `C-x b` (buffer) uses `orderless`
     (variable (styles orderless)))))   ; Variables (`C-h v`) use `orderless`


;; Improve the accessibility of Emacs documentation by placing
;; descriptions directly in your minibuffer. Give it a try:
;; "M-x find-file".
(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

;; For -> etc.
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 't '("->" "<-" "=>" "<=" "==" "&&" "||" "->>" "<<-" "!="))
  (global-ligature-mode t))

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
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  ;; You may want to play with delay/prefix/styles to suit your preferences.
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 0)
  (completion-styles '(basic)))

;; Adds LSP support. Note that you must have the respective LSP
;; server installed on your machine to use it with Eglot. e.g.
;; rust-analyzer to use Eglot with `rust-mode'.
(use-package eglot
  :ensure t
  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix)))

(use-package lsp-mode
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
  :config

  (custom-set-variables
   '(org-directory "~/org")
   '(org-agenda-files (list org-directory)))
  
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
  (add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews))

;; kanske behöver dessa någon gång
;;(add-hook 'text-scale-mode-hook #'my/text-scale-adjust-latex-previews)
;;Run latex scale function after org latex preview
;;(advice-add 'org-latex-preview :after #'my/text-scale-adjust-latex-previews)
;;(add-hook 'org-latex-preview #'my/text-scale-adjust-latex-previews)

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
  :bind (("C-c s" . #'avy-goto-char-timer)))

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

