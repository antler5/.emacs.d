;; init.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: (c) 2024 antlers <antlers@illucid.net>

;; Substantial portions inspired by:
;; - [doomemacs](https://github.com/doomemacs/doomemacs)
;; - emacs-bedrock
;; - mnewt's dotemacs repo
;; - kristofferbalintona.me

;;; Commentary:

;; The code is organized into pages, separated by form feed characters.

;;; Code:


;;; Optimization from doom-start.el
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq idle-update-delay 1.0)

;; Apparently can make posframes more responsive, cool
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Don't ping things that look like domain names when using
;; find-file-at-point.
(setq ffap-machine-p-known 'reject)

;; Scolling optimization and tweaks
;; (setq jit-lock-defer-time 0)
;; (setq redisplay-skip-fontification-on-input t)
(setq redisplay-dont-pause t)
(setq mouse-wheel-progressive-speed nil)

;; Ooo, yay! UTF-8!
(set-language-environment "UTF-8")
(setq default-input-method nil)


;;; Use-Package
;; This refers to my fork with `:custom-face <string> [...]`.
'(:guix emacs-use-package-fork)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Some hooks are un-usable under the default scheme.
(setq use-package-hook-name-suffix nil)

;; Setup `:guix` as a no-op use-package keyword
;; (It's pulled out by a stand-alone script.)
(push ':guix use-package-keywords)
(defun use-package-normalize/:guix (_ keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg) '())))
(defun use-package-handler/:guix (name-symbol keyword archive-name rest state)
  (use-package-process-keywords name-symbol rest state))


;;; Packages with `use-package' extensions or no autoloads
(use-package dash :guix emacs-dash)
(use-package general
  :guix emacs-general
  :config (general-evil-setup))


;;; Guix Integration
(use-package guix
  :guix    (emacs-guix guile)
  :general (evil-leader-map "G" 'guix)
  :custom  (global-guix-prettify-mode t))

;; Register privileged binaries
(defun antlers/append-to-path (dir &optional path)
  (let ((path (or path "PATH")))
    (when (file-directory-p dir)
      (setenv path
        (-> (getenv path)
            (parse-colon-path)
            (append (list dir))
            (delete-dups)
            (string-join ":")))
      (when (string-equal path "PATH")
        (add-to-list 'exec-path dir))
      (getenv path))))
(antlers/append-to-path "/run/privileged/bin/")
(antlers/append-to-path "/run/setuid-programs/")


;;; Garbage Collection Pt. 2
;; (Re: `early-init.el`)
(use-package gcmh
  :guix emacs-gcmh
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (gcmh-mode t))


;;; Native Emacs Configuration
(use-package emacs
  :custom
  (help-window-keep-selected t)        ; Re-use help buffer
  (select-enable-clipboard t)          ; Merge System and Emacs clipboard
  (tab-always-indent 'complete)        ; Preferred TAB behavior
  (custom-file                         ; Disable custom-file persistence
   (make-temp-file "custom-" nil ".el"))

  ;; Cursor and Windows
  (blink-cursor-mode nil)              ; Disable cursor blinking
  (cursor-in-non-selected-windows nil) ; Hide inactive window's cursor
  (help-window-select t)               ; Focus new help windows when opened
  (highlight-nonselected-windows nil)  ; Hide inactive window's active region

  ;; Preferences
  (recenter-positions '(5 top bottom)) ; Set re-centering positions
  (sentence-end-double-space nil)      ; Let one space end a sentence
  (visible-bell nil)                   ; Disable visual-bell

  ;; Personal
  (user-full-name "antlers")
  (user-mail-address "antlers@illucid.net")

  ;; Responsiveness
  (echo-keystrokes 0.001)                   ; Display prefixes in minibuffer instantly
  (switch-to-buffer-obey-display-actions t) ; "Make switching buffers more consistent"

  ;; Minor modes
  (global-so-long-mode t)   ; Disable major + some minor modes in large files
  (repeat-mode         t)   ; Enable repeat-maps
  (save-place-mode     t)   ; Remember cursor position
  (savehist-mode       t)   ; Save history of minibuffer
  (tooltip-mode        nil) ; Disable tooltips
  (truncate-lines      t)   ; Truncate lines by default

  ;; Startup
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)

  ;; Debug, Warnings, and Errors
  (native-comp-async-report-warnings-errors nil)

  ;; Leader-key shortcuts
  :init
  (defun antlers/find-file (path)
    (lambda ()
      (interactive)
      (find-file path)))
  (defvar antlers/init.el
    (concat (getenv "HOME")
            "/.emacs.d/init.el"))

  :general ("<wheel-left>" #'(lambda () (interactive) (scroll-left 1))
            "<wheel-right>" #'(lambda () (interactive) (scroll-right 1)))
           (evil-leader-map
            "s" #'scratch-buffer
            "e" `("init.el" . ,(antlers/find-file antlers/init.el)))

  :config
  (defalias 'yes-or-no-p 'y-or-n-p) ; Replace yes/no prompts with y/n

  ;; GPG / Pinentry
  (setq epa-pinentry-mode 'loopback)
  (defun pinentry-emacs (desc prompt ok error)
    (concat (replace-regexp-in-string "%22" "\""
              (replace-regexp-in-string "%0A" "\n" desc))
            prompt ": "))

  ;; Correct forward-page behavior on page delimiters
  (advice-add 'forward-page :before
    (lambda (&optional _count)
      (when (and (looking-at page-delimiter)
                 (> (match-end 0) (point)))
        (forward-char 1))))

  (defun antlers/grep-elisp-load-path (regex)
    (interactive (list (read-shell-command "Regex: " nil 'grep-history)))
    (->> (elisp-load-path-roots)
         (-filter #'file-exists-p)
         (mapcar #'shell-quote-argument)
         (append `("grep" "-R" ,(shell-quote-argument regex)))
         (funcall (-flip #'string-join) " ")
         (grep-find))))

;; Default Tabs & Indents
(use-package emacs
  :custom
  (tab-width 2)
  (indent-tabs-mode nil)
  ;; Must come last to use modified `tab-width'
  (tab-stop-list (number-sequence tab-width 120 tab-width))
  :config
  (add-to-list 'warning-suppress-types
    '(defvaralias losing-value lisp-indent-offset))
  (-map (-cut defvaralias <> 'tab-width)
        '(c-basic-offset
          css-indent-offset
          evil-shift-width
          lisp-indent-offset
          sh-basic-offset)))

(use-package autorevert
  :custom
  (auto-revert-use-notify nil)
  (auto-revert-avoid-polling t)
  (auto-revert-check-vc-info t)
  (auto-revert-interval 5)
  (global-auto-revert-mode t))

(use-package man
  :guix gawk sed
  :custom-face
  (Man-overtrike ((t (:inherit font-lock-type-face :bold t))))
  (Man-underline ((t (:inherit font-lock-keyword-face :underline t)))))


;; XXX: Breaks daemon init :c
;; ;; Dashboard
;; ;; TODO: easy link to eshell / sync dired+dashboard+bookmarks? (remember harpoon?)
;; ;; TODO: Make "r" into an avy-line-like command?
;; (use-package dashboard
;;   :guix emacs-dashboard
;;   :general (evil-leader-map "\\" #'dashboard-open)
;;   :config
;;   (dashboard-setup-startup-hook)
;;   :init
;;   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;;   :custom
;;   (dashboard-startup-banner
;;     (expand-file-name "Mx-butterfly.png" user-emacs-directory))
;;   (dashboard-footer-messages '("Someone loves you <3")))


;; Mode-line
(use-package emacs
  :config
  (defun spaceline--column-number-at-pos (pos)
    "Column number at POS.  Analog to `line-number-at-pos'."
    (save-excursion (goto-char pos) (current-column)))
  (defun spaceline--selection-info ()
    "Information about the size of the current selection, when applicable.
  Supports both Emacs and Evil cursor conventions."
    (if (or mark-active
              (and (bound-and-true-p evil-local-mode)
                   (eq 'visual evil-state)))
        (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
               (chars (- (1+ (region-end)) (region-beginning)))
               (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
                                 (spaceline--column-number-at-pos (region-beginning))))))
               (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
               (rect (or (bound-and-true-p rectangle-mark-mode)
                         (and evil (eq 'block evil-visual-selection))))
               (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
          (cond
           (rect (format " %d×%d " lines (if evil cols (1- cols))))
           (multi-line (format " %d/%d " lines chars))
           (t (format " 1×%d " (if evil chars (1- chars))))))
      " %l:%c ")))

(use-package moody
  :guix emacs-moody
  :after nerd-icons
  :custom
  (display-time-default-load-average nil)
  (moody-mode-line-height 20) ; ~1ch
  (x-underline-at-descent-line t)
  :custom-face
  (mode-line ((t (:overline "#666666"
                  :underline "#666666"
                  :foreground "#fef8ea")))) ; warmer text
  (mode-line-inactive ((t :background "#383838")))
  :ghook ('emacs-startup-hook #'antlers/set-mode-line-format)
  :config
  ;; Mostly from nano-modeline.el
  (defun antlers/mode-line-status (&optional status)
    (cl-flet ((icon (lambda (i)
                      (nerd-icons-faicon i))))
      (pcase (format-mode-line "%*")
        ("*"   (icon "nf-fa-chain_broken"))
        ("-"   (icon "nf-fa-link"))
        ("%"   (icon "nf-fa-lock")))))
  (defun antlers/mode-line-percent (&optional buffer)
    (format "%-7s "
      (format "(%d%%%%/%d)"
        (/ (window-start) 0.01 (point-max))
        (if buffer
            (car (buffer-line-statistics buffer))
          (- (line-number-at-pos (point-max)) 1)))))
  (defun antlers/mode-line-vcs ()
    (when vc-mode
      (when-let* ((file (buffer-file-name))
                  (limit (- (window-total-width) 67)) ; XXX: Lazy
                  (branch (substring-no-properties vc-mode 5))
                  (branch (if (< (string-bytes branch) limit)
                              branch
                            (concat (string-limit
                                      branch
                                      (- limit 3))
                                    "...")))
                  (state (vc-state file)))
        (format "%s %s, %s"
          (nerd-icons-devicon "nf-dev-git_branch")
          branch state))))
  (defun antlers/mode-line-file-size ()
    (if-let* ((file-name (buffer-file-name))
              (file-attributes (file-attributes file-name))
              (file-size (file-attribute-size file-attributes))
              (file-size (file-size-human-readable file-size)))
        (format " (%s)" file-size)
      ""))
  (defun antlers/mode-line-dedicated ()
    (cond ((not (window-dedicated-p)) "")
          (t (list (nerd-icons-octicon "nf-oct-pin") " "))))
  (moody-replace-eldoc-minibuffer-message-function)
  (defun antlers/mode-line-format (title center right end)
    `(" "
      (:eval (antlers/mode-line-dedicated))
      (:eval (antlers/mode-line-status))
      moody-mode-line-front-space
      ,@(if title
            `((:eval
               (moody-tab
                 (concat (car (propertized-buffer-identification ,title))
                         (antlers/mode-line-file-size))
                 20 'down)))
          '())
      " "
      ,@center
      mode-line-format-right-align
      ,@right
      (:eval (moody-wrap (or evil-mode-line-tag " <?> ")))
      ,@end))
  (defun antlers/set-mode-line-format ()
    (setq-default mode-line-format
      (antlers/mode-line-format
        '(buffer-name)
        '((:eval (antlers/mode-line-vcs)))
        '((:eval (spaceline--selection-info))
          (:eval (antlers/mode-line-percent)))
        '("  ")))
    (-map (lambda (b)
            (when (not (buffer-local-value 'mode-line-format b))
              (setf (buffer-local-value 'mode-line-format b)
                (default-value 'mode-line-format))))
      (buffer-list))))


;; Theme, Graphics, and Fringe
(use-package emacs
  :guix (fontconfig ; needs specified, else `guix shell` won't export search path
         font-mononoki)
  :custom (indicate-buffer-boundaries 'left)
  :custom-face
  (default        ((t (:family "mononoki" :height 110))))
  ;; These set the cursor, active-, & other- isearch results
  ;; (respectively, and which all default to grey) to visible colors.
  (cursor         ((t (:background"#ddaa6f"))))
  (isearch        ((t (:background ,(face-background 'cursor)))))
  (lazy-highlight ((t (:background "#6b8dff"))))
  ;; Highlights
  (custom-highlight
    "My custom face for highlights and regions."
    ((((class color) (background dark))  :background "#00415e")
     (((class color) (background light)) :background "#c0efff")))
  (highlight ((t (:inherit 'custom-highlight :background unspecified :underline nil))))
  (region    ((t (:inherit 'custom-highlight :background unspecified :underline nil))))
  (show-paren-match ((t (:background "#6fa2dd"))))
  ;; Fringe
  (fringe           ((t (:foreground "#ddaa6f"))))
  (vertical-border  ((t (:background "#000"))))
  :config (load-theme 'wombat))

(use-package display-line-numbers
  :general (evil-leader-map
            "#" #'display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

(use-package prettify-symbols-mode
  :ghook ('lisp-mode-hook
          'lisp-data-mode-hook
          'eshell-mode-hook))

(use-package highlight-indent-guides
  :guix emacs-highlight-indent-guides
  :ghook 'prog-mode-hook
  :custom
  ;; Issue #107: Method 'character breaks with whitespace.el
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-auto-even-face-perc 20)
  (highlight-indent-guides-auto-odd-face-perc 15))

;; Nerd Icons
(use-package nerd-icons
  :config
  ;; For dirvish
  (defun all-the-icons-octicon (icon &rest args)
    (apply #'nerd-icons-octicon
      (cons (concat "nf-oct-"
              (string-replace "-" "_" icon))
            args)))
  (defalias #'all-the-icons-icon-for-dir #'nerd-icons-icon-for-dir)
  (defalias #'all-the-icons-icon-for-file #'nerd-icons-icon-for-file)
  (provide 'all-the-icons))

(use-package nerd-icons-completion
  :guix  emacs-nerd-icons-completion
  :after marginalia
  :ghook ('marginalia-mode-hook
          #'nerd-icons-completion-marginalia-setup)
  :config
  (let ((font-dest
          (cond ;; Default Linux install directories
                 ((member system-type '(gnu gnu/linux gnu/kfreebsd))
                  (concat (or (getenv "XDG_DATA_HOME")
                              (concat (getenv "HOME") "/.local/share"))
                          "/fonts/"
                          nerd-icons-fonts-subdirectory))
                 ;; Default MacOS install directory
                 ((eq system-type 'darwin)
                  (concat (getenv "HOME")
                          "/Library/Fonts/"
                          nerd-icons-fonts-subdirectory)))))
    (unless (file-readable-p (concat font-dest "NFM.ttf"))
      (nerd-icons-install-fonts t)))
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :guix emacs-nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters
               #'nerd-icons-corfu-formatter))


;; Butlers
(use-package no-littering
  :guix   emacs-no-littering
  :config (no-littering-theme-backups)
  :custom (auto-save-file-name-transforms
           `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package ws-butler
  :guix emacs-ws-butler
  :config (ws-butler-global-mode t))


;; Undo
(use-package undo-tree
  :guix emacs-undo-tree
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-histoy-directory-alist
    (expand-file-name "undo-tree" user-emacs-directory))
  :config (global-undo-tree-mode t))


;; Lisp Parens
(use-package rainbow-delimiters
  :guix emacs-rainbow-delimiters
  :ghook 'prog-mode-hook
  :config
  (require 'cl-lib)
  (require 'color)
  (-map (lambda (lst) (set-face-foreground (car lst) (cadr lst)))
        '((rainbow-delimiters-depth-1-face "dark orange")
          (rainbow-delimiters-depth-2-face "deep pink")
          (rainbow-delimiters-depth-3-face "chartreuse")
          (rainbow-delimiters-depth-4-face "deep sky blue")
          (rainbow-delimiters-depth-5-face "yellow")
          (rainbow-delimiters-depth-6-face "orchid")
          (rainbow-delimiters-depth-7-face "spring green")
          (rainbow-delimiters-depth-8-face "sienna1")))
  (cl-loop for index from 1 to rainbow-delimiters-max-face-count do
    (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
      (cl-callf color-desaturate-name (face-foreground face) 50)
      (cl-callf color-darken-name (face-foreground face) 30))))


;; Evil
(use-package evil
  :guix emacs-evil
  :demand
  :general-config
  ("C-M-u" #'universal-argument) ; evil-want-Y-yank-to-eol
  ;; Swap evil section keys with emacs defaults
  ;; C-g -> normal mode
  (:states 'normal
   "\\"    #'evil-leader-map
   "C-SPC" #'evil-leader-map)
  (:states 'insert
   "C-g"    #'evil-normal-state)
  (:states 'motion
   "] ]"   #'forward-page
   "[ ["   #'backward-page
   "C-x ]" #'evil-forward-section-begin
   "C-x [" #'evil-backward-section-end)
  ;; Prioritize native org-return over evil jump binding
  (:states 'normal
   :keymaps 'org-mode-map
   "RET"   #'org-return)
  ;; TODO: Why no general do this?
  :bind    (:repeat-map evil-windows/repeat-map
            (">" . evil-window-increase-width)
            ("<" . evil-window-decrease-width))

  :custom
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-tree)
  (evil-want-C-u-scroll t) ; universal-arg is rebound to C-M-u in :bind
  (evil-want-Y-yank-to-eol t)
  (evil-want-fine-undo nil)
  (evil-want-integration t)
  (evil-want-keybinding nil)

  :init
  (evil-mode t)

  :config
  ;; enable leader key, bound to \ and C-SPC in normal mode
  ;; XXX: C-SPC doesn't work in TUI frames :/
  (define-prefix-command 'evil-leader-map)

  (evil-set-initial-state 'eshell-mode 'normal)
  (evil-set-initial-state 'messages-buffer-mode 'normal))

(use-package evil-collection
  :guix   emacs-evil-collection
  :after  evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

; (use-package devil
;   :guix emacs-devil-mode
;   :custom (devil-key "'")
;   :config (global-devil-mode)
;   :general (:states 'motion
;             "'" #'devil))


;; Other Navigation
(use-package avy
  :guix   emacs-avy
  :after  (evil)
  :custom (avy-timeout-seconds 0.35)
  ;; Good combo of QUERTY and STRDY keys
  (avy-keys '(?s ?d ?l ?o ?u ?i ?e ?a ?f ?r))
  :general ("C-l" 'evil-avy-goto-line)
           (:states 'motion
            "g l" #'evil-avy-goto-line
            "g s" #'evil-avy-goto-char-timer)
           (isearch-mode-map
            "M-j" #'avy-isearch)
  :config
  ;; https://gist.github.com/karthink/af013ffd77fe09e67360f040b57b4c7b
  ;; Embark
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  ;; Kill text
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
  ;; Copy text
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)
  ;; Yank text
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
  ;; Transpose/Move text
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)
  ;; Transpose/Move sexp
  (defun avy-action-exchange (pt)
      (set-mark pt)
      (transpose-sexps 0))
  (add-to-list 'avy-dispatch-alist '(?e . avy-action-exchange))
  ;; Mark text
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char))


;;; Completion-at-point Stack
(use-package orderless
  :guix emacs-orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Vertico
(use-package vertico
  :guix emacs-vertico
  ;; https://kristofferbalintona.me/posts/202202211546/
  :after (savehist orderless)
  :init
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
  (vertico-mode t)
  :custom
  (enable-recursive-minibuffers t) ; i need this
  (minibuffer-depth-indicate-mode t)
  ;; Do not allow the cursor inside the text of the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :ghook ('minibuffer-setup-hook #'cursor-intangible-mode)
  :general-config
  (vertico-map
   "C-l"           #'kb/vertico-quick-embark
   "<backspace>"   #'vertico-directory-delete-char
   "C-<backspace>" #'vertico-directory-delete-word
   "RET"           #'vertico-directory-enter))

(use-package vertico-posframe
  :guix        emacs-vertico-posframe
  :after       vertico
  :custom      (vertico-posframe-parameters '((frame-border-width . 8)))
  :custom-face (vertico-posframe-border ((t :background "#555")))
  :init        (general-after-gui (vertico-posframe-mode 1)))

(use-package marginalia
  :guix    emacs-marginalia
  :after   vertico
  :general-config
  (minibuffer-local-map "M-A" #'marginalia-cycle)
  :init    (marginalia-mode t))

(use-package corfu
  :guix  emacs-corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-min-width 35)
  (corfu-preselect-first nil)
  (corfu-quit-no-match nil)
  (global-corfu-mode t)
  :general-config
  (corfu-map
    ;; XXX: Prevents tab-completion of a single candidate
    ; "TAB"     #'corfu-next
    ; [tab]     #'corfu-next
    ; "S-TAB"   #'corfu-previous
    ; [backtab] #'corfu-previous
    "S-SPC" #'corfu-insert-separator)
  :config
  ;; Move to Minibuffer
  ;; This is from the Corfu README, but pairs well with `embark-collect'
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
           completion-cycle-threshold completion-cycling)
      (apply 'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" 'corfu-move-to-minibuffer)

  ;; Enable around Vertico
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (bound-and-true-p vertico--input)
      (setq-local corfu-auto nil) ; only in buffer
      (corfu-mode 1)))
  ;; Hook depth! Very fancy.
  (add-hook 'minibuffer-setup-hook 'corfu-enable-always-in-minibuffer 1)

  ;; Enable in Eshell
  (add-hook 'eshell-mode-hook
    (lambda ()
      (setq-local corfu-auto nil) ; only in buffer
      (corfu-mode 1)))
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
      ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
        (eshell-send-input))
      ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
        (comint-send-input))))
  (advice-add 'corfu-insert :after 'corfu-send-shell)
  ;; Strongly recommended in the README
  (-map (-cut advice-add 'pcomplete-completions-at-point :around <>)
    (list 'cape-wrap-silent 'cape-wrap-purify)))

(use-package corfu-terminal
  :guix emacs-corfu-terminal
  :defer
  :init (general-after-tty (corfu-terminal-mode 1)))

(use-package cape
  :guix emacs-cape
  ;; Defaults from the cape README.
  :ghook ('completion-at-point-functions #'cape-file)
  :general ("C-c p" #'cape-prefix-map))

(use-package embark
  :guix (emacs-embark ; This refers to my fork with a page-able which-key pop-up on `embark-collect'
          ; --with-git-url=emacs-embark=file:///home/maddhappy/projects/oantolin/embark
          ; --with-branch=emacs-embark=fix/issue-647
          )
  :after evil evil-repeat
  :general ("C->" #'embark-act  ; pick some comfortable binding
            "C-;" #'embark-dwim ; good alternative: M-.
            [remap describe-bindings] #'embark-bindings) ; C-h b
           (:state 'normal
            ;; XXX: "C-." doesn't work in Zorin's Gnome Terminal.
            ;; XXX: Also not seeing it in GUI mode. Ugh.
            "C-." #'embark-act)
  ;; Replace the key help with a completing-read interface
  :custom (prefix-help-command 'embark-prefix-help-command)

  :config
  (defun embark--add-which-key-map (keymap)
    (make-composed-keymap (which-key--get-popup-map) keymap))
  (advice-add #'embark--action-keymap
              :filter-return
              #'embark--add-which-key-map)
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
    '(embark-which-key-indicator
      embark-highlight-indicator
      embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))

  (advice-add #'embark-completing-read-prompter :around
    #'embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))

(use-package consult
  :guix (emacs-consult
         ripgrep)
  :general ([remap yank-pop] #'consult-yank-pop ; M-y
            "M-p"     #'consult-yank-pop        ; makes more sense to me
            ;; C-x bindings (ctl-x-map)
            [remap switch-to-buffer] #'consult-buffer ; C-x b
            [remap project-switch-to-buffer] #'consult-project-buffer ; C-x p b
            ;; I prefer to narrow consult-buffer, but like that this
            ;; makes missing bookmarks. Could the narrowed buffer
            ;; search do that?
            "C-x r b" #'consult-bookmark
            ;; M-g bindings (goto-map)
            [remap goto-line] #'consult-goto-line ; M-g g
            "M-g o"   #'consult-outline
            "M-g i"   #'consult-imenu
            ;; M-s bindings (search-map)
            "M-s d"   #'consult-find
            "M-s g"   #'consult-grep
            "M-s G"   #'consult-git-grep
            "M-s r"   #'consult-ripgrep
            "C-s"     #'consult-line
            "M-s l"   #'consult-line
            "M-s L"   #'consult-line-multi)
  :config
  ;; Isearch integration
  (with-eval-after-load 'isearch
    (general-def
      'isearch-mode-map
      "M-m"     #'consult-isearch-history ; like move-to-minibuffer
      "M-e"     #'consult-isearch-history ; orig. isearch-edit-string
      "M-s e"   #'consult-isearch-history ; orig. isearch-edit-string
      "M-s l"   #'consult-line            ; needed by consult-line to detect isearch (???)
      "M-s L"   #'consult-line-multi))    ; needed by consult-line to detect isearch (???)
  :custom (consult-narrow-key "<"))

(use-package embark-consult
  :ghook ('embark-collect-mode-hook #'consult-preview-at-point-mode))


;;; Application Packages
(use-package dired
  :gfhook ('dired-mode-hook #'dired-hide-details-mode))

(use-package dired-avfs
  :guix (emacs-dired-hacks avfs)
  :config
  (defun antlers/find-file-noselect (filename &rest _)
    (unless (and (file-readable-p (concat (getenv "HOME") "/.avfs"))
                 (= 0 (call-process "mountpoint" nil nil nil (concat (getenv "HOME") "/.avfs"))))
    (start-process-shell-command "mountavfs" nil "mountavfs"))))

;; Dired Font Lock -- for colors!
(use-package diredfl
  :guix emacs-diredfl
  :custom-face
  (diredfl-date-time   ((t :foreground "grey")))
  (diredfl-exec-priv   ((t :foreground ,(face-foreground 'escape-glyph nil t) :background unspecified)))
  (diredfl-file-name   ((t :foreground ,(face-foreground 'default nil t))))
  (diredfl-file-suffix ((t :foreground "grey" :background unspecified)))
  (diredfl-no-priv     ((t :foreground "grey" :background unspecified)))

  (diredfl-symlink     ((t :foreground "orchid")))
  (diredfl-link-priv   ((t :foreground "orchid")))
  (diredfl-dir-heading ((t :inherit dired-header           :foreground unspecified :background unspecified)))
  (diredfl-dir-name    ((t :inherit dired-directory        :foreground unspecified :background unspecified)))
  (diredfl-dir-priv    ((t :inherit dired-directory        :foreground unspecified :background unspecified)))
  (diredfl-number      ((t :inherit dired-directory        :foreground unspecified :background unspecified)))
  (diredfl-read-priv   ((t :inherit font-lock-keyword-face :foreground unspecified :background unspecified)))
  (diredfl-write-priv  ((t :inherit font-lock-builtin-face :foreground unspecified :background unspecified)))
  (diredfl-executable-tag ((t :inherit dired-directory     :foreground unspecified :background unspecified)))
  :ghook '(dired-mode-hook
           dirvish-directory-view-mode-hook))

(use-package vc-git
  :config
  ;; Improve handling of .git and untracked directories
  (defun antlers/vc-git-state (file)
    (when (equal (eshell/basename file) ".git")
      'ignored))
  (advice-add 'vc-git-state :before-until
    #'antlers/vc-git-state)
  (defun antlers/vc-git--git-status-to-vc-state (code-list)
    (when (and code-list
               (listp code-list)
               (-every? (-cut equal <> "??") code-list))
      'unregistered))
  (advice-add 'vc-git--git-status-to-vc-state :before-until
    #'antlers/vc-git--git-status-to-vc-state))

(use-package dirvish
  :guix (emacs-dirvish
         emacs-pdf-tools
         imagemagick
         fd
         mediainfo
         tar
         unzip)
  :after (nerd-icons
          moody
          eshell-prompt-extras)
  :general (evil-leader-map
            "d"   #'dirvish-dwim)
           (:states 'motion
            "-"   #'dired-jump)
           (dirvish-mode-map
            :states 'normal
            "q"   #'dirvish-quit) ; quit-window causes problems :/
           (dirvish-mode-map
            :states 'motion
            "a"   #'dirvish-quick-access
            "N"   #'dirvish-narrow
            "TAB" #'dirvish-toggle-subtree
            "_"   #'dirvish-layout-toggle)
  :custom-face
  (dirvish-collapse-dir-face ((t (:inherit dirvish-collapse-empty-dir-face))))
  (dirvish-git-commit-message-face ((t (:background "#333"))))
  :custom
  (dirvish-override-dired-mode t)
  (dirvish-attributes
    '(subtree-state
      collapse
      all-the-icons
      git-msg
      file-size
      file-time
      vc-state
      hl-line
      ))
  (dirvish-quick-access-entries
   '(("d" "~/"         "Home")
     ("p" "~/projects" "Projects")
     ("s" "~/Sync"     "Sync")
     ))
  (dired-listing-switches
    "-lv --escape --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-header-line-height moody-mode-line-height)
  (dirvish-mode-line-height moody-mode-line-height)
  :init
  (defun antlers/disable-indicate-buffer-boundaries ()
    (setq-local indicate-buffer-boundaries nil))
  :ghook ('dired-mode-hook #'antlers/disable-indicate-buffer-boundaries)
  :config
  (require 's)
  ;; Customize header-line and mode-line
  (defvar header-line-format-right-align
    '((:eval (progn (setq mode-line-format-bak mode-line-format) nil))
      (:eval (progn (setq mode-line-format header-line-format) nil))
      mode-line-format-right-align
      (:eval (progn (setq mode-line-format mode-line-format-bak) nil))))
  (defun antlers/s-subtract (n str)
    (number-to-string (- (string-to-number str) n)))
  (defun antlers/dirvish--mode-line-fmt-setter (left right &optional header)
    (cl-labels ((expand (segments)
                  (cl-loop for s in segments collect
                           (if (stringp s) s
                             `(:eval (,(intern (format "dirvish-%s-ml" s)) (dirvish-curr))))))
                (antlers/expand (segments)
                  `(:eval
                    (let* ((dv (dirvish-curr))
                           (buf (and (car (dv-layout dv)) (cdr (dv-index dv)))))
                      ,segments))))
      (if header
          `(,(antlers/expand
               `(list (if (or (not buf) (eq buf (current-buffer)))
                          "   "
                          "  ")
                      (format-mode-line ',(expand '(file-modes)) nil nil buf)))
            ,@header-line-format-right-align
            ,(antlers/expand
               `(list (format-mode-line ',(expand '(free-space)) nil nil buf)
                      ;; Doesn't show up without this, probably height-related?
                      (if (and (or (not buf) (eq buf (current-buffer)))
                               (buffer-local-value 'dired-hide-details-mode
                                                   (or buf (current-buffer))))
                          "      "
                          " ")
                      (dirvish--bar-image (car (dv-layout dv)) t))))
        (antlers/mode-line-format
          '(epe-fish-path (epe-pwd))
          `(,(antlers/expand
               `(s-truncate
                  (- (window-total-width) 42) ; XXX: Lazy
                  (-> ',(or (expand left) mode-line-format)
                      (format-mode-line nil nil buf)
                      (string-trim)))))
          `(,(antlers/expand
               '(list (if (and buf (not (eq buf (current-buffer))))
                          "-/"
                        (let ((info (format-mode-line (spaceline--selection-info) nil nil buf)))
                          (if (s-contains? "/" info)
                              (list (car (string-split info "/")) ":/")
                            (list (antlers/s-subtract 1 (car (string-split info ":"))) "/"))))
                      (antlers/s-subtract 1
                        (cadr (string-split (antlers/mode-line-percent buf) "[/)]"))))))
          '(" ")))))
  (advice-add 'dirvish--mode-line-fmt-setter :override
    #'antlers/dirvish--mode-line-fmt-setter))

(use-package dirvish-collapse
  :config
  ;; Replace `\` with `|` in collapsed paths.
  (defun antlers/dirvish-collapse--cache (x)
    (or (and (not (stringp (car x))) x)
        (cons (apply #'propertize
                (subst-char-in-string ?| ?/ (car x) t)
                (text-properties-at 0 (car x)))
              (cdr x))))
  (advice-add 'dirvish-collapse--cache :filter-return
    #'antlers/dirvish-collapse--cache))

(use-package dirvish-widgets
  :config
  (defun antlers/dirvish-file-modes-ml (str)
    "Replicates diredfl colors."
    (prog1 str
      (-map-indexed (pcase-lambda (i `(,char . ,face))
                      (put-text-property i (1+ i)
                        'face (if (= char ?-) diredfl-no-priv face)
                        str))
        (-zip-lists (string-to-list str)
                    (list diredfl-link-priv
                          diredfl-read-priv
                          diredfl-write-priv
                          diredfl-exec-priv
                          diredfl-read-priv
                          diredfl-write-priv
                          diredfl-exec-priv
                          diredfl-read-priv
                          diredfl-write-priv
                          diredfl-exec-priv)))))
  (advice-add 'dirvish-file-modes-ml :filter-return
    #'antlers/dirvish-file-modes-ml)

  (defun antlers/dirvish-free-space-ml (str)
    (propertize str 'face 'dired-ignored))
  (advice-add 'dirvish-free-space-ml :filter-return
    #'antlers/dirvish-free-space-ml))

(use-package dirvish-vc
  :config
  (require 'git-gutter)
  (dirvish-define-attribute git-msg
    ;; Customized to use face with custom bg, had to trim edges /
    ;; account for hl-line.
    "Append git commit message to filename."
    :index 1
    :when (and (eq (dirvish-prop :vc-backend) 'Git)
               (not (dirvish-prop :remote))
               (> win-width 65))
    (let* ((info (dirvish-attribute-cache f-name :git-msg))
           (face (or hl-face 'dirvish-git-commit-message-face))
           (str (concat (substring (concat " " info) 0 -1) " ")))
      (when hl-face
        (add-face-text-property 0 1 face t str)
        (add-face-text-property (- (length str) 1) (length str) face t str))
      (when (> (length (string-to-list str)) 1)
        (add-face-text-property 1 (- (length str) 1) face t str)
        `(left . ,str))))

  ;; Use git-gutter for vc-state
  (defun antlers/magit-post-refresh-hook (&optional _)
    (-map (lambda (b)
            (when (eq (buffer-local-value 'major-mode b) #'dired-mode)
              (save-window-excursion
                (switch-to-buffer b)
                (revert-buffer t t nil))))
          (buffer-list)))
  (with-eval-after-load 'magit-mode
    (add-hook 'magit-post-refresh-hook
      #'antlers/magit-post-refresh-hook))

  (defvar antlers/vc-state-cache (make-hash-table :test #'equal))
  (defun antlers/clear-vc-state-cache ()
    (clrhash antlers/vc-state-cache)
    (git-gutter:clear-gutter))
  (add-hook 'dirvish-after-revert-hook
    #'antlers/clear-vc-state-cache)

  (defun antlers/dirvish-subtree-remove (ret)
    (git-gutter:clear-gutter)
    ret)
  (advice-add 'dirvish-subtree-remove :filter-return
    #'antlers/dirvish-subtree-remove)
  (advice-add 'dirvish--init-session :filter-return
    #'antlers/magit-post-refresh-hook)

  (defun antlers/dirvish--render-attrs (ret)
    (setq git-gutter:last-chars-modified-tick
      (buffer-chars-modified-tick))
    ret)
  (advice-add 'dirvish--render-attrs :filter-return
    #'antlers/dirvish--render-attrs)

  (defun antlers/dirvish-find-entry-hook (_key _buffer)
    (git-gutter:clear-gutter))
  (add-hook 'dirvish-find-entry-hook
    #'antlers/dirvish-find-entry-hook)

  (dirvish-define-attribute vc-state
    :when (when (not (equal (buffer-chars-modified-tick)
                            git-gutter:last-chars-modified-tick))
            (git-gutter:set-window-margin (git-gutter:window-margin))
            t)
    (when (and (not (equal (buffer-chars-modified-tick)
                            git-gutter:last-chars-modified-tick))
               (vc-find-root (file-name-parent-directory f-name) ".git"))
      ;; Cache full of nonsense: (dirvish-attribute-cache f-name :vc-state)
      (let* ((state (or (gethash f-name antlers/vc-state-cache)
                        (puthash f-name (vc-state-refresh f-name 'Git) antlers/vc-state-cache))))
        (pcase state
          ('conflict     (git-gutter:put-signs (propertize "◆" 'face '(:foreground "#e5786d")) (list f-beg)))
          ('ignored      (git-gutter:put-signs (propertize "✕" 'face '(:foreground "#646")) (list f-beg)))
          ('added        (git-gutter:put-signs (propertize git-gutter:added-sign 'face '(:foreground "#cae682")) (list f-beg)))
          ('removed      (git-gutter:put-signs (propertize git-gutter:removed-sign 'face '(:foreground "#e5786d")) (list f-beg)))
          ('edited       (git-gutter:put-signs (propertize git-gutter:modified-sign 'face 'git-gutter:modified) (list f-beg)))
          ('unregistered (git-gutter:put-signs (propertize "✕" 'face '(:foreground "orchid")) (list f-beg)))
          ('up-to-date   nil)
          (_ nil)
          )
        nil))))

(use-package tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:antlers@192\\.168\\.0\\.[0-9]+:")
                     "direct-async-process" t))
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil)) ; presumes SSH is already configured

(use-package magit
  :guix    (emacs-magit
            emacs-transient-posframe
            diffutils)
  :general (evil-leader-map "g" #'magit)
           (magit-status-mode-map "M-RET" #'magit-diff-visit-file-other-window)
  :custom  (magit-diff-refine-hunk t)
  :init    (general-after-gui (transient-posframe-mode 1)))

(defvar ledger-dir
  (concat (getenv "HOME") "/Sync/ledger"))
(defvar ledger-init-file-name
  (concat ledger-dir "/ledgerrc"))
(use-package ledger-mode
  :guix emacs-ledger-mode
  :custom (ledger-clear-whole-transactions t)
          (ledger-post-account-alignment-column 2)
          (ledger-post-amount-alignment-column 49)
          (ledger-post-amount-alignment-at :decimal)
  :general (evil-leader-map
            "l" `("ledger" . (antlers/find-file ledger-dir))))


;; Org
(use-package org
  :guix emacs-org
  :custom
  (org-startup-indented      t)
  (org-default-priority      ?C)
  (org-lowest-priority       ?D)
  (org-agenda-files          '("~/Sync/org"))
  (diary-file                "~/Sync/org/diary")
  (org-archive-location      "%s_archive::* Archived Tasks")
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-todo-keywords         '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                               (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces    '(("TODO" . org-todo)
                               ("NEXT" :foreground "blue" :weight bold)
                               ("DONE" . org-done)
                               ("WAITING" :foreground "orange" :weight bold)
                               ("HOLD" . org-warning)
                               ("CANCELLED" :foreground "forest green" :weight bold)))
  (org-agenda-compact-blocks t)
  (org-return-follows-link   t)
  ;; Refile
  (org-default-notes-file    "~/Sync/org/refile.org")
  (org-refile-targets        '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))
  (org-refile-use-outline-path t)
  (org-refile-allow-creating-parent-nodes '(confirm))
  ;; Misc
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-priority-faces
    '((?A :foreground "red")
      (?B :foreground "orange")
      (?C :foreground "yellow")
      (?D :foreground "grey")))
  :custom-face
  ;; This was comment-grey by default :/
  (outline-4 ((t :foreground "#c0a6f5")))
  :general (evil-leader-map
            "a" #'org-agenda
            "t" #'org-todo-list
            "c" #'org-capture)
           (org-mode-map
            "C-c l" #'org-store-link
            "C-c b" #'org-switchb
            "C-c [" nil
            "C-c ]" nil)
  :gfhook #'visual-line-mode
          #'flyspell-mode
          #'org-setup-<>-syntax-fix
          #'(lambda () (setq-local tab-width 8))
  :config
  ;; Credit: https://emacs.stackexchange.com/a/52209
  (defun org-mode-<>-syntax-fix (start end)
    "Change syntax of characters ?< and ?> to symbol within source code blocks."
    (let ((case-fold-search t))
      (when (eq major-mode 'org-mode)
        (save-excursion
          (goto-char start)
          (while (re-search-forward "<\\|>" end t)
            (when (save-excursion
                    (and
                     (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
                     (string-equal (downcase (match-string 1)) "begin")))
              ;; This is a < or > in an org-src block
              (put-text-property (point) (1- (point))
                                 'syntax-table (string-to-syntax "_"))))))))

  (defun org-setup-<>-syntax-fix ()
    "Setup for characters ?< and ?> in source code blocks.
     Add this function to `org-mode-hook'."
    (make-local-variable 'syntax-propertize-function)
    (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
    (syntax-propertize (point-max))))

(use-package org-agenda
  :after org
  :general-config
  (org-agenda-mode-map
   "j" #'evil-next-line
   "k" #'evil-previous-line))

(use-package org-super-agenda
  :guix emacs-org-super-agenda
  :after org)

(use-package ob
  :after org
  :custom (org-src-preserve-indentation t)
  :config (org-babel-do-load-languages
            'org-babel-load-languages
            '((shell . t)
              (gnuplot . t))))

(use-package svg-tag-mode
  :guix emacs-svg-tag-mode
  :ghook ('org-mode-hook #'svg-tag-mode)
  :after org-faces
  :custom-face
  (org-todo-tag      ((t :background ,(face-foreground 'org-todo nil t)
                         :foreground ,(face-background 'default)
                         :weight bold)))
  (org-next-tag      ((t :background "orange"
                         :foreground ,(face-background 'default)
                         :weight bold)))
  (org-hold-tag      ((t :foreground ,(face-foreground 'org-warning nil t)
                         :weight bold)))
  (org-done-tag      ((t :foreground ,(face-foreground 'org-done nil t)
                         :weight bold)))
  (org-waiting-tag   ((t :foreground "orange"
                         :weight bold)))
  (org-cancelled-tag ((t :foreground ,(face-foreground 'org-verbatim nil t)
                         :weight bold)))
  (org-tag-tag       ((t :foreground ,(face-foreground 'org-verbatim nil t)
                         :weight bold)))
  (org-priority-tag  ((t :background ,(face-foreground 'org-priority nil t)
                         :foreground ,(face-background 'default)
                         :weight bold)))
  :custom
  (svg-tag-action-at-point 'edit)
  (svg-tag-tags
    '(("TODO"      . ((lambda (tag) (svg-tag-make tag :face 'org-todo-tag))))
      ("NEXT"      . ((lambda (tag) (svg-tag-make tag :face 'org-next-tag))))
      ("HOLD"      . ((lambda (tag) (svg-tag-make tag :face 'org-hold-tag))))
      ("DONE"      . ((lambda (tag) (svg-tag-make tag :face 'org-done-tag))))
      ("CANCELLED" . ((lambda (tag) (svg-tag-make tag :face 'org-cancelled-tag))))
      ("WAITING"   . ((lambda (tag) (svg-tag-make tag :face 'org-waiting-tag))))
      ("\\[#A\\]"  . ((lambda (tag) (svg-lib-progress-pie 1.00 nil :foreground "red" :stroke 3))))
      ("\\[#B\\]"  . ((lambda (tag) (svg-lib-progress-pie 0.75 nil :foreground "orange" :stroke 3))))
      ("\\[#C\\]"  . ((lambda (tag) (svg-lib-progress-pie 0.50 nil :foreground "yellow" :stroke 3))))
      ("\\[#D\\]"  . ((lambda (tag) (svg-lib-progress-pie 0.25 nil :foreground "grey" :stroke 3))))
      ;; XXX: I want to style props and src blocks, but
      ;; action-at-point edit gets really buggy when using :beg, :end,
      ;; or (possibly overlapping) tags with a capture group.
      )))

(use-package ox
  :guix emacs-htmlize
  :defer)

(use-package ox-haunt
  :guix emacs-ox-haunt
  :after ox
  :defer)


;; Roam
(use-package org-roam
  :guix emacs-org-roam
  :custom
  (org-roam-directory "~/Sync/org")
  (org-roam-completion-everywhere nil)
  :config
  (org-roam-db-autosync-mode))

(use-package websocket
  :guix  emacs-websocket
  :after org-roam)

(use-package org-roam-ui
  :guix    emacs-org-roam-ui
  :after   org-roam
  :general (evil-leader-map "u" #'org-roam-ui-open)
  :custom  (org-roam-ui-sync-theme t)
           (org-roam-ui-open-on-start nil))

(use-package org-node
  :guix    emacs-org-node
  :after   org org-roam
  :general ("M-s f" 'org-node-find)
           ("M-s i" 'org-node-insert-link)
           ("M-s s" #'org-node-series-dispatch)
  :custom
  (org-node-ask-directory "~/Sync/org/roam")
  (org-node-filter-fn
    (lambda (node)
      (not (or (org-node-get-todo node) ;; Ignore headings with todo state
               (member "drill" (org-node-get-tags node)) ;; Ignore :drill:
               (assoc "ROAM_EXCLUDE" (org-node-get-properties node))
               (string-search "archive" (org-node-get-file-path node))))))
  ;; Seek wide use
  :ghook ('org-open-at-point-functions
          #'org-node-try-visit-ref-node)
  :config
  (org-node-cache-mode)
  (org-node-complete-at-point-mode)

  ;; Undo a Roam override
  (with-eval-after-load 'org-roam-id
    (org-link-set-parameters
     "id" :follow #'org-id-open :store #'org-id-store-link-maybe))

  ;; "Old Default Setting", from:
  ;; https://github.com/meedstrom/org-node/wiki/Configuring-series#old-default-setting
  ;; (setq org-node-series-defs
  ;;   (list '("d" :name "Dailies"
  ;;      :version 2
  ;;      :classifier (lambda (node)
  ;;                    (let ((path (org-node-get-file-path node)))
  ;;                      (when (string-search (concat (getenv "HOME") "/Sync/org/roam/journals") path)
  ;;                        (let ((ymd (org-node-helper-filename->ymd path)))
  ;;                          (when ymd
  ;;                            (cons ymd path))))))
  ;;      :whereami (lambda ()
  ;;                  (org-node-helper-filename->ymd buffer-file-name))
  ;;      :prompter (lambda (key)
  ;;                  (let ((org-node-series-that-marks-calendar key))
  ;;                    (org-read-date)))
  ;;      :try-goto (lambda (item)
  ;;                  (org-node-helper-try-visit-file (cdr item)))
  ;;      :creator (lambda (sortstr key)
  ;;                 (let ((org-node-datestamp-format "")
  ;;                       (org-node-ask-directory "~/Sync/org/roam/journals"))
  ;;                   (org-node-create sortstr (org-id-new) key))))))
  )

(use-package org-node-fakeroam
  :guix emacs-org-node-fakeroam
  :custom
  (org-node-extra-id-dirs '("~/Sync/org"))
  ;; Right from the README.
  (org-node-creation-fn #'org-node-fakeroam-new-via-roam-capture)
  (org-node-slug-fn #'org-node-fakeroam-slugify-via-roam)
  (org-node-datestamp-format "%Y%m%d%H%M%S-")
  ;; DB:
  (org-roam-db-update-on-save nil) ; don't update DB on save, not needed
  (org-roam-link-auto-replace nil) ; don't look for "roam:" links on save
  :config
  (org-node-fakeroam-fast-render-mode)  ; build the Roam buffer faster
  (org-node-fakeroam-setup-persistence) ; cache previews on-disk
  ;; DB:
  (unless org-roam-db-update-on-save
    (org-node-fakeroam-redisplay-mode))  ; auto-refresh the Roam buffer
  (org-node-fakeroam-db-feed-mode)       ; keep Roam DB up to date
  (org-node-fakeroam-jit-backlinks-mode) ; skip DB for Roam buffer
  ;; Seek wide use
  (advice-add #'org-roam-db-sync :override
              #'org-node-fakeroam-db-rebuild)
  (advice-add #'org-roam-list-files :override
              #'org-node-fakeroam-list-files)
  (advice-add #'org-roam-dailies--list-files :override
              #'org-node-fakeroam-list-dailies)
  (advice-add #'org-roam-dailies--daily-note-p :override
              #'org-node-fakeroam-daily-note-p))


;; Eshell
(use-package eat
  :guix emacs-eat
  :custom
  (eat-term-name "xterm-256color")
  (eat-eshell-mode t)
  (eat-eshell-visual-command-mode t))

(use-package eshell
  :after evil
  :gfhook ('emacs-startup-hook #'eshell)
  :general
  (evil-leader-map
   "q" #'eshell)
  :general-config
  (eshell-mode-map
   "C-l" #'antlers/clear)
  :init
  (defun antlers/clear ()
    (interactive)
    (evil-goto-line)
    (evil-scroll-line-to-top
      (string-to-number
        (format-mode-line "%l"))))
  :custom
  (eshell-scroll-to-bottom-on-output nil)
  (eshell-scroll-show-maximum-output nil)
  :config
  ;; Clever rebinding of nvim/emacs -> :edit
  ;; TODO: More of these, built into guix home config?
  (defun eshell/my-find-file (pattern)
    (if (stringp pattern)
        (find-file pattern)
      (mapc 'find-file (mapcar 'expand-file-name pattern))))
  (defun eshell/nvim (&rest args)
    (apply 'eshell/my-find-file args))
  (defun eshell/emacs (&rest args)
    (apply 'eshell/my-find-file args))

  ;; Ooo, now here's something bold:
  (defun quit-to-eshell ()
    (interactive)
    (if (> (count-windows) 1)
        (let ((buffer (current-buffer)))
          (delete-window)
          (when (not (get-buffer-window buffer))
            (kill-buffer buffer)))
      (kill-buffer)
      (eshell)))
  (defun quit-all-to-eshell ()
    (interactive)
    (let ((target (window-frame (get-buffer-window (current-buffer)))))
      (-map (lambda (b)
              (when (eq (window-frame (get-buffer-window b)) target)
                (with-current-buffer b (quit-to-eshell))))
            (buffer-list))))
  (defun save-and-quit-to-eshell (&optional arg)
    (interactive)
    ;; This filters over all buffers, but had the `pred' behavior that I want...
    (let ((target (current-buffer)))
      (save-some-buffers arg (lambda () (eq (current-buffer) target))))
    (quit-to-eshell))
  (defun save-and-quit-to-eshell* (&optional arg)
    (interactive)
    (save-and-quit-to-eshell t))
  (defun save-and-quit-all-to-eshell (&optional arg)
    (interactive)
    (let ((target (window-frame (get-buffer-window (current-buffer)))))
      (save-some-buffers arg
        (lambda () (get-buffer-window (current-buffer) target))))
    (quit-all-to-eshell))
  (defun save-and-quit-all-to-eshell* (&optional arg)
    (interactive)
    (save-and-quit-all-to-eshell t))
  (defun save-and-kill-frame (&optional arg)
    (condition-case nil
        (delete-frame)
        (error (save-buffers-kill-terminal))))
  (evil-ex-define-cmd "exit"    #'save-and-kill-frame)
  (evil-ex-define-cmd "q[uit]"  #'quit-to-eshell)
  (evil-ex-define-cmd "qa[ll]"  #'quit-all-to-eshell)
  (evil-ex-define-cmd "wq"      #'save-and-quit-to-eshell)
  (evil-ex-define-cmd "wqa[ll]" #'save-and-quit-to-eshell*)
  (evil-ex-define-cmd "qa[ll]"  #'save-and-quit-all-to-eshell)
  (evil-ex-define-cmd "wqa[ll]" #'save-and-quit-all-to-eshell*)
  (defalias 'save-buffers-kill-terminal #'save-and-quit-all-to-eshell))

(use-package eshell-syntax-highlighting
  :guix   emacs-eshell-syntax-highlighting
  :after  eshell
  :config (eshell-syntax-highlighting-global-mode t))

(use-package eshell-prompt-extras
  :guix   emacs-eshell-prompt-extras
  :custom (eshell-highlight-prompt nil)
          (eshell-prompt-function 'epe-theme-lambda))


;; Minor Modes

; (use-package beacon
;   :load-path "/home/maddhappy/projects/beacon"
;   :custom
;   (beacon-size 35)
;   (beacon-blink-when-point-moves-vertically 15)
;   (beacon-do-blink-commands '(evil-scroll-up evil-scroll-down
;                               evil-goto-line evil-goto-last-line))
;   :gfhook (#'beacon-dont-blink-major-modes
;            (list #'eshell-mode #'shell-mode))
;   :config (general-after-gui
;             (beacon-mode 1))
;   :config
;   (defun beacon-do-blink-command (func)
;     (advice-add func
;                 :after
;                 (lambda (func &rest args)
;                   (let ((beacon-dont-blink-commands '()))
;                     (beacon--post-command)))))
;   (mapc #'beacon-do-blink-command beacon-do-blink-commands))

(use-package flyspell
  :guix hunspell hunspell-dict-en-us
  :ghook ('(prog-mode-hook) #'flyspell-prog-mode)
         'git-commit-mode-hook
  :custom
  (ispell-alternate-dictionary "/tmp/words")
  (ispell-personal-dictionary ; Keep `ispell' dictionary in .emacs.d
    (concat (getenv "HOME") "/Sync/org/ispell-english.dict"))
  (ispell-silently-savep t)   ; Don't ask before saving dict. updates
  :config
  ;; TODO: Should be a Guix package instead of building into /tmp at
  ;; runtime.
  (unless (file-readable-p "/tmp/words")
    (start-process-shell-command "unmunch" nil
      (string-join
        (list (concat (getenv "GUIX_ENVIRONMENT")
                      "/bin/unmunch")
              (shell-quote-argument
                (concat (getenv "GUIX_ENVIRONMENT")
                        "/share/hunspell/en_US.dic"))
              (shell-quote-argument
                (concat (getenv "GUIX_ENVIRONMENT")
                        "/share/hunspell/en_US.aff"))
              "> /tmp/words")
        " "))))

(use-package which-key
  :guix emacs-which-key
  ;; "same as default, except all keys from local maps shown first"
  :custom (which-key-compute-remaps t)
          (which-key-sort-order 'which-key-local-then-key-order)
          (which-key-use-C-h-commands t)
          (which-key-idle-delay 0.5)
  :config (which-key-mode t))

(use-package whitespace
  :ghook 'prog-mode-hook
  :custom (whitespace-space 'whitespace-newline)
          (whitespace-style '(face lines-tail missing-newline-at-eof tab-mark))
  :custom-face (whitespace-line ((t :background unspecified :foreground "#555"))))

(use-package git-gutter
  :guix emacs-git-gutter
  :commands (git-gutter:set-window-margin git-gutter:window-margin)
  :ghook 'prog-mode-hook 'org-mode-hook
  :custom-face
  (git-gutter:added ((t :foreground "#cae682")))
  (git-gutter:deleted ((t :foreground "#e5786d")))
  (git-gutter:modified ((t :foreground "#8ac6f2")))
  :custom (git-gutter:modified-sign "≈")
  :config
  (with-eval-after-load 'magit-mode
    (add-hook 'magit-post-refresh-hook
      #'git-gutter:update-all-windows)))

(use-package rainbow-mode
  :guix emacs-rainbow-mode
  :ghook 'prog-mode-hook)


;; On-demand Minor Modes
'(:guix (emacs-csv-mode ; used to enable `csv-align-mode', but long line issues
         ;; emacs-go-mode ; gonna try tree-sitter first
         ;; emacs-makefile-ts ; not installed yet
         tree-sitter-go
         emacs-debbugs
         emacs-json-mode
         emacs-markdown-mode
         emacs-yaml-mode))

(use-package nov
  :guix emacs-nov-el
  :after darkroom
  :gfhook #'darkroom-tentative-mode
  :custom (nov-text-width 80)
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package darkroom
  :guix    emacs-darkroom
  :general (evil-leader-map "f" #'darkroom-tentative-mode)
  :custom  (darkroom-text-scale-increase 1.25))


;; EAF
(use-package eaf
  :guix (alsa-lib
         at-spi2-core
         bzip2
         cairo
         cups
         dbus
         gcc-toolchain
         gdk-pixbuf
         git
         glib
         gst-plugins-base
         gstreamer
         gtk+
         jq
         libxcomposite
         libxkbcommon
         libxkbfile
         libxrandr
         libxrender
         libxtst
         mesa
         mit-krb5
         mysql
         node
         nss
         pango
         patchelf
         pcsc-lite
         postgresql
         pulseaudio
         python
         speech-dispatcher
         unixodbc
         wmctrl
         xcb-util-cursor
         xcb-util-image
         xcb-util-keysyms
         xcb-util-wm
         xdotool)
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :after evil
  :init
  (defun antlers/eaf-install-and-update ()
    (eaf-install-and-update 'browser 'org-previewer 'pdf-viewer))
  (let ((eaf-url "https://github.com/emacs-eaf/emacs-application-framework")
        (eaf-dir (concat (getenv "HOME") "/.emacs.d/site-lisp/emacs-application-framework")))
    (if (not (file-readable-p eaf-dir))
        (progn
          (mkdir (file-name-directory eaf-dir) t)
          (vc-git-clone eaf-url eaf-dir nil)
          (antlers/eaf-install-and-update))
      (-> eaf-dir
          (concat "/.git/FETCH_HEAD")
          (file-attributes)
          (file-attribute-modification-time)
          (time-since)
          (time-less-p (days-to-time 7))
          (unless (add-hook 'after-init-hook #'antlers/eaf-install-and-update)))))
  :config
  (antlers/append-to-path
    (concat (getenv "GUIX_ENVIRONMENT") "/lib")
    "LD_LIBRARY_PATH")
  (antlers/append-to-path
    (concat (getenv "GUIX_ENVIRONMENT") "/lib/nss")
    "LD_LIBRARY_PATH")
  (let* ((sh-bin (shell-command-to-string "which sh"))
         (sh-bin (shell-command-to-string (concat "realpath " sh-bin)))
         (interpreter (shell-command-to-string (concat "patchelf --print-interpreter " sh-bin)))
         (interpreter (string-trim-right interpreter)))
      (-map (lambda (file)
              ;; XXX: No error handling
              (start-process-shell-command "patchelf" nil
                (concat "patchelf --set-interpreter " interpreter " " (concat (getenv "HOME") file))))
        '("/.local/lib/python3.10/site-packages/PyQt6/Qt6/libexec/QtWebEngineProcess"
          "/.local/lib/python3.10/site-packages/PyQt6/Qt6/lib/libQt6Core.so.6"))))

(use-package eaf-evil
  :after eaf-browser
  :config (eaf-enable-evil-intergration))

(use-package eaf-browser)
(use-package eaf-org-previewer)
(use-package eaf-pdf-viewer)
(use-package eaf-ocap-cad-viewer
  :load-path "~/0/1-project/eaf-ocap-cad-viewer")


;; Other Dependencies
;; For eg. `guix shell --container --network emacs'
;; - Some lisp sources, like filecache.el, have store paths inlined.
;;   Others, like locate.el, do not?
;; - Am omitting:
;;   - arc-mode.el, bc we use avfs
;;   - w3m for Gnus
'(:guix (bash
         coreutils
         bind:utils ; for #'dig
         findutils ; for #'locate
         ghostscript ; for ps2pdf, for org-agenda.el ; XXX: guix issue#50625
         git
         gnupg
         gzip
         imagemagick ; for #'mpc
         net-tools ; for #'arp
         nss-certs
         openssh ; for tramp
         openssl ; for erc/erc-sasl.el
         optipng ; > pngnq pngcrush ; for image-dired-external.el
         patch ; for debugs
         poppler-qt6 ; for pdfinfo, for doc-view.el
         rclone ; for net/tramp-rclone.el
         sed
         sshfs  ; for net/tramp-sshfs.el
         zip ; for org/ox-odt.el (not that i use it)
         ))
(put 'narrow-to-page 'disabled nil)
