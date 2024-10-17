;; init.el -*- lexical-binding: t; -*-
;;
;; TODO: Intangible eshell prompt?
;; TODO: EAT term

;; TODO: Corfu-doc
;; TODO: Fence edit
;; TODO: Projectile?
;; TODO: htmlize (for org-export)
;; TODO: Window undo / Ctl-o and Ctl-i how they were pls
;; - evil doesn't seem to like that idea??

;; TODO: Vertico for :e find-file style prompt?
;; TODO: Vertico hjkl or is the keymap gonna fix that

;; TODO: Sort words in region ignoring comments!!!

;; TODO: Looser, smex-style orderless?
;; TODO: dirvish: error in process sentinel: Wrong type argument: hash-table-p, nil [2 times]
;; TODO: Indent comment to column on tab
;; TODO: Get the ledger which-key label working
;; TODO: Absorb https://www.reddit.com/r/emacs/comments/18qa15/dired_discussion/
;; TODO: Absorb remaining ref-config
;; TODO: Absorb https://github.com/angrybacon/dotemacs/
;; TODO: What's up with all the litter?
;; TODO: Configure lispyville? (evil-)paredit?
;; TODO: Can :q pls close a buffer iff no other windows have it open?

;; The code is organized in pages, separated by formfeed characters.
;;
;; Substantial portions wisdom of:
;; - [doomemacs](https://github.com/doomemacs/doomemacs)
;; - emacs-bedrock
;; - mnewt's dotemacs repo
;; - kristofferbalintona.me
;;

;;; Native Compilation
(when (fboundp 'native-compile)
  (setq package-native-compile t)
  (custom-set-variables
    '(native-comp-async-report-warnings-errors nil)))


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
'(:guix (emacs-use-package ; This refers to my fork with :custom-face <string> [...]
          --with-git-url=emacs-use-package=https://github.com/antler5/use-package
          --with-branch=emacs-use-package=feature/custom-face-with-doc-string-backport-2.4.4
          ))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Some hooks are un-usable without this.
(setq use-package-hook-name-suffix nil)

;; Setup the no-op :guix keyword
(push ':guix use-package-keywords)
(defun use-package-normalize/:guix (_ keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg) '())))
(defun use-package-handler/:guix (name-symbol keyword archive-name rest state)
  (use-package-process-keywords name-symbol rest state))


;;; Packages with `use-package' extensions or no autoloads
(use-package dash    :guix emacs-dash)
(use-package delight :guix emacs-delight)
(use-package general
  :guix emacs-general
  :config (general-evil-setup))


;;; Guix Integration
(use-package guix
  :guix    emacs-guix
  :general (evil-leader-map "G" 'guix)
  :delight (guix-prettify-mode nil guix-prettify)
  :custom  (global-guix-prettify-mode t))

;; Locate profile and home packages
(-map (lambda (dir)
        (let ((default-directory dir))
          (normal-top-level-add-subdirs-to-load-path)))
      '("~/.guix-home/profile/share/emacs/site-lisp/"
        "~/.guix-profile/share/emacs/site-lisp/"))

;; Locate setuid binaries
;; Yes, I will pay the runtime cost of de-duping my PATH.
(when (file-directory-p "/run/setuid-programs")
  (setenv "PATH"
    (string-join
      (delete-dups
        (split-string (concat "/run/setuid-programs:"
                              (getenv "PATH"))
                      ":"))
      ":"))
  (add-to-list 'exec-path "/run/setuid-programs"))

;; Load fonts from XDG directory
(-map (-cut add-to-list 'bdf-directory-list <>)
      (-filter 'file-directory-p
               (-map (-cut concat <> "fonts/truetype")
                     (parse-colon-path (getenv "XDG_DATA_DIRS")))))


;;; Garbage Collection Pt. 2
(use-package gcmh
  :guix emacs-gcmh
  :delight
  :custom
  (gcmh-idle-delay 'auto) ; default is 15s
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (gcmh-mode t))


;;; Native Emacs Configuration
(use-package emacs
  :guix gzip
  :custom
  (scroll-bar-mode nil)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (cursor-in-non-selected-windows nil)  ; Hide the cursor in inactive windows
  (custom-file                          ; Disable custom-file persistence
   (make-temp-file "custom-" nil ".el"))
  (display-line-numbers-width 3)        ; Mode-toggle is bound in evil config
  (echo-keystrokes 0.001)               ; Display prefixes in minibuffer instantly
  (help-window-select t)                ; Focus new help windows when opened
  (highlight-nonselected-windows nil)   ; Hide active region in inactive windows
  (select-enable-clipboard t)           ; Merge System and Emacs clipboard
  (sentence-end-double-space nil)       ; Let one space end a sentence
  (switch-to-buffer-obey-display-actions t) ; "Make switching buffers more consistent"
  (tab-always-indent 'complete)         ; Preferred TAB behavior
  (visible-bell nil)                    ; Disable visual-bell
  (truncate-lines t)                    ; Truncate lines by default
  (recenter-positions '(5 top bottom))  ; Set re-centering positions

  ;; Customized Modes
  (blink-cursor-mode   nil) ; Disable cursor blinking
  (global-so-long-mode t)   ; Disable major + some minor modes in large files
  (repeat-mode         t)   ; Enable repeat-maps
  (save-place-mode     t)   ; Remember cursor position
  (savehist-mode       t)   ; Save history of minibuffer
  (tooltip-mode        nil) ; Disable tooltips

  ;; startup.el
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)

  :init
  (defun antlers/edit-init-el ()
    (interactive)
    (find-file
      (concat (getenv "HOME")
              "/.emacs.d/init.el")))

  :general (evil-leader-map "e" #'antlers/edit-init-el)
           ("C-x x T" #'visual-line-mode)

  :config
  (server-start)                    ; Start server for emacsclient
  (defalias 'yes-or-no-p 'y-or-n-p) ; Replace yes/no prompts with y/n

  ;; TODO: use general
  (global-set-key (kbd "<wheel-left>")
    (lambda ()
      (interactive)
      (scroll-left 1)))
  (global-set-key (kbd "<wheel-right>")
    (lambda ()
      (interactive)
      (scroll-right 1)))

  ;; GPG / Pinentry
  (setq epa-pinentry-mode 'loopback)
  (defun pinentry-emacs (desc prompt ok error)
    (concat (replace-regexp-in-string "%22" "\""
              (replace-regexp-in-string "%0A" "\n" desc))
            prompt ": "))

  ;; FIX: Correct forward-page behavior when on a page delimiter
  (advice-add 'forward-page :before
    (lambda (_)
      (when (and (looking-at page-delimiter)
              (> (match-end 0) (point)))
        (forward-char 1)))))

;; Default Tabs & Indents
(use-package emacs
  :custom
  (tab-width 2)
  (indent-tabs-mode nil)
  ;; Must come last to use modified `tab-width'.
  (tab-stop-list (number-sequence tab-width 120 tab-width))
  :config
  (add-to-list 'warning-suppress-types '(defvaralias))
  (-map (-cut defvaralias <> 'tab-width)
        '(c-basic-offset
          css-indent-offset
          evil-shift-width
          lisp-indent-offset
          sh-basic-offset)))

(use-package autorevert
  :custom
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
(use-package moody
  :guix emacs-moody
  :custom
  (display-time-default-load-average nil)
  (display-time-mode t)
  (moody-mode-line-height 20) ; ~1ch
  (x-underline-at-descent-line t)
  :custom-face
  (mode-line ((t (:overline   "#666666"
                  :underline  "#666666"
                  :foreground "#fef8ea")))) ; warmer text
  (mode-line-inactive ((t :background "#383838")))

  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package emacs
  :delight (eldoc-mode nil eldoc) ; Hide eldoc-mode lighter
  :custom  (column-number-mode t) ; Enable column display
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
           (rect (format "%d×%d" lines (if evil cols (1- cols))))
           (multi-line (format "%d/%d" lines chars))
           (t (format "1×%d" (if evil chars (1- chars))))))
      "0×0"))
  (add-to-list 'mode-line-position
    '((:propertize
        (:eval (spaceline--selection-info))
        display
        (min-width (5.0))))))


;; Theme, Graphics, and Fringe
(use-package emacs
  :guix (fontconfig ; needs specified, else `guix shell` won't export search path
         font-mononoki)
  :custom (indicate-buffer-boundaries 'left)
  :custom-face
  (default ((t (:family "mononoki" :height 110))))
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

(use-package prettify-symbols-mode
  :ghook 'lisp-mode-hook 'lisp-data-mode-hook)

(use-package highlight-indent-guides
  :guix emacs-highlight-indent-guides
  :delight
  :ghook 'prog-mode-hook
  :custom
  ;; Issue #107: Method 'character breaks with whitespace.el
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-auto-even-face-perc 20)
  (highlight-indent-guides-auto-odd-face-perc 15))

;; Enable /all/ the icons
(use-package all-the-icons
  :guix emacs-all-the-icons
  :defer
  :config
  (let ((font-dest
          (cond
            ;; Default Linux install directories
            ((member system-type '(gnu gnu/linux gnu/kfreebsd))
             (concat (or (getenv "XDG_DATA_HOME")
                         (concat (getenv "HOME") "/.local/share"))
                     "/fonts/"))
            ;; Default MacOS install directory
            ((eq system-type 'darwin)
             (concat (getenv "HOME") "/Library/Fonts/")))))
    (unless (file-exists-p (concat font-dest "all-the-icons.ttf"))
      (all-the-icons-install-fonts)))
  ;; Customize the Scheme icon
  (add-to-list 'all-the-icons-extension-icon-alist
    '("scm" all-the-icons-fileicon "scheme"
      :height 0.75 :width 1.25
      :face all-the-icons-blue)))

(use-package all-the-icons-completion
  :guix  emacs-all-the-icons-completion
  :init  (add-function :before-while
           all-the-icons-completion-mode
           #'display-graphic-p)
  :ghook ('marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
         'vertico-mode-hook
         'corfu-mode-hook)

(use-package kind-icon
  :guix   emacs-kind-icon
  :after  corfu
  ;; "To compute blended backgrounds correctly"
  :custom (kind-icon-default-face 'corfu-default)
  :ghook  ('corfu-margin-formatters #'kind-icon-margin-formatter))


;; Butlers
(use-package no-littering
  :guix   emacs-no-littering
  :config (no-littering-theme-backups)
  :custom (auto-save-file-name-transforms
           `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package ws-butler
  :delight
  :guix emacs-ws-butler
  :config (ws-butler-global-mode t))


;; Undo
(use-package undo-tree
  :guix emacs-undo-tree
  :delight
  :custom
  (undo-tree-auto-save-history t)     ; preserve undo history
  (undo-tree-histoy-directory-alist   ; where to keep it
   (expand-file-name "undo-tree" user-emacs-directory))
  :config (global-undo-tree-mode t))  ; enable globally


;; Lisp Parens
(use-package rainbow-delimiters
  :guix  emacs-rainbow-delimiters
  :ghook 'lisp-mode-hook
         'emacs-lisp-mode-hook
         'scheme-mode-hook
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
      ;; TODO: Tune these in a GUI frame pls
      (cl-callf color-desaturate-name (face-foreground face) 60)
      (cl-callf color-darken-name (face-foreground face) 25))))


;; Evil
(use-package evil
  :guix emacs-evil
  :demand
  :general ("C-M-u" #'universal-argument) ; evil-want-Y-yank-to-eol
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
           ;; Prioritize native org-cycle over evil-jump-forward
           ;; Ditto for return
           (:states 'normal
            :keymaps 'org-mode-map
            "<tab>" #'org-cycle
            "TAB"   #'org-cycle ; at some point <tab> wasn't enough?
            "<return>" #'org-return
            "RET"      #'org-return)
           (evil-leader-map
            "#"     #'display-line-numbers-mode)
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
  :guix    emacs-evil-collection
  :after   evil
  :delight (evil-collection-unimpaired-mode nil evil-collection-unimpaired)
  :custom  (evil-collection-setup-minibuffer t)
  :config  (evil-collection-init))

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
            "g b" #'evil-avy-goto-word-1
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
  :general (vertico-map
            "C-l"           #'kb/vertico-quick-embark
            "<backspace>"   #'vertico-directory-delete-char
            "C-<backspace>" #'vertico-directory-delete-word
            "RET"           #'vertico-directory-enter))

(use-package vertico-posframe
  :guix   emacs-vertico-posframe
  :after  vertico
  :custom (vertico-posframe-parameters '((frame-border-width . 8)))
  :custom-face (vertico-posframe-border ((t :background "#555")))
  :init   (general-after-gui
            (vertico-posframe-mode 1)))

(use-package marginalia
  :guix    emacs-marginalia
  :after   vertico
  :general (minibuffer-local-map "M-A" #'marginalia-cycle)
  :init    (marginalia-mode t))

(use-package corfu
  :guix  emacs-corfu
  :after (cape consult)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-min-width 35)
  (corfu-preselect-first nil)
  (corfu-quit-no-match nil)
  (global-corfu-mode t)
  :general (corfu-map
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
  :init (general-after-tty
          (corfu-terminal-mode 1)))

(use-package cape
  :guix emacs-cape
  :demand
  ;; These are defaults from the cape README.
  :general ("C-c p p" #'completion-at-point
            "C-c p t" #'complete-tag
            "C-c p h" #'cape-history
            "C-c p f" #'cape-file
            "C-c p k" #'cape-keyword
            "C-c p s" #'cape-symbol
            "C-c p a" #'cape-abbrev
            "C-c p i" #'cape-ispell
            "C-c p l" #'cape-line
            "C-c p w" #'cape-dict
            "C-c p r" #'cape-rfc1345)
  :init
  (add-to-list 'completion-at-point-functions 'cape-file))

(use-package embark
  :guix (emacs-embark ; This refers to my fork with a page-able which-key pop-up on `embark-collect'
          ; --with-git-url=emacs-embark=file:///home/maddhappy/projects/oantolin/embark
          ; --with-branch=emacs-embark=fix/issue-647
          )
  :after evil
  :general ("C-." #'embark-act         ; pick some comfortable binding
            "C-;" #'embark-dwim        ; good alternative: M-.
            "C-h B" #'embark-bindings) ; alternative for `describe-bindings'
           (:state 'normal
            ;; XXX: "C-." doesn't work in Zorin's Gnome Terminal.
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

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))

(use-package consult
  :guix emacs-consult
  :demand
  :general ("M-y"     #'consult-yank-pop        ; orig. yank-pop
            ;; C-x bindings (ctl-x-map)
            [remap switch-to-buffer] #'consult-buffer
            "C-x p b" #'consult-project-buffer  ; orig. project-switch-to-buffer
            "C-x r b" #'consult-bookmark        ; prefer to narrow consult-buffer
            ;; M-g bindings (goto-map)
            "M-g o"   #'consult-outline
            "M-g g"   #'consult-goto-line       ; orig. goto-line
            "M-g i"   #'consult-imenu
            ;; M-s bindings (search-map)
            "M-s d"   #'consult-find
            "M-s g"   #'consult-grep
            "M-s G"   #'consult-git-grep
            "M-s r"   #'consult-ripgrep
            "C-s"     #'consult-line
            "M-s l"   #'consult-line
            "M-s L"   #'consult-line-multi
            "M-s s"   #'isearch-forward)
            ;; Isearch integration
           (:keymaps 'isearch-mode-map
            "M-m"   #'consult-isearch-history ; like move-to-minibuffer
            "M-e"   #'consult-isearch-history ; orig. isearch-edit-string
            "M-s e" #'consult-isearch-history ; orig. isearch-edit-string
            "M-s l" #'consult-line            ; needed by consult-line to detect isearch (???)
            "M-s L" #'consult-line-multi)     ; needed by consult-line to detect isearch (???)
  :custom (consult-narrow-key "<"))

(use-package embark-consult
  :ghook ('embark-collect-mode-hook #'consult-preview-at-point-mode))


;;; Application Packages
(use-package dirvish
  :guix (emacs-dirvish
         emacs-pdf-tools
         imagemagick
         fd
         mediainfo
         tar unzip)
  :general (evil-leader-map
            "d" #'dirvish)
           (dirvish-mode-map
            "a"   #'dirvish-quick-access
            "N"   #'dirvish-narrow
            "TAB" #'dirvish-toggle-subtree)
           (:states 'motion
            "-" #'dired-jump
            "_" #'dirvish-layout-toggle)
  :custom
  (dirvish-attributes
    '(subtree-state
      all-the-icons
      git-msg
      file-size))
  (dirvish-quick-access-entries
   '(("d" "~/"         "Home")
     ("p" "~/projects" "Projects")
     ("s" "~/Sync"     "Sync")
     ))
  (dired-listing-switches
    "-l --almost-all --human-readable --group-directories-first --no-group")
  :init
  (dirvish-override-dired-mode))

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
  (setq tramp-use-ssh-controlmaster-options nil))

(use-package magit
  :guix emacs-magit emacs-transient-posframe
  :general (evil-leader-map "g" #'magit)
  :custom (magit-diff-refine-hunk t)
  :init (general-after-gui
          (transient-posframe-mode 1)))

(defvar ledger-dir
  (concat (getenv "HOME") "/Sync/org/ledger"))
(defvar ledger-init-file-name
  (concat ledger-dir "/ledgerrc"))
(use-package ledger-mode
  :guix emacs-ledger-mode
  :custom (ledger-clear-whole-transactions t)
          (ledger-post-account-alignment-column 2)
          (ledger-post-amount-alignment-column 49)
          (ledger-post-amount-alignment-at :decimal)
  :general (evil-leader-map
            "l" '((lambda ()
                    (interactive)
                    (find-file ledger-dir))
                  :which-key "ledger")))


;; Org
(use-package org
  :guix emacs-org
  :custom
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
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 9)))
  (org-refile-use-outline-path t)
  (org-refile-allow-creating-parent-nodes '(confirm))
  ;; Misc
  (org-treat-S-cursor-todo-selection-as-state-change nil)
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

;; This is it's own block because `org-agenda-map' isn't loaded by `org'.
(use-package org-agenda
  :after org
  :general (org-agenda-mode-map
            "j" #'evil-next-line
            "k" #'evil-previous-line))

(use-package ob
  :after org
  :custom (org-src-preserve-indentation t)
  :config (org-babel-do-load-languages
            'org-babel-load-languages
            '((shell . t)
              (gnuplot . t))))

(use-package ox
  :guix emacs-htmlize)

(use-package ox-haunt
  :guix emacs-ox-haunt
  :after ox)

(use-package svg-lib)

(use-package org-margin
  :guix (emacs-org-margin
         font-hack)
  :after org svg-lib
  :ghook 'org-mode-hook
  :init
  (defun antlers/margin-marker (family icon raise)
    (propertize
      (funcall (intern (concat "all-the-icons-" (symbol-name family)))
        icon
        :face 'all-the-icons-dsilver)
        'display `(raise ,raise)))
  (defun antlers/permute-margin-marker (family marker icon raise)
    (list (cons (concat "\\(#\\+" marker "\\)")
                (antlers/margin-marker family icon raise))
          (cons (concat "\\(#\\+" (upcase marker) "\\)")
                (antlers/margin-marker family icon raise))))
  :custom
  (org-margin-headers-set 'H-txt-min)
  (org-margin-markers
    (append
      (antlers/permute-margin-marker 'faicon "begin_src" "code" 0.05)
      (antlers/permute-margin-marker 'faicon "begin_quote" "quote-right" -0.1)
      (antlers/permute-margin-marker 'octicon "begin_comment" "comment" -0.1)
      (antlers/permute-margin-marker 'faicon "begin_example" "map-o" -0.1)
      (antlers/permute-margin-marker 'faicon "begin_music" "music" -0.1)
      (list
        (cons "\\(SCHEDULED:\\)" (antlers/margin-marker 'faicon "calendar" 0))
        (cons "\\(DEADLINE:\\)" (antlers/margin-marker 'faicon "calendar-times-o" 0))
        )))
  :config
  (add-to-list 'org-margin-headers
    (cons 'H-txt-min
      (-map (lambda (i)
              (let* ((i* (number-to-string i))
                     (face (intern (concat "org-level-" i*))))
                (svg-lib-tag i* nil
                  :padding 1.6
                  :font-family "Hack"
                  :font-weight 'bold
                  :foreground (face-background 'default nil t)
                  :background (face-foreground face nil t))))
            (-iota 6 1)))))


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
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-open-on-start nil))

(use-package org-node
  :guix emacs-org-node
  :after org org-roam
  :general ("M-s f" 'org-node-find)
           ("M-s i" 'org-node-insert-link)
           ("M-s s" #'org-node-series-dispatch)
  :custom
  (org-node-ask-directory "~/Sync/org/roam/")
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

  ;; "Old Default Setting", from:
  ;; https://github.com/meedstrom/org-node/wiki/Configuring-series#old-default-setting
  (setq org-node-series-defs
    (list '("d" :name "Dailies"
       :version 2
       :classifier (lambda (node)
                     (let ((path (org-node-get-file-path node)))
                       (when (string-search "~/Sync/org/roam/dailies" path)
                         (let ((ymd (org-node-helper-filename->ymd path)))
                           (when ymd
                             (cons ymd path))))))
       :whereami (lambda ()
                   (org-node-helper-filename->ymd buffer-file-name))
       :prompter (lambda (key)
                   (let ((org-node-series-that-marks-calendar key))
                     (org-read-date)))
       :try-goto (lambda (item)
                   (org-node-helper-try-visit-file (cdr item)))
       :creator (lambda (sortstr key)
                  (let ((org-node-datestamp-format "")
                        (org-node-ask-directory "~/Sync/org/roam/dailies"))
                    (org-node-create sortstr (org-id-new) key)))))))

(use-package org-node-fakeroam
  :guix emacs-org-node-fakeroam
  :custom
  (org-node-extra-id-dirs '("~/Sync/org/"))
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
(use-package eshell
  :general (evil-leader-map "q" #'eshell)

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
          (delete-window (get-buffer-window buffer))
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
  (evil-ex-define-cmd "wqa[ll]" #'save-and-quit-to-eshell*)
  (evil-ex-define-cmd "qa[ll]"  #'save-and-quit-all-to-eshell)
  (evil-ex-define-cmd "wqa[ll]" #'save-and-quit-all-to-eshell*))

(use-package eshell-syntax-highlighting
  :guix   emacs-eshell-syntax-highlighting
  :after  eshell
  :config (eshell-syntax-highlighting-global-mode t))

(use-package eshell-prompt-extras
  :guix   emacs-eshell-prompt-extras
  :after  eshell
  :init   (autoload 'epe-theme-lambda "eshell-prompt-extras")
  :custom (eshell-highlight-prompt nil)
          (eshell-prompt-function 'epe-theme-lambda))

(use-package eat
  :guix emacs-eat)


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
  :ghook ('(prog-mode-hook) #'flyspell-prog-mode)
         ('(git-commit-mode-hook) #'flyspell-mode)
  :custom
  (ispell-personal-dictionary ; Keep `ispell' dictionary in .emacs.d
   "/home/maddhappy/Sync/org/ispell-english.dict")
  (ispell-silently-savep t))  ; Don't ask before saving dict. updates

(use-package which-key
  :delight
  :guix emacs-which-key
  ;; "same as default, except all keys from local maps shown first"
  :custom (which-key-compute-remaps t)
          (which-key-sort-order 'which-key-local-then-key-order)
          (which-key-use-C-h-commands t)
          (which-key-idle-delay 0.5)
  :config (which-key-mode t))

(use-package whitespace
  :ghook  ('prog-mode-hook  #'whitespace-mode)
  :custom (whitespace-space 'whitespace-newline)
          (whitespace-style '(face lines-tail missing-newline-at-eof tab-mark))
  :custom-face (whitespace-line ((t :background unspecified :foreground "#555"))))

(use-package git-gutter
  :guix emacs-git-gutter
  :commands (git-gutter-mode)
  :ghook ('prog-mode-hook #'git-gutter-mode)
  :custom-face
  (git-gutter:added ((t :foreground "#cae682")))
  (git-gutter:deleted ((t :foreground "#e5786d")))
  (git-gutter:modified ((t :foreground "#8ac6f2"))))

(use-package rainbow-mode
  :delight
  :guix emacs-rainbow-mode
  :ghook ('prog-mode-hook #'rainbow-mode))


;; On-demand Minor Modes
'(:guix (emacs-csv-mode ; used to enable `csv-align-mode', but long line issues
         ;; emacs-go-mode ; gonna try tree-sitter first
         ;; emacs-makefile-ts ; not installed yet
         tree-sitter-go
         emacs-debbugs
         emacs-json-mode
         emacs-markdown-mode
         emacs-yaml-mode))

(use-package darkroom
  :guix    emacs-darkroom
  :general (evil-leader-map "f" #'darkroom-tentative-mode)
  :custom  (darkroom-text-scale-increase 1.25))


;; Other Dependencies
;; For eg. `guix shell --container --network emacs'
'(:guix (coreutils
         bash
         git
         gnupg
         ispell
         nss-certs
         openssh ; for tramp
         patch ; for debugs
         sed
         ))
