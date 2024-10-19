;;; early-init.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: (c) 2024 antlers <antlers@illucid.net>

;; Substantial portions inspired by:
;; - https://github.com/mnewt/dotemacs

;;; Commentary:

;; The code is organized into pages, separated by form feed characters.

;;; Code:


;; Never load outdated byte-code
(setq load-prefer-newer t)


;; Garbage Collection Pt. 1
(setq gc-cons-threshold most-positive-fixnum)


;;; Performance
;; Unset `file-name-handler-alist' (temporarily).
;; Opened files iterate through this list, but it never hits during startup.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))


;; Theme

;; Faster to disable these here (before they've been initialized)
;; TODO: better syntax / numbers / what are the the defaults anyways
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(height . 50) default-frame-alist)
(push '(width . 174) default-frame-alist)

;; Skip adjusting frame-size based on font-size
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Avoid flash of unstyled text
(set-face-attribute 'mode-line nil :background "#444"
                                   :foreground "#fef8ea"
                                   :overline "#666"
                                   :underline "#666")
(set-face-attribute 'default nil :background "#242424"
                                 :foreground "#f6f3e8")
(set-face-attribute 'fringe nil :background "#303030"
                                :foreground "#ddaa6f")

;; Ignore X Resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Guix will take care of this
(setq package-enable-at-startup nil)
(setq package-archives nil)
