# SPDX-FileCopyrightText: 2024 antlers <antlers@illucid.net>
# SPDX-License-Identifier: GPL-3.0-or-later

.ONESHELL:

.PHONY: all

all: current

README.md: SHELL != realpath ./run.sh
README.md: .SHELLFLAGS = -nw --batch -l ./early-init.el -l ./init.el --eval
README.md: README.md.src init.el early-init.el
	(progn
	  (find-file "./README.md.src")
	  (org-mode)
	  (setq org-confirm-babel-evaluate nil)
	  (org-babel-execute-buffer))

current: init.el early-init.el run.sh modules/gnu/packages/emacs-aux.scm
	./run.sh -P && touch -h ./current
