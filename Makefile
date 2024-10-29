# SPDX-FileCopyrightText: 2024 antlers <antlers@illucid.net>
# SPDX-License-Identifier: GPL-3.0-or-later

.PHONY: all

all: README.md

.ONESHELL:

README.md: SHELL != realpath ./run.sh
README.md: .SHELLFLAGS = -nw --batch -l ./early-init.el -l ./init.el --eval
README.md: README.md.src
	(progn
	  (find-file "./README.md.src")
	  (org-mode)
	  (setq org-confirm-babel-evaluate nil)
	  (org-babel-execute-buffer))
