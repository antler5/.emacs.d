#!/usr/bin/env bash
guix shell emacs $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/init.el) $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/early-init.el) -- emacs --daemon
