#!/usr/bin/env bash
set -x

export GUIX_PACKAGE_PATH+="$HOME/.emacs.d/modules"

guix shell \
  -L ~/.emacs.d/modules \
  --with-input=emacs=emacs-next \
  --with-input=emacs-minimal=emacs-next-minimal \
  --without-tests=emacs-lispy \
  emacs \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/init.el) \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/early-init.el) \
  -- emacs $@
