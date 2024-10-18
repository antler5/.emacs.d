#!/usr/bin/env bash
set -x

export GUIX_PACKAGE_PATH+="$HOME/.emacs.d/modules"

guix shell \
  -L ~/.emacs.d/modules \
  emacs \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/init.el) \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/early-init.el) \
  -- emacs $@
