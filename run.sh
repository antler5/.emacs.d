#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 antlers <antlers@illucid.net>
# SPDX-License-Identifier: GPL-3.0-or-later
set -xeuo pipefail

export GUIX_PACKAGE_PATH+="$HOME/.emacs.d/modules"

CONTAINER_ARGS=(
  --pure --container --network --nesting --emulate-fhs
  --share="$HOME"
  --share="/tmp"
  --share="$XDG_RUNTIME_DIR"
  --share="/media"
  --preserve='^DISPLAY$' --preserve='^XAUTHORITY$' --expose="$XAUTHORITY"
  --preserve='^XDG_' --preserve='^WAYLAND_'
  --preserve='^DBUS_' --expose=/var/run/dbus
  --preserve='TERM' --preserve='LANG'
  --expose=/sys/dev --expose=/sys/devices --expose=/dev/dri
)

[[ -L /home/antlers/.config/emacs/current ]] \
  && unlink /home/antlers/.config/emacs/current
guix shell ${CONTAINER_ARGS[@]} \
  --root="$HOME"/.config/emacs/current \
  -L ~/.config/emacs/modules \
  --with-input=emacs=emacs-next-pgtk \
  --with-input=emacs-minimal=emacs-next-minimal \
  --with-input=emacs-dirvish=emacs-dirvish-patched \
  --with-input=emacs-general=emacs-general-next \
  --with-input=emacs-embark=emacs-embark-patched \
  --with-input=emacs-lsp-mode=emacs-lsp-mode-next \
  --without-tests=emacs-lispy \
  --without-tests=emacs-explain-pause-mode \
  emacs \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/README.md.src) \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/init.el) \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/early-init.el) \
  -- emacs "${@}"
