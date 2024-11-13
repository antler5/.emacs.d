#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2024 antlers <antlers@illucid.net>
# SPDX-License-Identifier: GPL-3.0-or-later
set -x

export GUIX_PACKAGE_PATH+="$HOME/.emacs.d/modules"

CONTAINER_ARGS=(
  --pure --container --network --nesting
  --preserve='^DISPLAY$' --preserve='^XAUTHORITY$' --expose="$XAUTHORITY"
  --preserve='^DBUS_' --expose=/var/run/dbus
  --preserve='TERM'
  --expose=/sys/dev --expose=/sys/devices --expose=/dev/dri
  --share="$HOME"
)

ARGS=()
if [[ $1 == '-C' ]]; then
  shift;
  ARGS+=("${CONTAINER_ARGS[@]}")
fi

cd ~/.config/emacs
if [[ $1 == '-P' ]]; then
  shift;
  guix shell ${ARGS[@]} \
    --root="$HOME"/.config/emacs/current \
    -L ~/.emacs.d/modules \
    --with-input=emacs=emacs-next \
    --with-input=emacs-minimal=emacs-next-minimal \
    --with-input=emacs-dirvish=emacs-dirvish-patched \
    --with-input=emacs-general=emacs-general-next \
    --with-input=emacs-embark=emacs-embark-patched \
    --without-tests=emacs-lispy \
    --without-tests=emacs-explain-pause-mode \
    emacs \
    $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/README.md.src) \
    $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/init.el) \
    $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/early-init.el) \
    -- printf '%s' "Built profile $HOME/.config/emacs/current"
else
  make --check-symlink-times
  guix shell ${ARGS[@]} \
    --profile="$HOME"/.config/emacs/current \
    -- emacs "${@}"
fi
