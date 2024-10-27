#!/usr/bin/env bash
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
  ARGS+=(${CONTAINER_ARGS[@]})
fi

guix shell ${ARGS[@]} \
  --no-offload \
  -L ~/.emacs.d/modules \
  --with-input=emacs=emacs-next \
  --with-input=emacs-minimal=emacs-next-minimal \
  --with-input=emacs-dirvish=emacs-dirvish-patched \
  --with-input=emacs-general=emacs-general-next \
  --without-tests=emacs-lispy \
  emacs \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/init.el) \
  $(~/.emacs.d/extract-packages.scm < ~/.emacs.d/early-init.el) \
  -- emacs $@
