;; SPDX-FileCopyrightText: 2025 antlers <antlers@illucid.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gnu packages fonts-aux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system font)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public font-dm-mono
  (let ((commit "57fadabfb200a77de2812540026c249dc3013077")
        (revision "0"))
    (package
      (name "font-dm-mono")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/googlefonts/dm-mono")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "07y3csk0vy3b3mq33bb73m63p9vyk8dhf6ysqnxabdpvy6d98gjy"))))
      (build-system font-build-system)
      (home-page "https://github.com/googlefonts/dm-mono")
      (synopsis "Designed by Colophon Foundry")
      (description "Commissioned from Colophon Foundry, with Creative Direction from the DeepMind team.")
      (license license:silofl1.1))))
