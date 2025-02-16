;; SPDX-FileCopyrightText: 2024 antlers <antlers@illucid.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gnu packages crates-io-aux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-io))

(define-public rust-cargo-makedocs
  (package
    (name "rust-cargo-makedocs")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-makedocs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xs4rxfrig5s8zfvk8z2pgq7wm45qh88f170jzqaa2wh8jag9429"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap" ,rust-clap-2)
                       ("rust-semver" ,rust-semver-0.9)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-derive" ,rust-serde-derive-1)
                       ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/Bunogi/cargo-makedocs")
    (synopsis
     "cargo doc wrapper that only builds the documentation you care about")
    (description
     "This package provides a cargo doc wrapper that only builds the documentation you
care about.")
    (license license:expat)))
