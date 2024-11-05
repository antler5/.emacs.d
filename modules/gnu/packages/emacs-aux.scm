;; SPDX-FileCopyrightText: 2024 antlers <antlers@illucid.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gnu packages emacs-aux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages password-utils))

(define-public emacs-keepass
  (let ((commit "0a14707fac0a74311cffbbcd2b6c7ee578977809")
        (revision "0"))
    (package
      (name "emacs-keepass")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/antler5/keepass.el")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32 "15l7yz9yi17dr873mgwibbi4wv5g3yw4nsx98lj8iqiqm5a5jnd8"))))
      (build-system emacs-build-system)
      (inputs (list keepassxc))
      (home-page "https://gitlab.com/grinn.amy/keepass.el")
      (synopsis "Retrieve passwords to the kill ring from KeePass.")
      (description "This package provides the command keepass-picker to quickly
retrieve selected KeePass passwords from within Emacs using
keepassxc-cli.")
      (license license:gpl3))))

(define-public emacs-org-roam-logseq
  (let ((commit "b76a900d938f829facf59d73006e8bddcc8c0363")
        (revision "0"))
    (package
      (name "emacs-org-roam-logseq")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://gist.githubusercontent.com/zot/ddf1a89a567fea73bc3c8a209d48f527/raw/"
               commit "/org-roam-logseq.el"))
         (sha256
          (base32 "04ijjz9yshc0840yrqxac1cgxplhn3yrhyfsryh6i0h54lcqk8hb"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'subst
              (lambda* (#:key source #:allow-other-keys)
                (use-modules (ice-9 binary-ports))
                (substitute* "org-roam-logseq.el"
                  (("(;; put the directory you use here)" all)
                   (string-append all "\n(require 'f)")))
                (with-atomic-file-replacement "org-roam-logseq.el"
                  (lambda (in out)
                    (let loop ()
                      (let ((u8 (get-u8 in)))
                        (unless (eof-object? u8)
                          (put-u8 out u8)
                          (loop))))
                    (display "\n(provide 'org-roam-logseq)\n" out))))))))
      (propagated-inputs
       (list emacs-f))
      (home-page "https://gist.github.com/zot/ddf1a89a567fea73bc3c8a209d48f527")
      (synopsis "")
      (description "")
      (license license:gpl3))))

(define-public emacs-org-logseq
  (let ((commit "054b4ba6fff43ae50be9c8fa6cf1da9f4e54b52b")
        (revision "0"))
    (package
      (name "emacs-org-logseq")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/llcc/org-logseq")
               (commit commit)))
         (sha256
          (base32 "106vjnc9z2wyr4l6k9vzb9b6ajrwfk385sl2cbyg9azn7dm0r9lr"))
         (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-dash))
      (home-page "https://github.com/llcc/org-logseq")
      (synopsis "")
      (description "")
      (license license:agpl3))))

(define-public emacs-general-next
  (let ((commit "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f")
        (revision "0"))
    (package
      (inherit emacs-general)
      (name "emacs-general-next")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/noctuid/general.el")
               (commit commit)))
         (sha256
          (base32 "1jillsr80l4wfbcqsxh3zbgbvmbfih2wcz518mgw9p9pwg4xwvy7"))
         (file-name (git-file-name name version)))))))

(define-public emacs-embark-patched
  (package
    (inherit emacs-embark)
    (name "emacs-embark-patched")
    (version "0.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/antler5/embark")
                    (commit "73369d8f92fafccfda5aef74f9b7d4353f393c2f")))
              (sha256
               (base32
                "0mjxvymxgmjb0wp3cxlkqjawkpl8njw3bjwncfrnhw8vvvv4dv8z"))
              (file-name (git-file-name name version))))))

(define-public emacs-dirvish-patched
  (package
    (inherit emacs-dirvish)
    (name "emacs-dirvish-patched")
    (version "2.0.53")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alexluigit/dirvish")
                    (commit "600b81d5b8adc8532cb31b72c9cf2fc981c678e9")))
              (sha256
               (base32
                "0035cydmdgdva5azcdcxnpzzjm191shg33q21sfh5jfy0xhs05si"))
              (file-name (git-file-name name version))))))

(define-public emacs-nerd-icons-corfu
  (let ((commit "7077bb76fefc15aed967476406a19dc5c2500b3c")
        (revision "0"))
    (package
      (name "emacs-nerd-icons-corfu")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/LuigiPiucco/nerd-icons-corfu")
               (commit commit)))
         (sha256
          (base32 "13m20k242zma6jw7pkbw89fk3dnbkwdajcpiyay5xx2l9241snb7"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-nerd-icons))
      (home-page "https://github.com/LuigiPiucco/nerd-icons-corfu")
      (synopsis "")
      (description "")
      (license license:gpl3))))

(define-public emacs-nerd-icons-completion
  (let ((commit "426a1d7c29a04ae8e6ae9b55b0559f11a1e8b420")
        (revision "0"))
    (package
      (name "emacs-nerd-icons-completion")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rainstormstudio/nerd-icons-completion")
               (commit commit)))
         (sha256
          (base32 "03kkyxc9v38v1fc69xqc70gwvsq4pr8bgsk8f6is9z2w7p4y08sm"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-compat emacs-nerd-icons))
      (home-page "https://github.com/rainstormstudio/nerd-icons-completion")
      (synopsis "")
      (description "")
      (license license:gpl3))))

(define-public emacs-use-package-fork
  (let ((commit "6d55bdd2d281d1b33da06eab0f920327fefdd2a8"))
    (package
      (inherit emacs-use-package)
      (name "emacs-use-package-fork")
      (version "2.4.4")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/antler5/use-package")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qdcwg2y054m11lby8d9cash8j8k1yyjm94x53djgbnr9dph41m3")))))))

;; First packaged in 2022 by Fredrik Salomonsson <plattfot@posteo.net>
;; Not upstream because it includes minified Javascript.
(define-public emacs-org-roam-ui
  (let ((commit "5ac74960231db0bf7783c2ba7a19a60f582e91ab")
        (revision "0"))
    (package
      (name "emacs-org-roam-ui")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/org-roam/org-roam-ui")
               (commit commit)))
         (sha256
          (base32 "0yic5rgp4f1rmi979if79kva7wn3rqnky423mqgf7sdw310h8akl"))))
      (build-system emacs-build-system)
      (arguments
       (list #:include #~(cons "^out" %default-include)))
      (propagated-inputs
       (list emacs-org-roam emacs-simple-httpd emacs-websocket))
      (home-page "https://github.com/org-roam/org-roam-ui")
      (synopsis "Web User Interface for Org Roam")
      (description
       "Org Roam UI is meant as a successor of Org Roam server that extends
functionality of Org Roam with a web app that runs side-by-side with Emacs,
providing a web interface for navigating around notes created within Org
Roam.")
      (license license:gpl3+))))

(define-public emacs-org-node-fakeroam
  (package
    (name "emacs-org-node-fakeroam")
    (version "0.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/meedstrom/org-node-fakeroam")
                    (commit "3e84dee8641191c87cc5a9e74999b2396db92972")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13pkrzldbbn1n5hc967baqhd2ncdfh1fdcw8f4107mbvbfh3g3v9"))))
    (build-system emacs-build-system)
    (propagated-inputs
      (list emacs-org-node
            emacs-org-roam))
    (home-page "https://github.com/meedstrom/org-node-fakeroam")
    (synopsis "Speed up org-roam and/or integrate org-node.")
    (description "Speed up org-roam and/or integrate org-node.")
    (license license:gpl3)))
