;;; Copyright © 2024 antlers <antlers@illucid.net>

(define-module (gnu packages emacs-aux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages emacs-xyz))

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
