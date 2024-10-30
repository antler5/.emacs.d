;; SPDX-FileCopyrightText: 2024 antlers <antlers@illucid.net>
;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (gnu packages file-systems-aux)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  )

(define-public tagsistant
  (let ((commit "0dabdca1077136b7626a2977410f910689c235b7")
        (revision "0"))
    (package
      (name "tagsistant")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/StrumentiResistenti/Tagsistant")
               (commit commit)))
         (sha256
          (base32 "06pi712grfz790n2v75qbxwdm4dy96h25fyd9jkx3wfm1hc83y5m"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
        (list
          #:phases
          #~(modify-phases %standard-phases
              (add-before 'configure 'bootstrap
                (lambda* (#:key outputs #:allow-other-keys)
                  ;; libtool.m4 -> /usr/share/aclocal/libtool.m4
                  ;; lt~obsolete.m4 -> /usr/share/aclocal/lt~obsolete.m4
                  ;; ltoptions.m4 -> /usr/share/aclocal/ltoptions.m4
                  ;; ltsugar.m4 -> /usr/share/aclocal/ltsugar.m4
                  ;; ltversion.m4 -> /usr/share/aclocal/ltversion.m4

                  (invoke "ls" "-la")
                  (invoke "ls" "-la" "..")
                  (invoke "ls" "-la" "./m4")

                  (map (lambda (pair)
                         (let ((file-name (car pair))
                               (src (cadr pair)))
                           (invoke "unlink" file-name)
                           (invoke "ln" "-s" src file-name)))
                       `(("./m4/libtool.m4"     ,(string-append #$(this-package-input "libtool")
                                                                "/share/aclocal/libtool.m4"))
                         ("./m4/lt~obsolete.m4" ,(string-append #$(this-package-input "libtool")
                                                                "/share/aclocal/lt~obsolete.m4"))
                         ("./m4/ltoptions.m4"   ,(string-append #$(this-package-input "libtool")
                                                                "/share/aclocal/ltoptions.m4"))
                         ("./m4/ltsugar.m4"     ,(string-append #$(this-package-input "libtool")
                                                                "/share/aclocal/ltsugar.m4"))
                         ("./m4/ltversion.m4"   ,(string-append #$(this-package-input "libtool")
                                                                "/share/aclocal/ltversion.m4"))
                         ("./depcomp"           ,(string-append #$(this-package-input "automake")
                                                                "/share/automake-1.16/depcomp"))
                         ("./install-sh"        ,(string-append #$(this-package-input "automake")
                                                                "/share/automake-1.16/install-sh"))
                         ("./ltmain.sh"         ,(string-append #$(this-package-input "libtool")
                                                                "/share/libtool/build-aux/ltmain.sh"))))
                  (invoke "ln"
                          "-s" (string-append #$(this-package-input "libtool")
                                              "/share/libtool/build-aux/config.sub")
                          "./config.sub")

                  (invoke "ls" "-la" "./m4")
                  (invoke "ls" "-la" ".")

                  (invoke "mv" "./src/plugins/Makefile.am" "./src/plugins/Makefile.in")

                  ;; (invoke "autoconf" "./configure.ac")
                  (invoke "autoconf")

                  )))))
      (native-inputs
        (list autoconf fuse-2 glib libdbi perl))
      (inputs
        (list automake libextractor libtool))
      (home-page "https://www.tagsistant.net/")
      (synopsis "Semantic filesystem for Linux, with relation reasoner, autotagging plugins and a deduplication service")
      (description "Semantic filesystem for Linux, with relation reasoner, autotagging plugins and a deduplication service")
      (license license:gpl2))))
