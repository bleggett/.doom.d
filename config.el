;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here


;; BEGIN GENERAL EMACS CONFIG
;;Use custom font
(setq doom-font (font-spec :family "Droid Sans Mono" :size 17))

;; No quit prompt please
(setq confirm-kill-emacs nil)

;;Tighten which-key interval
(setq which-key-idle-delay 0.4)

;;Who needs a line number gutter, just takes up space
(setq display-line-numbers-type nil)

;;GPG Key ID for encryption
(setq epg-file-encrypt-to '8F92EEB450612F77)
(setf epg-pinentry-mode nil)

;;LSP perf thing
(setq read-process-output-max (* 1024 1024))

(after! lsp
        (lsp-register-custom-settings
        '(("gopls.experimentalWorkspaceModule" t t)))
)

;;cc-mode lsp/clang stuff
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

;; macOS specific config
(when (eq system-type 'darwin)
  ;; These two lines remap macOS Option to emacs Meta key
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)

  ;;This must be set to True for the emacs-mac port in order to get proper macOS maximize behavior
  ;; (menu-bar-mode t)
  )

;;Render images
(setq auto-image-file-mode 1)

(after! evil
  ;; Twiddle evil cursor visuals for insert state
  (setq evil-insert-state-cursor '(bar "orange"))

  ;; DO NOT USE, WEIRD
  ;; (setq evil-want-minibuffer 1)

  ;; I want C-w o to close other windows
  (define-key evil-window-map "o" 'delete-other-windows)

  ;; I want C-w F to open a new frame
  (define-key evil-window-map "F" 'make-frame)
  )


;; Autosave dirty Org buffers when they are no longer visible
(add-to-list 'focus-out-hook (lambda () (save-some-buffers t nil)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;When I quit, don't ask me if I want to kill processes
(setq confirm-kill-processes nil)

;; Format Go files on save
;; (add-hook! 'before-save-hook #'gofmt-before-save)

;; Time display in modeline settings
(add-hook! display-time-mode
  (setq display-time-format "%l:%M %p %b %e")
  (setq display-time-default-load-average nil)
  )

(display-time-mode 1)

;; Go mode hook
(add-hook! go-mode
  (setq gofmt-command "goimports")
  (gofmt-before-save)
  ;;Workaround for gopls and emacs 27
  (setq lsp-gopls-codelens nil)
  )

;;Prettier JS mode hook
;;
(add-hook! 'js2-mode-hook 'prettier-js-mode)

;;Rust mode hook
(add-hook! go-mode
  (setq rustic-lsp-server 'rust-analyzer)
  )
;; Prefer SVG mermaid diagrams
(setq mermaid-output-format ".svg")

;; Delete to (freedesktop.org) trash
(setq delete-by-moving-to-trash t)

;; DON'T HIDE THINGS FROM ME THAT'S NOT HOW RELATIONSHIPS WORK
(setq dired-omit-mode nil)

;; Indentation tweaks
(setq-default indent-tabs-mode nil)
;; (defun my-setup-indent (n)
;;   ;;General tab size
;;   (setq-default tab-width n)
;;   ;; java/c/c++
;;   (setq-default c-basic-offset n)
;;   (setq-default go-tab-width n)
;;   ;; web development
;;   (setq-default coffee-tab-width n) ; coffeescript
;;   ;; (setq-default javascript-indent-level n) ; javascript-mode
;;   ;; (setq-default js-indent-level n) ; js-mode
;;   ;; (setq-default js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
;;   (setq-default web-mode-markup-indent-offset n) ; web-mode, html tag in html file
;;   (setq-default web-mode-css-indent-offset n) ; web-mode, css in html file
;;   (setq-default web-mode-code-indent-offset n) ; web-mode, js code in html file
;;   (setq-default css-indent-offset n) ; css-mode
;;   ;; (setq-default typescript-indent-level n)

;;   )
;; (my-setup-indent 4)

;; Define a custom Cloudformation minor mode that runs `cfn-lint'
(define-derived-mode cfn-yaml-mode yaml-mode
  "CFN-YAML"
  "Simple mode to edit CloudFormation template in YAML format.")

(add-to-list 'magic-mode-alist
             '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode))

;;posframe stuff

;; (use-package! ivy-posframe
;;   :defer
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
;;         ivy-posframe-parameters
;;           '((left-fringe . 8)
;;             (right-fringe . 8))))

;; (ivy-posframe-mode 1)
;; END GENERAL EMACS CONFIG

;; BEGIN PACKAGE CONFIG
;; Tweak company-mode display options
(after! company
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
  )

;; I do not want company completion for $PATH binaries in shell mode, it's slow as ass
(after! sh-script
  (set-company-backend! 'sh-mode nil))

;; Configure grip markdown previewer to pull Github API token from ~/.authinfo.gpg
(use-package! grip-mode
  :defer
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential)))
  )

(after! flycheck
  ;; Set up cfn-lint integration if flycheck is installed
  ;; Get flycheck here https://www.flycheck.org/
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.

  Install cfn-lint first: pip install cfn-lint

  See `https://github.com/aws-cloudformation/cfn-python-lint'."

    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-yaml-mode))

  (add-to-list 'flycheck-checkers 'cfn-lint)
  (add-hook! 'cfn-yaml-mode-hook 'flycheck-mode)


  (setq flycheck-markdown-markdownlint-cli-config ".markdownlint.json")
  (setq flycheck-golangci-lint-enable-linters '("goimports" "godox" "bodyclose" "gosec"))
  )

;; When I switch Counsel/Projectile projects, open Magit buffer by default
(after! counsel-projectile
  (counsel-projectile-modify-action 'counsel-projectile-switch-project-action '((default counsel-projectile-switch-project-action-vc)))
  )
;; END PACKAGE CONFIG

;; BEGIN Custom keybinds

;; BEGIN Spacemacs muscle memory
;; Invert SPC b b and SPC b B to mirror Spacemacs more closely
(map!
 :leader
 :prefix "b"
 :desc "Filter current buffers"           "b" #'switch-to-buffer)

(map!
 :leader
 :prefix "b"
 :desc "Filter project buffers"           "B" #'persp-switch-to-buffer)

;; END Spacemacs muscle memory

;; SPC / r - resume last search
(map!
 :leader
 :prefix "s"
 :desc "Resume last search"           "r" #'ivy-resume)

;; Org binds are goofy, fix
(map!
 :leader
 :prefix "n"
 :desc "Find file in notes"           "f" #'+default/find-in-notes)

;; Put org subtree narrow on a more mnemonic keybind
(map!
 :leader
 :prefix "n"
 :desc "Narrow buffer to subtree"           "n" #'org-narrow-to-subtree)

(map!
 :leader
 :prefix "n"
 :desc "Widen buffer"           "N" #'widen)

;; Magit forge binds (under SPC-g-f)
(map!
 :leader
 :prefix "g"
 (:prefix ("f" . "Forge")
  :desc "Forge add repo" "a" #'forge-add-repository
  :desc "Forge pull" "f" #'forge-pull
  :desc "Forge browse post" "b" #'forge-browse-post
  :desc "Forge create pull request" "r" #'forge-create-pullreq
  :desc "Forge create issue" "i" #'forge-create-issue
 (:prefix ("s" . "Forge Search")
  :desc "Forge search issues" "i" #'forge-visit-issue
  :desc "Forge search PRs" "f" #'forge-visit-pullreq
  )
 ))

;; END Custom keybinds


;;BEGIN ORG-ROAM CONFIG
(require 'org-roam-protocol)

(use-package! org-journal
  :defer
  :config
  (setq org-journal-dir "~/Dropbox/org-roam/"
        org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-enable-agenda-integration t))

(add-hook! org-roam-mode 'org-roam-server-mode)

(after! org-roam
  (setq org-roam-db-update-method 'immediate)
  (setq org-roam-graph-viewer "open")
  (setq org-roam-directory "~/Dropbox/org-roam")
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-search" "s" #'deft
        :desc "org-roam-yesterday" "y" #'org-roam-dailies-yesterday
        ;; :desc "org-roam-today" "t" #'org-roam-dailies-today
        :desc "org-roam-today" "t" #'org-journal-new-entry
        :desc "org-roam-capture" "c" #'org-roam-capture)

  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%x"
           :file-name "websites/${slug}"
           :head "#+TITLE: ${title}
    #+ROAM_KEY: ${ref}
    - source :: ${ref}"
           :unnarrowed t
           :immediate-finish t))
        )
  )

(after! deft
  (setq deft-directory "~/Dropbox/org-roam/")
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-default-extension "org")
  (setq deft-extensions '("org" "gpg"))
  (setq deft-file-limit 10)
  )

(use-package! org-roam-server
  :defer
  :config
  (setq org-roam-server-host "localhost"
        org-roam-server-port 9987
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))

;;END ORG-ROAM CONFIG


;;BEGIN ORGMODE CONFIG
;; Put orgmode config here, spacemacs does not use the built-in org version
(after! org

  ;; ;; (setq org-default-notes-file "~/Dropbox/org/refile.org")
  ;; (setq org-archive-location (concat "archive/archive-"
  ;;                                    (format-time-string "%Y%m" (current-time))
  ;;                                    ".org::"))
  '(org-enforce-todo-dependencies t)

  ;; Tell org that we're using org-protocol
  ;; (add-to-list 'org-modules 'org-protocol)

  ;;Org-babel graphing stuff
  (setq org-ditaa-jar-path "~/Dropbox/org/ditaa.jar")
  (add-hook! 'org-babel-after-execute-hook 'bml-org-mode/display-inline-images 'append)

  ;;                                     ; Make babel results blocks lowercase
  (setq org-babel-results-keyword "results")

  (defun bml-org-mode/display-inline-images ()
    (condition-case nil
        (org-display-inline-images)
      (error nil)))

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)

  ;; Bullet list symbols
  (setq-default org-bullets-bullet-list '("•" "◊" "Δ" "†"))


  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

  ;; Include ".org.gpg" files in agenda query
  (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
    (setq org-agenda-file-regexp
          (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?" org-agenda-file-regexp)))

  ;; What files to include in the agenda
  ;; include subfolders, and archives for search purposes
  (setq org-agenda-files (list org-directory (expand-file-name "conferences") (expand-file-name "archive")))

  ;; ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        '(("b" "Ben's TODO View"
           ((tags "REFILE" ((org-agenda-overriding-header "Ben's Projects")
                            (org-super-agenda-groups
                             '((:name "Pending Refile"
                                      :tag "REFILE")
                               (:discard (:anything t))))))
            (tags-todo "-REFILE/!"
                       ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Active Projects"
                                  :and (:todo "TODO" :children todo))
                           (:name "Active Tasks"
                                  :and (:todo "TODO" :children nil))
                           (:name "Stuck Projects"
                                  :and (:todo ("WAITING" "HOLD") :children todo))
                           ))))
            ))
          )
        )

  )
(use-package! org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  ;; Since we use multiple blocks to 'fake' a complete agenda, drop separator
  (setq org-agenda-block-separator nil
        ;; Org super agenda headers have their own keymap - I want it to clone the `evil-org-agenda' map
        org-super-agenda-header-map nil
        ;;Don't let children inherit tags
        org-use-tag-inheritance nil))
;; END ORGMODE CONFIG
