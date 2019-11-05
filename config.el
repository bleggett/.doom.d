;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here


;;Use custom font
(setq doom-font (font-spec :family "Droid Sans Mono" :size 17))

;; No quit prompt please
(setq confirm-kill-emacs nil)

;;Tighten which-key interval
(setq which-key-idle-delay 0.4)

;;Who needs a line number gutter, just takes up space
(setq display-line-numbers-type nil)

;;GPG Key ID for encryption
(setq epa-file-encrypt-to '8F92EEB450612F77)

;; macOS specific config
(when (eq system-type 'darwin)
  ;; These two lines remap macOS Option to emacs Meta key
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)

  ;;This must be set to True for the emacs-mac port in order to get proper macOS maximize behavior
  (menu-bar-mode t)
  )

(after! evil
  ;; Twiddle evil cursor visuals for insert state
  (setq evil-insert-state-cursor '(bar "orange"))

  ;; I want C-w o to close other windows
  (define-key evil-window-map "o" 'delete-other-windows)

  ;; I want C-w F to open a new frame
  (define-key evil-window-map "F" 'make-frame)
  )


;; Autosave dirty Org buffers when they are no longer visible
(add-to-list 'focus-out-hook (lambda () (save-some-buffers t nil)))

;; Format Go files on save
;; (add-hook! 'before-save-hook #'gofmt-before-save)

;; Time display in modeline settings

(add-hook! display-time-mode
  (setq display-time-format "%l:%M %p %b %e")
  (setq display-time-default-load-average nil)
  )

(display-time-mode 1)

;; Tweak company-mode display options
(after! company
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
  )
;;When I quit, don't ask me if I want to kill processes
(setq confirm-kill-processes nil)

;; Delete to (freedesktop.org) trash
(setq delete-by-moving-to-trash t)

;; (setq +ivy-project-search-engines '(ag rg))

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
 :prefix "/"
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
;; END Custom keybinds

;;BEGIN ORGMODE CONFIG
;; Put orgmode config here, spacemacs does not use the built-in org version
(after! org
  (setq org-directory "~/Dropbox/org")

  (setq org-default-notes-file "~/Dropbox/org/refile.org")
  (setq org-archive-location (concat "archive/archive-"
                                     (format-time-string "%Y%m" (current-time))
                                     ".org::"))
  '(org-enforce-todo-dependencies t)

  ;; Tell org that we're using org-protocol
  (add-to-list 'org-modules 'org-protocol)

  ;; ;;Syntax mode highight to ORG #+BEGIN_SRC code block mappings.
  ;; (add-to-list 'org-src-lang-modes (cons "JSON" 'json))
  ;; (add-to-list 'org-src-lang-modes (cons "CS" 'csharp))
  ;; (add-to-list 'org-src-lang-modes (cons "JS" 'js2))

  ;; Whenever I invoke org-agenda, it should open in another frame
  (setq org-agenda-window-setup 'other-frame)

  ;;Org-babel graphing stuff
  (setq org-ditaa-jar-path "~/Dropbox/org/ditaa.jar")
  (add-hook! 'org-babel-after-execute-hook 'bml-org-mode/display-inline-images 'append)

  ;;                                     ; Make babel results blocks lowercase
  (setq org-babel-results-keyword "results")

  (defun bml-org-mode/display-inline-images ()
    (condition-case nil
        (org-display-inline-images)
      (error nil)))

  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((ditaa . t))))

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)

  ;; Bullet list symbols
  (setq-default org-bullets-bullet-list '("•" "◊" "Δ" "†"))

  ;; Capture templates for: TODO tasks, Notes, and org-protocol
  (setq org-capture-templates
        (quote (("t" "todo" entry (file org-default-notes-file)
                 "* TODO %?\n%U\n%a\n")
                ("n" "note" entry (file org-default-notes-file)
                 "* %? :NOTE:\n%U\n%a\n")
                ("p" "Protocol" entry (file org-default-notes-file)
                 "* %^{Title}\nSource: %:link\nAt: %u\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                ("L" "Protocol Link" entry (file org-default-notes-file)
                 "* [[%:link][%:description]] :NOTE:\nCaptured On: %U" :immediate-finish t))))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))

  ;;Let me refile to top level headers
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; What files to include in the agenda
  ;; include subfolders, and archives for search purposes
  (let ((default-directory org-directory))
    (setq org-agenda-files (list org-directory (expand-file-name "conferences") (expand-file-name "archive"))))

  ;; Enable org-super-agenda
  (org-super-agenda-mode 1)

  ;; Since we use multiple blocks to 'fake' a complete agenda, drop separator
  (setq org-agenda-block-separator nil)

  ;; Org super agenda headers have their own keymap - I want it to clone the `evil-org-agenda' map
  (setq org-super-agenda-header-map nil)

  ;;Don't let children inherit tags
  ;; (setq org-use-tag-inheritance nil)

  ;; I want refile.org's file-level :REFILE: to be inherited, but only by the top-level heading
  (setq org-tags-match-list-sublevels nil)

  ;; Whenever I invoke org-agenda, it should open in another frame
  ;; (setq org-agenda-window-setup 'other-frame)

  ;; Custom agenda command definitions
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

  ) ;; END ORGMODE CONFIG

;; Indentation tweaks
(setq-default indent-tabs-mode nil)
(defun my-setup-indent (n)
  ;;General tab size
  (setq-default tab-width n)
  ;; java/c/c++
  (setq-default c-basic-offset n)
  (setq-default go-tab-width n)
  ;; web development
  (setq-default coffee-tab-width n) ; coffeescript
  (setq-default javascript-indent-level n) ; javascript-mode
  (setq-default js-indent-level n) ; js-mode
  (setq-default js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-default web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-default web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-default web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-default css-indent-offset n) ; css-mode
  (setq-default typescript-indent-level n)

  )
(my-setup-indent 4)

;; Define a custom Cloudformation minor mode that runs `cfn-lint'
(define-derived-mode cfn-yaml-mode yaml-mode
  "CFN-YAML"
  "Simple mode to edit CloudFormation template in YAML format.")

(add-to-list 'magic-mode-alist
             '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode))


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
  )
