;;; akkala-theme-common.el --- Akkala Theme, Common Utilities

;;; Commentary:
;;
;; Requires Emacs 24 or newer

;;; Code:
(defun akkala/make-role-def (x)
  "Defines a role from X."
  (list (car x) `(list ,@(cdr x))))

(defmacro akkala/def-roles (name roles)
  "Defines at NAME a series of ROLES for use in a theme."
  `(defvar ,name ',(mapcar #'akkala/make-role-def roles)))

(defun akkala/build-face (spec)
  "Builds a face specification from SPEC."
  (let ((face (car spec))
        (face-def (cdr spec)))
    (list face `((((type graphic)) ,@face-def)))))

(defmacro def-akkala-theme (theme-name colors roles)
  "Defines THEME-NAME with COLORS from the color ROLES."
  `(let* (,@(eval colors)
          ,@(eval roles))
     (deftheme ,theme-name)
     (apply
      'custom-theme-set-faces ',theme-name
      (mapcar
       #'akkala/build-face
       `(;; === Emacs built-in & included plugins =========
         ;; --- base faces
         (default ,default)
         (fringe :inherit 'default :foreground ,fg-dim)
         (region ,region)
         (highlight ,highlight)
         (cursor ,highlight)
         (minibuffer-prompt ,highlight)
         (shadow ,dim)
         (tooltip :inherit 'default)
         (secondary-selection :background ,bg-dim)
         (lazy-highlight ,highlight)
         (match ,match)
         (trailing-whitespace ,warning)
         (vertical-border ,dim)
         (link ,link)

         (warning ,warning)
         (success ,success)
         (error ,error)

         (font-lock-builtin-face ,symbol)
         (font-lock-comment-delimiter-face ,comment)
         (font-lock-comment-face ,comment)
         (font-lock-constant-face ,symbol)
         (font-lock-doc-face ,string)
         (font-lock-doc-string-face ,string)
         (font-lock-function-name-face ,definition)
         (font-lock-keyword-face ,keyword)
         (font-lock-negation-char-face ,operator)
         (font-lock-regexp-grouping-backslash ,string)
         (font-lock-regexp-grouping-construct ,string)
         (font-lock-string-face ,string)
         (font-lock-type-face ,definition)
         (font-lock-warning-face ,warning)
         (font-lock-variable-name-face ,definition)

         ;; --- mode-line / header-line
         (mode-line :foreground ,fg-main :background ,bg-focus)
         (mode-line-buffer-id :weight bold)
         ;; (mode-line-emphasis)
         ;; (mode-line-highlight)
         (mode-line-inactive :background ,bg-dim :foreground ,fg-soft)
         ;; (header-line)

         ;; === Built-in Plugins

         (hl-line :underline ,bg-focus)

         ;; --- linum-mode
         (linum ,subtle)

         ;; --- parens
         (show-paren-match ,match)
         (show-paren-mismatch ,warning)

         ;; --- window-divider
         (window-divider :inherit 'vertical-border)
         (window-divider-first-pixel :inherit 'window-divider)
         (window-divider-last-pixel :inherit 'window-divider)

         ;; === Third Party Plugins =======================

         ;; --- company
         (company-tooltip :inherit 'tooltip)
         (company-tooltip-common ,highlight)
         (company-tooltip-search ,highlight)
         (company-tooltip-selection ,selection)
         (company-scrollbar-bg :inherit 'tooltip)
         (company-scrollbar-fg ,highlight)
         (company-preview ,highlight)
         (company-preview-common ,symbol)
         (company-preview-search ,operator)

         ;; --- dired
         (diredp-dir-heading ,definition)
         (diredp-dir-name ,symbol)
         (diredp-directory ,symbol)
         (diredp-file-suffix ,directive)
         (diredp-symlink ,directive)

         ;; evil
         (evil-ex-search ,match)
         (evil-ex-substitute-matches     :foreground ,fg-red   :background ,bg-red
                                         :underline t)
         (evil-ex-substitute-replacement :foreground ,fg-green :background ,bg-green
                                         :underline t)
         (evil-search-highlight-persist-highlight-face ,match)

         ;; --- git gutter & co
         (git-gutter:added :foreground ,bold-green)
         (git-gutter+-added :foreground ,bold-green)
         (git-gutter-fr+-added :foreground ,bold-green)
         (git-gutter:deleted :foreground ,bold-red)
         (git-gutter+-deleted :foreground ,bold-red)
         (git-gutter-fr+-deleted :foreground ,bold-red)
         (git-gutter:modified :foreground ,bold-yellow)
         (git-gutter+-modified :foreground ,bold-yellow)
         (git-gutter-fr+-modified :foreground ,bold-yellow)

         ;; helm
         (helm-selection ,selection)
         (helm-match ,match)
         (helm-source-header ,definition)

         ;; --- highlight-numbers-mode
         (highlight-numbers-number ,number)

         ;; --- hlinum
         (linum-highlight-face :foreground ,fg-focus :background ,bg-main
                               :weight bold)

         ;; --- hydra
         (hydra-face-red :foreground ,fg-red :weight bold)
         (hydra-face-blue :foreground ,fg-blue :weight bold)
         (hydra-face-amaranth :foreground ,fg-purple :weight bold)
         (hydra-face-pink :foreground ,fg-green :weight bold)
         (hydra-face-teal :foreground ,fg-cyan :weight bold)

         ;; --- ivy
         (ivy-current-match ,match)

         ;; --- magit
         (magit-branch-current :background ,bg-purple :weight bold)
         (magit-branch-local :background ,bg-blue)
         (magit-branch-remote :background ,bg-green)
         (magit-section-heading :background ,bg-yellow :weight bold)

         (magit-diff-hunk-heading :background ,bg-dim :foreground ,fg-dim)
         (magit-diff-hunk-heading-highlight :background ,bg-focus :foreground ,fg-main)
         (magit-diff-added :background ,bg-green :foreground ,fg-dim)
         (magit-diff-added-highlight :background ,bg-focus-green :foreground ,fg-main)
         (magit-diff-removed :background ,bg-red :foreground ,fg-dim)
         (magit-diff-removed-highlight :background ,bg-focus-red :foreground ,fg-main)

         ;; --- powerline
         (powerline-active1 :inherit 'mode-line)
         (powerline-active2 :foreground ,fg-cyan :background ,bg-dim)
         (powerline-inactive1 :inherit 'mode-line-inactive)
         (powerline-active2 :foreground ,fg-cyan :background ,bg-soft)
         (powerline-inactive2 :background ,bg-dim)

         ;; --- rainbow-delimiters

         ;; --- spaceline
         (spaceline-highlight-face :foreground ,fg-blue)

         ;; --- spacemacs
         (spacemacs-normal-face :background ,bold-yellow :foreground ,fg-main)
         (spacemacs-insert-face :background ,bold-green :foreground ,fg-main)
         (spacemacs-visual-face :background ,bg-soft :foreground ,fg-main)

         ;; --- which-key
         (which-key-command-description-face ,default)
         (which-key-group-description-face :foreground ,fg-purple)
         (which-key-key-face :foreground ,fg-blue :weight bold)
         (which-key-local-map-description-face :weight bold)
         (which-key-separator-face ,dim)

         ;; === Major Modes ===============================
         ;; --- Clojure
         (clojure-keyword-face ,symbol)
         ;; --- Markdown
         (markdown-code-face :background ,bg-yellow)
         (markdown-header-face :background ,bg-purple :weight bold)
         (markdown-header-delimiter-face :background ,bg-purple :foreground ,fg-soft)
         (markdown-inline-code-face :background ,bg-yellow)
         (markdown-link-face :background ,bg-blue)
         (markdown-link-title-face :background ,bg-blue)
         (markdown-list-face :weight bold)
         (markdown-pre-face :background ,bg-yellow)
         (markdown-reference-face :background ,bg-blue))))))

(provide 'akkala-themes-common)

;;; akkala-theme-common ends here
