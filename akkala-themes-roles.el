;; akkala-theme-roles.el --- Akkala Theme, Included Roles

;;; Commentary:

;;; Code:
(require 'akkala-themes-common)

(akkala/def-roles akkala/flat-roles
  ((default    :foreground fg-main  :background bg-main)
   (dim        :foreground fg-dim)
   (subtle     :foreground fg-soft)

   (comment    :foreground fg-dim
               :slant 'italic)
   (definition :foreground fg-main
               :weight 'bold)
   (directive  :foreground fg-main  :background bg-purple)
   (keyword    :foreground fg-main  :background bg-red)
   (number     :foreground fg-main  :background bg-cyan)
   (operator   :foreground fg-main  :background bg-yellow)
   (string     :foreground fg-main  :background bg-green)
   (symbol     :foreground fg-main  :background bg-blue)

   (region     :foreground fg-main  :background bg-focus)
   (highlight  :foreground fg-main  :background bg-focus)
   (match      :foreground fg-green :background bg-focus)
   (link       :foreground fg-blue
               :weight 'bold        :underline t)
   (selection  :foreground fg-main  :background bg-red)

   (error      :foreground fg-red
               :weight 'bold)
   (warning    :foreground fg-yellow
               :weight 'bold)
   (success    :foreground fg-green
               :weight 'bold)))

(provide 'akkala-themes-roles)
;;; akkala-theme-roles.el ends here
