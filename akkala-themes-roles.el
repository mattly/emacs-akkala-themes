;; akkala-theme-roles.el --- Akkala Theme, Included Roles

;;; Commentary:

;;; Code:
(require 'akkala-themes-common)

(akkala/def-roles akkala/flat-roles
  ((default    :foreground base-4   :background base-1)
   (dim        :foreground base-3   :background base-1)
   (subtle     :foreground base-2   :background base-1)

   (comment    :foreground base-3   :background base-1   :slant 'italic)
   (definition :foreground base-4   :weight 'bold)
   (directive  :foreground base-4   :background purple-0)
   (keyword    :foreground base-4   :background red-0)
   (number     :foreground base-4   :background cyan-0)
   (operator   :foreground base-4   :background yellow-0)
   (string     :foreground base-4   :background green-0)
   (symbol     :foreground base-4   :background blue-0)

   (region     :foreground base-4   :background base-0)
   (highlight  :foreground base-4   :background base-0)
   (match      :foreground green-2  :background base-0)
   (link       :foreground blue-2   :weight 'bold        :underline t)
   (selection  :foreground base-4   :background red-0)

   (error      :foreground red-2    :weight 'bold)
   (warning    :foreground yellow-2 :weight 'bold)
   (success    :foreground green-2  :weight 'bold)))

(provide 'akkala-themes-roles)
;;; akkala-theme-roles.el ends here
