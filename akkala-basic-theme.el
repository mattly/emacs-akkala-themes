;; akkala-theme-basic A proof-of-concept akkala-flat theme

;;; Commentary:

;;; Code:
(require 'akkala-themes)
(require 'akkala-themes-roles)

(defvar akkala/basic-colors
  '(
     (base-0    "#ffffff")
     (base-1    "#eeeeee")
     (base-2    "#aaaaaa")
     (base-3    "#444444")
     (base-4    "#222222")
     (blue-0    "#ddddff")
     (blue-2    "#1111aa")
     (cyan-0    "#ddffff")
     (cyan-2    "#11aaaa")
     (green-0   "#ddffdd")
     (green-2   "#11aa11")
     (purple-0  "#ffaaff")
     (purple-2  "#aa11aa")
     (red-0     "#ffdddd")
     (red-2     "#aa1111")
     (yellow-0  "#ffffdd")
     (yellow-1  "#dddd44")
     (yellow-2  "#aaaa11")))

(def-akkala-theme akkala-basic akkala/basic-colors akkala/flat-roles)

;;; akkala-theme-basic.el ends here
