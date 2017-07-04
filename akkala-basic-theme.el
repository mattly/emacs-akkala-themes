;; akkala-theme-basic --- A proof-of-concept akkala-flat theme

;;; Commentary:
;; What do you want from me?

;;; Code:
(require 'akkala-themes)
(require 'akkala-themes-roles)

(defvar akkala/basic-colors
  '((bg-focus "#ffffff")
    (bg-main  "#eeeeee")
    (bg-dim   "#e2e2e2")
    (bg-soft  "#dddddd")
    (fg-soft  "#666666")
    (fg-dim   "#444444")
    (fg-main  "#222222")
    (fg-focus "#000000")

    (bg-red    "#ffdddd")
    (bg-purple "#ffaaff")
    (bg-blue   "#ddddff")
    (bg-cyan   "#ddffff")
    (bg-green  "#ddffdd")
    (bg-yellow "#ffffdd")

    (bg-focus-red "#ffeeee")
    (bg-focus-green "#eeffee")

    (fg-red    "#aa1111")
    (fg-purple "#aa11aa")
    (fg-blue   "#1111aa")
    (fg-cyan   "#11aaaa")
    (fg-green  "#11aa11")
    (fg-yellow "#aaaa11")

    (bold-green "#00ee00")
    (bold-red "#ee0000")
    (bold-yellow "#eeaa00")))

(def-akkala-theme akkala-basic akkala/basic-colors akkala/flat-roles)

;;; akkala-theme-basic.el ends here
