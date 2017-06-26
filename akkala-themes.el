;;; akkala-themes.el --- a pack of minimalist themes
;;
;; Copyright (C) 2017 Matthew Lyon
;;
;; Author: Matthew Lyon <matthew@lyonheart.us>
;; Keywords: themes
;; URL: https://github.com/mattly/emacs-akkala-themes
;; Version: 2017-06
;; Package-Requires: ((emacs "24.1"))
;;
;;; Commentary:
;;
;; Akkala Themes is a pack of minimalistic themes, inspired by Alabaster for
;; Light Table, FlatWhite for Atom, and my own Farmhouse theme for Emacs.
;;
;;; Code:
(require 'akkala-themes-common)

(defgroup akkala-themes nil
  "Options for akkala-themes."
  :group 'faces)

(defcustom akkala-themes-enable-bold t
  "If nil, bold will be disabled across all faces."
  :group 'akkala-themes
  :type 'boolean)

(defcustom akkala-themes-enable-italic t
  "If nil, italic will be disabled across all faces."
  :group 'akkala-themes
  :type 'boolean)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide 'akkala-themes)
;;; akkala-themes.el ends here
