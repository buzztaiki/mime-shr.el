;;; mime-shr.el --- SEMI module that render html mail using shr.

;; Copyright (C) 2013  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; Keywords: mail
;; URL: https://github.com/buzztaiki/mime-shr-el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;
;; Put followings in your .wl:

;;      (require 'mime-shr)
;;      (mime-shr:insinuate)

;;; Code:

(require 'mime-parse)
(require 'mime)
(require 'alist)
(require 'shr)

(defgroup mime-shr nil
  "SEMI module that render html mail using shr."
  :group 'mail)

(defcustom mime-shr:trust-addresses nil
  "Mail addresses that is arrowed to display image."
  :group 'mime-shr
  :type '(repeat string))

(defun mime-shr:insinuate ()
  "Insinuate `mime-shr' module to SEMI."
  (let ((match (mime-shr:find-ctree
                mime-preview-condition '(text html visible))))
    (when match
      (setcdr match nil)))
  (ctree-set-calist-strictly
   'mime-preview-condition
   '((type . text)
     (subtype . html)
     (body . visible)
     (body-presentation-method . mime-shr:preview-text/html)))
  (set-alist 'mime-view-type-subtype-score-alist
             '(text . html) 3))

(defun mime-shr:find-ctree (ctree fields)
  (if (null fields)
      ctree
    (mime-shr:find-ctree
     (cdr (assq (car fields) (cdr ctree)))
     (cdr fields))))

(defun mime-shr:preview-text/html (entity situation)
  (goto-char (point-max))
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      (mime-insert-text-content entity)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (delete-region (point-min) (point-max))
        (let ((shr-inhibit-images (not (mime-shr:trust-entity-p entity))))
          (shr-insert-document dom)))
      (mime-shr:realize-before-string))))

(defun mime-shr:trust-entity-p (entity)
  (mime-shr:trust-address-p
   (mime-entity-fetch-field (mime-find-root-entity entity) "From")))

(defun mime-shr:trust-address-p (address)
  (member (cadr (eword-extract-address-components address))
          mime-shr:trust-addresses))

(defun mime-shr:realize-before-string ()
  (dolist (o (overlays-in (point-min) (point-max)))
    (let ((s (overlay-get o 'before-string))
          (beg (overlay-start o))
          (end (overlay-end o)))
      (when s
        (save-excursion
          (goto-char beg)
          (remove-overlays beg end 'before-string s)
          (insert s))))))

(provide 'mime-shr)
;;; mime-shr.el ends here
