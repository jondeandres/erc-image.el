;;; erc-image.el --- Show received image urls in the ERC buffer

;; Copyright (C) 2012  Jon de Andrés Frías
;; Copyright (C) 2012  Raimon Grau Cuscó
;; Copyright (C) 2012  David Vázquez

;; Author: Jon de Andrés Frías <jondeandres@gmail.com>
;; Author: Raimon Grau Cuscó <raimonster@gmail.com>
;; Keywords: multimedia

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

;;; Show inlined images (png/jpg) in erc buffers.
;;; usage:

;;; (require 'erc-image)
;;;
;;; This plugin subscribes to hooks `erc-insert-modify-hook' and
;;; `erc-send-modify-hook' to download and show images. In this early
;;; version it's doing this synchronously.


;;; Code:
(require 'erc)

(defun erc-image-show-url ()
  (interactive)
  (goto-char (point-min))
  (search-forward "http" nil t)
  (let ((url (thing-at-point 'url))
        (file-name))
    (when (and url (string-match "\\.png$\\|\\.jpg$\\|\\.jpeg$" url))
      (setq file-name (concat "/tmp/" (car (last (split-string url "/")))))
      (when (string-match "\\.png$\\|\\.jpg$\\|\\.jpeg$" url)
        (url-retrieve url
                      (lambda  (status file-name buffer position)
                        (goto-char (point-min))
                        (search-forward "\n\n")
                        (write-region (point) (point-max) file-name)
                        (with-current-buffer buffer
                          (save-excursion
                            (let ((inhibit-read-only t))
                              (goto-char position)
                              (insert-image (create-image file-name) "[image]")
                              (insert "\n")
                              (put-text-property (point-min) (point-max) 'read-only t)))))
                      (list
                       file-name
                       (current-buffer)
                       (point-max)))))))

(add-hook 'erc-insert-modify-hook 'erc-image-show-url t)
(add-hook 'erc-send-modify-hook 'erc-image-show-url t)

(provide 'erc-image)
;;; erc-image.el ends here
