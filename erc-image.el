;;; erc-image.el --- Show received image urls in the ERC buffer

;; Copyright (C) 2012  Jon de Andrés Frías
;; Copyright (C) 2012  Raimon Grau Cuscó

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

(defun erc-image-get-url ()
  "This function fetchs the url found at cursor point only if it
has these extensions: png, jpg or jpeg"
  (let* ((url (thing-at-point 'url))
         (file-name (concat "/tmp/" (car (last (split-string url "/"))))))
    (when (string-match "\\.png$\\|\\.jpg$\\|\\.jpeg$" url)
      ;; alternative way is
      ;; http://stackoverflow.com/questions/4448055/download-a-file-with-emacs-lisp
      (url-copy-file url file-name t t)
      (create-image file-name))))

(defun erc-image-find-image ()
  "Moves the cursor to the first match with 'http' in the buffer.
The buffer here is just the received message in erc"
  (beginning-of-buffer)
  (let ((url-found (search-forward "http" nil t)))
    (when url-found
      (erc-image-get-url))))

(defun erc-image-show-url ()
  "Function to display an image in the erc buffer"
  (interactive)
  (let ((image (erc-image-find-image)))
    (when image
      (beginning-of-buffer)
      (insert-image image "images")
      (insert "\n")
      (put-text-property (point-min) (point-max) 'read-only t))))

(add-hook 'erc-insert-modify-hook 'erc-image-show-url t)
(add-hook 'erc-send-modify-hook 'erc-image-show-url t)

(provide 'erc-image)
;;; erc-image.el ends here
