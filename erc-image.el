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

;; 

;;; Code:
(require 'erc)

(defun erc-wget-url ()
  "This function foo"
  (let* ((url (thing-at-point 'url))
         (file-name (concat "/tmp/" (car (last (split-string url "/"))))))
    (shell-command (format "wget  %s --output-document=%s" url file-name))
    file-name))

(defun erc-show-image ()
  (beginning-of-buffer)
  (let ((url-found (search-forward "http" nil t)))
    (when url-found
      (create-image (erc-wget-url)))))

(defun erc-show-url ()
  (interactive)
  (let ((image (erc-show-image)))
    (when image
      (beginning-of-buffer)
      (insert-image image "images")
      (insert "\n"))))

(add-hook 'erc-insert-modify-hook 'erc-show-url t)
(add-hook 'erc-send-modify-hook 'erc-show-url t)



(provide 'erc-image)
;;; erc-image.el ends here
