(require 'erc)

(defun erc-wget-url
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
