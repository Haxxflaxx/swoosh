;;;; swoosh.el

;;;###autoload
(defvar swoosh-list '()
  "List to hold buffers for quick selection.")

;;;###autoload
(defun swoosh-toggle-buffer ()
  "Toggle the inclusion of current buffer in 'swoosh-list'."
  (interactive)
   (if (member (current-buffer) swoosh-list)
       (progn (setq swoosh-list (delete (current-buffer) swoosh-list))
              (message "Removed buffer from Swoosh"))
     (progn (push (current-buffer) swoosh-list)
            (message "Added buffer to Swoosh"))))

;;;###autoload
(defun swoosh-prev-buffer ()
  "Switch to previous buffer in 'swoosh-list'.
Wraps around when reaching the beginning of the list."
  (interactive)
  (let ((current-pos (cl-position (current-buffer) swoosh-list)))
    (if current-pos
        (if (and (zerop current-pos) (= (length swoosh-list) 1))
            (switch-to-buffer (other-buffer))
          (if (zerop current-pos)
              (switch-to-buffer (car (last swoosh-list)))
            (switch-to-buffer (nth (1- current-pos) swoosh-list))))
      (switch-to-buffer (car swoosh-list)))))

;;;###autoload
(defun swoosh-next-buffer ()
  "Switch to next buffer in 'swoosh-list'.
Wraps around when reaching the end of the list."
  (interactive)
  (let ((current-pos (cl-position (current-buffer) swoosh-list)))
    (if current-pos
        (if (and (eq current-pos (1- (length swoosh-list))) (= (length swoosh-list) 1))
            (switch-to-buffer (other-buffer))
          (if (eq current-pos (1- (length swoosh-list)))
              (switch-to-buffer (car swoosh-list))
            (switch-to-buffer (nth (1+ current-pos) swoosh-list))))
      (switch-to-buffer (car swoosh-list)))))

;;;###autoload
(defun swoosh-list-buffers ()
  "Display an editable view of 'swoosh-list'."
  (interactive)
  (let ((swoosh-buffer (get-buffer-create "*Swoosh*")))
    (with-current-buffer swoosh-buffer
      (swoosh--update-list)
      (swoosh-mode))
    (pop-to-buffer swoosh-buffer)))

(defun swoosh--update-list ()
  (interactive)
  (let ((current-point (point))
        (current-buffer (current-buffer))
        (inhibit-read-only t))
    (with-current-buffer "*Swoosh*"
      (erase-buffer)
      (dolist (buf swoosh-list)
               (let ((name (buffer-name buf))
                     (file-name (or (buffer-file-name buf) ""))
                     (mode (format-mode-line mode-name buf)))
                 (insert (format "%-40s %-20s %s\n" name mode file-name))))
      (goto-char current-point))))

(defun swoosh--switch-to-buffer-at-pt ()
  "Switch to the buffer at point in '*Swoosh*'"
  (interactive)
  (switch-to-buffer (swoosh--buffer-at-pt)))

(defun swoosh--delete-buffer-at-pt ()
  "Remove the buffer at point in '*Swoosh*' from swoosh-list"
  (interactive)
  (progn
    (setq swoosh-list (delete (swoosh--buffer-at-pt) swoosh-list))
    (swoosh--update-list)))

(defun swoosh--move-buffer (direction)
  "Move the buffer at point based on 'direction'."
  (let* ((current-index (cl-position (swoosh--buffer-at-pt) swoosh-list))
        (next-index (+ current-index direction)))
    (when (and (>= next-index 0) (< next-index (length swoosh-list)))
     (cl-rotatef (nth current-index swoosh-list)
                  (nth next-index swoosh-list)))))

(defun swoosh--move-buffer-down ()
  "Move the buffer at point down one step."
  (interactive)
  (progn
    (swoosh--move-buffer 1)
    (next-line)
    (swoosh--update-list)))

(defun swoosh--move-buffer-up ()
  "Move the buffer at point up one step."
  (interactive)
  (progn
    (swoosh--move-buffer -1)
    (previous-line)
    (swoosh--update-list)))

(defun swoosh--remove-current-buffer ()
  "Remove current buffer from swoosh-list."
  (when (member (current-buffer) swoosh-list)
    (setq swoosh-list (delete (current-buffer) swoosh-list))))

(add-hook 'kill-buffer-hook #'swoosh--remove-current-buffer)

(defun swoosh-add-buffer ()
  "Select buffer to add from open buffers."
  (interactive)
  (let* ((all-buffers (cl-remove-if (lambda (buf) (string-prefix-p " " buf)) (mapcar 'buffer-name (buffer-list))))
         (selected-buffers (completing-read-multiple "Select buffers to add to swoosh-list: " all-buffers nil t)))
    (mapc (lambda (buf) (if (not( member (get-buffer buf) swoosh-list))
                            (add-to-list 'swoosh-list (get-buffer buf))))
          selected-buffers)))

(defun swoosh--buffer-at-pt ()
  (let* ((current-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (buffer-name (car (split-string current-line))))
    (get-buffer buffer-name)))

(defvar swoosh-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "P") 'swoosh--move-buffer-up)
    (define-key map (kbd "N") 'swoosh--move-buffer-down)
    (define-key map (kbd "g") 'swoosh--update-list)
    (define-key map (kbd "d") 'swoosh--delete-buffer-at-pt)
    (define-key map (kbd "a") (lambda () (interactive)
                                (progn (swoosh-add-buffer)
                                       (swoosh--update-list))))
    (define-key map (kbd "RET") 'swoosh--switch-to-buffer-at-pt)
    map)
  "Keymap for swoosh-mode.")

(define-derived-mode swoosh-mode special-mode "Swoosh"
  "Major mode for browsing and managing swoosh-list of buffers.")


(provide 'swoosh)
