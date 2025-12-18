;;; emacs-alacritty.el --- Alacritty terminal emulator in Emacs -*- lexical-binding: t -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'emacs-alacritty "target/debug/libemacs_alacritty.so")

(defvar-local emacs-alacritty-term nil)

(defun emacs-alacritty-mode ()
  "Major mode for Alacritty terminal emulator."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'emacs-alacritty-mode)
  (setq mode-name "Alacritty")
  (setq emacs-alacritty-term (emacs-alacritty-create-term (window-width) (window-height)))
  (use-local-map (make-sparse-keymap))
  (add-hook 'post-command-hook #'emacs-alacritty-update nil t))

(defun emacs-alacritty-update ()
  (when emacs-alacritty-term
    (let ((inhibit-read-only t)
          (content (emacs-alacritty-get-contents emacs-alacritty-term))
          (row (emacs-alacritty-cursor-row emacs-alacritty-term))
          (col (emacs-alacritty-cursor-col emacs-alacritty-term)))
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (forward-line row)
      (forward-char col))))

(defun emacs-alacritty-send-string (str)
  (interactive "sSend string: ")
  (when emacs-alacritty-term
    (emacs-alacritty-feed emacs-alacritty-term str)
    (emacs-alacritty-update)))

(provide 'emacs-alacritty)
