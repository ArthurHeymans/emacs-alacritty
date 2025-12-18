;;; emacs-alacritty.el --- Alacritty terminal emulator in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: emacs-alacritty contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: terminals
;; URL: https://github.com/emacs-alacritty/emacs-alacritty

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs-alacritty is a terminal emulator for Emacs using the
;; alacritty_terminal library.  It provides a fully-featured terminal
;; experience similar to emacs-libvterm but using Alacritty's
;; terminal emulation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Load the dynamic module
(defvar emacs-alacritty-module-path nil
  "Path to the emacs-alacritty dynamic module.")

(defvar emacs-alacritty-module-loaded nil
  "Whether the emacs-alacritty module has been loaded.")

(defun emacs-alacritty--get-module-dir ()
  "Get the directory containing the emacs-alacritty module."
  (or (and load-file-name (file-name-directory load-file-name))
      (and buffer-file-name (file-name-directory buffer-file-name))
      (and (boundp 'emacs-alacritty-source-dir) emacs-alacritty-source-dir)
      default-directory))

(defun emacs-alacritty--find-module ()
  "Find the emacs-alacritty dynamic module."
  (let* ((dir (emacs-alacritty--get-module-dir))
         (release-path (expand-file-name "target/release/libemacs_alacritty.so" dir))
         (debug-path (expand-file-name "target/debug/libemacs_alacritty.so" dir)))
    (cond
     ((file-exists-p release-path) release-path)
     ((file-exists-p debug-path) debug-path)
     (t (error "emacs-alacritty module not found in %s. Please build it first with 'cargo build --release'" dir)))))

(defun emacs-alacritty--load-module ()
  "Load the emacs-alacritty dynamic module."
  (unless emacs-alacritty-module-loaded
    (let ((module-path (or emacs-alacritty-module-path
                           (emacs-alacritty--find-module))))
      (module-load module-path)
      (setq emacs-alacritty-module-loaded t))))

;; Customization

(defgroup emacs-alacritty nil
  "Alacritty terminal emulator for Emacs."
  :group 'terminals)

(defcustom emacs-alacritty-shell (or (getenv "SHELL") "/bin/sh")
  "Shell to run in the terminal."
  :type 'string
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-max-scrollback 10000
  "Maximum number of scrollback lines."
  :type 'integer
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-kill-buffer-on-exit t
  "Kill the buffer when the terminal process exits."
  :type 'boolean
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-timer-interval 0.05
  "Interval in seconds for the refresh timer."
  :type 'number
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-clear-scrollback-when-clearing nil
  "Clear scrollback when the screen is cleared."
  :type 'boolean
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-buffer-name-string "alacritty %s"
  "Format string for buffer names.  %s is replaced with the title."
  :type 'string
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-eval-cmds
  '(("find-file" find-file)
    ("message" message)
    ("vterm-clear-scrollback" emacs-alacritty-clear-scrollback))
  "List of commands that can be executed from the terminal.
Each entry is (NAME FUNCTION) where NAME is the command name
received from the terminal and FUNCTION is the Emacs function to call."
  :type '(repeat (list string function))
  :group 'emacs-alacritty)

;; Faces for terminal colors

(defface emacs-alacritty-color-black
  '((t :foreground "black" :background "black"))
  "Face for black color."
  :group 'emacs-alacritty)

(defface emacs-alacritty-color-red
  '((t :foreground "red3" :background "red3"))
  "Face for red color."
  :group 'emacs-alacritty)

(defface emacs-alacritty-color-green
  '((t :foreground "green3" :background "green3"))
  "Face for green color."
  :group 'emacs-alacritty)

(defface emacs-alacritty-color-yellow
  '((t :foreground "yellow3" :background "yellow3"))
  "Face for yellow color."
  :group 'emacs-alacritty)

(defface emacs-alacritty-color-blue
  '((t :foreground "blue2" :background "blue2"))
  "Face for blue color."
  :group 'emacs-alacritty)

(defface emacs-alacritty-color-magenta
  '((t :foreground "magenta3" :background "magenta3"))
  "Face for magenta color."
  :group 'emacs-alacritty)

(defface emacs-alacritty-color-cyan
  '((t :foreground "cyan3" :background "cyan3"))
  "Face for cyan color."
  :group 'emacs-alacritty)

(defface emacs-alacritty-color-white
  '((t :foreground "white" :background "white"))
  "Face for white color."
  :group 'emacs-alacritty)

;; Buffer-local variables

(defvar-local emacs-alacritty--term nil
  "The terminal instance for this buffer.")

(defvar-local emacs-alacritty--timer nil
  "Timer for refreshing the terminal display.")

(defvar-local emacs-alacritty--title ""
  "Current terminal title.")

(defvar-local emacs-alacritty--directory-from-title nil
  "Whether directory was set from title (for tracking purposes).")

(defvar-local emacs-alacritty--copy-mode nil
  "Whether copy mode is enabled.")

;; Mode map

(defvar emacs-alacritty-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Basic key handling - most keys are sent to terminal
    (define-key map [remap self-insert-command] #'emacs-alacritty--self-insert)
    
    ;; Special keys
    (define-key map (kbd "RET") #'emacs-alacritty-send-return)
    (define-key map (kbd "TAB") #'emacs-alacritty-send-tab)
    (define-key map (kbd "DEL") #'emacs-alacritty-send-backspace)
    (define-key map (kbd "<backspace>") #'emacs-alacritty-send-backspace)
    (define-key map (kbd "<delete>") #'emacs-alacritty-send-delete)
    (define-key map (kbd "<escape>") #'emacs-alacritty-send-escape)
    
    ;; Arrow keys
    (define-key map (kbd "<up>") #'emacs-alacritty-send-up)
    (define-key map (kbd "<down>") #'emacs-alacritty-send-down)
    (define-key map (kbd "<left>") #'emacs-alacritty-send-left)
    (define-key map (kbd "<right>") #'emacs-alacritty-send-right)
    
    ;; Navigation keys
    (define-key map (kbd "<home>") #'emacs-alacritty-send-home)
    (define-key map (kbd "<end>") #'emacs-alacritty-send-end)
    (define-key map (kbd "<prior>") #'emacs-alacritty-send-page-up)
    (define-key map (kbd "<next>") #'emacs-alacritty-send-page-down)
    (define-key map (kbd "<insert>") #'emacs-alacritty-send-insert)
    
    ;; Function keys
    (define-key map (kbd "<f1>") (lambda () (interactive) (emacs-alacritty--send-key "f1")))
    (define-key map (kbd "<f2>") (lambda () (interactive) (emacs-alacritty--send-key "f2")))
    (define-key map (kbd "<f3>") (lambda () (interactive) (emacs-alacritty--send-key "f3")))
    (define-key map (kbd "<f4>") (lambda () (interactive) (emacs-alacritty--send-key "f4")))
    (define-key map (kbd "<f5>") (lambda () (interactive) (emacs-alacritty--send-key "f5")))
    (define-key map (kbd "<f6>") (lambda () (interactive) (emacs-alacritty--send-key "f6")))
    (define-key map (kbd "<f7>") (lambda () (interactive) (emacs-alacritty--send-key "f7")))
    (define-key map (kbd "<f8>") (lambda () (interactive) (emacs-alacritty--send-key "f8")))
    (define-key map (kbd "<f9>") (lambda () (interactive) (emacs-alacritty--send-key "f9")))
    (define-key map (kbd "<f10>") (lambda () (interactive) (emacs-alacritty--send-key "f10")))
    (define-key map (kbd "<f11>") (lambda () (interactive) (emacs-alacritty--send-key "f11")))
    (define-key map (kbd "<f12>") (lambda () (interactive) (emacs-alacritty--send-key "f12")))
    
    ;; Control keys
    (define-key map (kbd "C-a") (lambda () (interactive) (emacs-alacritty--send-char ?a "C")))
    (define-key map (kbd "C-b") (lambda () (interactive) (emacs-alacritty--send-char ?b "C")))
    (define-key map (kbd "C-c C-c") (lambda () (interactive) (emacs-alacritty--send-char ?c "C")))
    (define-key map (kbd "C-d") (lambda () (interactive) (emacs-alacritty--send-char ?d "C")))
    (define-key map (kbd "C-e") (lambda () (interactive) (emacs-alacritty--send-char ?e "C")))
    (define-key map (kbd "C-f") (lambda () (interactive) (emacs-alacritty--send-char ?f "C")))
    (define-key map (kbd "C-g") (lambda () (interactive) (emacs-alacritty--send-char ?g "C")))
    (define-key map (kbd "C-k") (lambda () (interactive) (emacs-alacritty--send-char ?k "C")))
    (define-key map (kbd "C-l") (lambda () (interactive) (emacs-alacritty--send-char ?l "C")))
    (define-key map (kbd "C-n") (lambda () (interactive) (emacs-alacritty--send-char ?n "C")))
    (define-key map (kbd "C-p") (lambda () (interactive) (emacs-alacritty--send-char ?p "C")))
    (define-key map (kbd "C-r") (lambda () (interactive) (emacs-alacritty--send-char ?r "C")))
    (define-key map (kbd "C-t") (lambda () (interactive) (emacs-alacritty--send-char ?t "C")))
    (define-key map (kbd "C-u") (lambda () (interactive) (emacs-alacritty--send-char ?u "C")))
    (define-key map (kbd "C-w") (lambda () (interactive) (emacs-alacritty--send-char ?w "C")))
    (define-key map (kbd "C-y") #'emacs-alacritty-yank)
    (define-key map (kbd "C-z") (lambda () (interactive) (emacs-alacritty--send-char ?z "C")))
    
    ;; Emacs-specific commands
    (define-key map (kbd "C-c C-t") #'emacs-alacritty-copy-mode)
    (define-key map (kbd "C-c C-l") #'emacs-alacritty-clear-scrollback)
    (define-key map (kbd "C-c C-q") #'emacs-alacritty-send-next-key)
    
    map)
  "Keymap for `emacs-alacritty-mode'.")

;; Note: Meta key bindings (M-b, M-f, etc.) are not defined by default
;; because they can conflict with general.el and other packages that
;; manipulate keymaps. Users can add them manually if needed:
;;
;; (define-key emacs-alacritty-mode-map (kbd "M-b")
;;   (lambda () (interactive) (emacs-alacritty--send-char ?b "M")))
;;
;; Or use C-c C-q (send-next-key) to send any key directly to the terminal.

(defvar emacs-alacritty-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'emacs-alacritty-copy-mode)
    (define-key map (kbd "RET") #'emacs-alacritty-copy-mode-done)
    (define-key map (kbd "C-c C-t") #'emacs-alacritty-copy-mode)
    map)
  "Keymap for `emacs-alacritty-copy-mode'.")

;; Internal functions

(defun emacs-alacritty--send-key (key &optional modifiers)
  "Send KEY with optional MODIFIERS to the terminal."
  (when (and emacs-alacritty--term (not emacs-alacritty--copy-mode))
    (emacs-alacritty-send-key emacs-alacritty--term key modifiers)))

(defun emacs-alacritty--send-char (char &optional modifiers)
  "Send CHAR with optional MODIFIERS to the terminal."
  (when (and emacs-alacritty--term (not emacs-alacritty--copy-mode))
    (emacs-alacritty-send-char emacs-alacritty--term char modifiers)))

(defun emacs-alacritty--self-insert ()
  "Send the last input character to the terminal."
  (interactive)
  (when (and emacs-alacritty--term (not emacs-alacritty--copy-mode))
    (let ((char last-command-event))
      (when (characterp char)
        (emacs-alacritty-write-input emacs-alacritty--term (char-to-string char))))))

(defun emacs-alacritty--get-window-size ()
  "Get the current window size in columns and lines."
  (cons (window-body-width) (window-body-height)))

(defun emacs-alacritty--refresh ()
  "Refresh the terminal display."
  (when (and emacs-alacritty--term
             (buffer-live-p (current-buffer))
             (not emacs-alacritty--copy-mode))
    (condition-case err
        (let ((inhibit-read-only t))
          ;; Process any pending events
          (emacs-alacritty--process-events)
          
          ;; Check if terminal has exited
          (when (emacs-alacritty-is-exited emacs-alacritty--term)
            (emacs-alacritty--handle-exit)
            (cl-return-from emacs-alacritty--refresh))
          
          ;; Update display with styled content
          (let* ((styled-lines (emacs-alacritty-get-styled-content emacs-alacritty--term))
                 (cursor-row (emacs-alacritty-cursor-row emacs-alacritty--term))
                 (cursor-col (emacs-alacritty-cursor-col emacs-alacritty--term)))
            (erase-buffer)
            ;; Insert styled content
            (dolist (line styled-lines)
              (dolist (segment line)
                (emacs-alacritty--insert-styled-segment segment))
              (insert "\n"))
            ;; Position cursor
            (goto-char (point-min))
            (forward-line cursor-row)
            (let ((line-end (line-end-position)))
              (move-to-column cursor-col)
              (when (> (point) line-end)
                (goto-char line-end)))))
      (error
       (message "emacs-alacritty refresh error: %s" (error-message-string err))))))

(defcustom emacs-alacritty-default-bg "#000000"
  "Default terminal background color.
This color will not be applied, allowing Emacs default background to show through."
  :type 'string
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-default-fg "#e5e5e5"
  "Default terminal foreground color.
This color will not be applied, using Emacs default foreground instead."
  :type 'string
  :group 'emacs-alacritty)

(defun emacs-alacritty--insert-styled-segment (segment)
  "Insert a styled SEGMENT into the buffer.
SEGMENT is (text fg-color bg-color bold italic underline inverse)."
  (let* ((text (nth 0 segment))
         (fg (nth 1 segment))
         (bg (nth 2 segment))
         (bold (nth 3 segment))
         (italic (nth 4 segment))
         (underline (nth 5 segment))
         (inverse (nth 6 segment))
         (start (point))
         (face-attrs nil))
    ;; Handle inverse video
    (when inverse
      (let ((temp fg))
        (setq fg bg)
        (setq bg temp)))
    ;; Build face attributes - skip default colors to allow Emacs theme to show through
    (when (and fg (not (string-equal-ignore-case fg emacs-alacritty-default-fg)))
      (push :foreground face-attrs)
      (push fg face-attrs))
    (when (and bg (not (string-equal-ignore-case bg emacs-alacritty-default-bg)))
      (push :background face-attrs)
      (push bg face-attrs))
    (when bold
      (push :weight face-attrs)
      (push 'bold face-attrs))
    (when italic
      (push :slant face-attrs)
      (push 'italic face-attrs))
    (when underline
      (push :underline face-attrs)
      (push t face-attrs))
    ;; Insert text
    (insert text)
    ;; Apply face if we have attributes
    (when face-attrs
      (add-face-text-property start (point) (nreverse face-attrs)))))

(defun emacs-alacritty--parse-title-for-directory (title)
  "Parse TITLE for directory information.
Expected format: user@host:path or host:path.
Returns (user host path) or nil if not parseable."
  (when (and title (stringp title))
    (cond
     ;; Format: user@host:path
     ((string-match "^\\([^@]+\\)@\\([^:]+\\):\\(.+\\)$" title)
      (list (match-string 1 title)
            (match-string 2 title)
            (match-string 3 title)))
     ;; Format: host:path (no user)
     ((string-match "^\\([^:]+\\):\\(.+\\)$" title)
      (list nil
            (match-string 1 title)
            (match-string 2 title))))))

(defun emacs-alacritty--set-directory (user host path)
  "Set `default-directory' based on USER, HOST, and PATH.
Handles TRAMP for remote hosts."
  (when path
    (let ((dir (emacs-alacritty--get-directory-from-remote user host path)))
      (when (and dir (file-directory-p dir))
        (setq default-directory dir)))))

(defun emacs-alacritty--get-directory-from-remote (user host path)
  "Construct directory from USER, HOST, and PATH.
Returns a local path or TRAMP path as appropriate."
  (let ((local-host-p (or (null host)
                          (string= host "")
                          (string= host "localhost")
                          (string= host (system-name))
                          (string= host (car (split-string (system-name) "\\.")))))
        (expanded-path (if (string-prefix-p "~" path)
                           (expand-file-name path)
                         path)))
    (if local-host-p
        ;; Local path
        (file-name-as-directory expanded-path)
      ;; Remote path via TRAMP
      (let ((tramp-path (if user
                            (format "/%s@%s:%s" user host expanded-path)
                          (format "/%s:%s" host expanded-path))))
        (file-name-as-directory tramp-path)))))

(defun emacs-alacritty--process-events ()
  "Process pending terminal events."
  (when emacs-alacritty--term
    (let ((events (emacs-alacritty-poll-events emacs-alacritty--term)))
      (dolist (event events)
        (pcase (car event)
          ('title
           (let ((title (cdr event)))
             (setq emacs-alacritty--title title)
             (emacs-alacritty--update-buffer-name)
             ;; Parse title for directory information
             (when-let ((dir-info (emacs-alacritty--parse-title-for-directory title)))
               (apply #'emacs-alacritty--set-directory dir-info))))
          ('bell
           (ding))
          ('exit
           (emacs-alacritty--handle-exit))
          ('clipboard-store
           (kill-new (cdr event)))
          ('clipboard-load
           (when (and emacs-alacritty--term (current-kill 0 t))
             (emacs-alacritty-paste emacs-alacritty--term (current-kill 0)))))))))

(defun emacs-alacritty--handle-exit ()
  "Handle terminal process exit."
  (when emacs-alacritty--timer
    (cancel-timer emacs-alacritty--timer)
    (setq emacs-alacritty--timer nil))
  (if emacs-alacritty-kill-buffer-on-exit
      (kill-buffer (current-buffer))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n\nProcess exited.\n"))))

(defun emacs-alacritty--update-buffer-name ()
  "Update the buffer name based on the terminal title."
  (when (and emacs-alacritty-buffer-name-string emacs-alacritty--title)
    (rename-buffer (format emacs-alacritty-buffer-name-string emacs-alacritty--title) t)))

(defun emacs-alacritty--setup-window-hooks ()
  "Set up hooks to handle window size changes."
  (add-hook 'window-size-change-functions #'emacs-alacritty--window-size-change nil t)
  (add-hook 'window-configuration-change-hook #'emacs-alacritty--window-config-change nil t))

(defun emacs-alacritty--window-size-change (_frame)
  "Handle window size changes."
  (when (and emacs-alacritty--term
             (eq (current-buffer) (window-buffer)))
    (let ((size (emacs-alacritty--get-window-size)))
      (emacs-alacritty-resize emacs-alacritty--term (car size) (cdr size)))))

(defun emacs-alacritty--window-config-change ()
  "Handle window configuration changes."
  (emacs-alacritty--window-size-change nil))

;; Public commands

;;;###autoload
(defun emacs-alacritty ()
  "Create a new terminal buffer."
  (interactive)
  (emacs-alacritty--load-module)
  (let ((buffer (generate-new-buffer "*alacritty*")))
    (with-current-buffer buffer
      (emacs-alacritty-mode))
    ;; Switch to buffer first so we get correct window dimensions
    (switch-to-buffer buffer)
    (let ((size (emacs-alacritty--get-window-size)))
      (with-current-buffer buffer
        (setq emacs-alacritty--term
              (emacs-alacritty-create (car size) (cdr size) emacs-alacritty-shell))
        ;; Start refresh timer
        (setq emacs-alacritty--timer
              (run-with-timer 0.1 emacs-alacritty-timer-interval
                              (lambda ()
                                (when (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (emacs-alacritty--refresh))))))
        ;; Setup window hooks
        (emacs-alacritty--setup-window-hooks)))))

;;;###autoload
(defun emacs-alacritty-other-window ()
  "Create a new terminal buffer in another window."
  (interactive)
  (emacs-alacritty--load-module)
  (let ((buffer (generate-new-buffer "*alacritty*")))
    (with-current-buffer buffer
      (emacs-alacritty-mode))
    ;; Switch to other window first so we get correct window dimensions
    (switch-to-buffer-other-window buffer)
    (let ((size (emacs-alacritty--get-window-size)))
      (with-current-buffer buffer
        (setq emacs-alacritty--term
              (emacs-alacritty-create (car size) (cdr size) emacs-alacritty-shell))
        ;; Start refresh timer
        (setq emacs-alacritty--timer
              (run-with-timer 0.1 emacs-alacritty-timer-interval
                              (lambda ()
                                (when (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (emacs-alacritty--refresh))))))
        ;; Setup window hooks
        (emacs-alacritty--setup-window-hooks)))))

(define-derived-mode emacs-alacritty-mode fundamental-mode "Alacritty"
  "Major mode for Alacritty terminal emulator.

\\{emacs-alacritty-mode-map}"
  :group 'emacs-alacritty
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq buffer-undo-list t)  ; Disable undo
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 101)
  (setq-local hscroll-margin 0)
  (setq-local hscroll-step 1)
  (setq-local auto-hscroll-mode 'current-line)
  ;; Environment variables for shell
  (setenv "TERM" "xterm-256color")
  (setenv "COLORTERM" "truecolor")
  (setenv "INSIDE_EMACS" "alacritty"))

;; Key sending commands

(defun emacs-alacritty-send-return ()
  "Send return key to terminal."
  (interactive)
  (emacs-alacritty--send-key "return"))

(defun emacs-alacritty-send-tab ()
  "Send tab key to terminal."
  (interactive)
  (emacs-alacritty--send-key "tab"))

(defun emacs-alacritty-send-backspace ()
  "Send backspace key to terminal."
  (interactive)
  (emacs-alacritty--send-key "backspace"))

(defun emacs-alacritty-send-delete ()
  "Send delete key to terminal."
  (interactive)
  (emacs-alacritty--send-key "delete"))

(defun emacs-alacritty-send-escape ()
  "Send escape key to terminal."
  (interactive)
  (emacs-alacritty--send-key "escape"))

(defun emacs-alacritty-send-up ()
  "Send up arrow key to terminal."
  (interactive)
  (emacs-alacritty--send-key "up"))

(defun emacs-alacritty-send-down ()
  "Send down arrow key to terminal."
  (interactive)
  (emacs-alacritty--send-key "down"))

(defun emacs-alacritty-send-left ()
  "Send left arrow key to terminal."
  (interactive)
  (emacs-alacritty--send-key "left"))

(defun emacs-alacritty-send-right ()
  "Send right arrow key to terminal."
  (interactive)
  (emacs-alacritty--send-key "right"))

(defun emacs-alacritty-send-home ()
  "Send home key to terminal."
  (interactive)
  (emacs-alacritty--send-key "home"))

(defun emacs-alacritty-send-end ()
  "Send end key to terminal."
  (interactive)
  (emacs-alacritty--send-key "end"))

(defun emacs-alacritty-send-page-up ()
  "Send page up key to terminal."
  (interactive)
  (emacs-alacritty--send-key "page-up"))

(defun emacs-alacritty-send-page-down ()
  "Send page down key to terminal."
  (interactive)
  (emacs-alacritty--send-key "page-down"))

(defun emacs-alacritty-send-insert ()
  "Send insert key to terminal."
  (interactive)
  (emacs-alacritty--send-key "insert"))

(defun emacs-alacritty-yank ()
  "Paste from kill ring to terminal."
  (interactive)
  (when (and emacs-alacritty--term (current-kill 0 t))
    (emacs-alacritty-paste emacs-alacritty--term (current-kill 0))))

(defun emacs-alacritty-send-next-key ()
  "Read the next key and send it to the terminal."
  (interactive)
  (let ((key (read-key "Send key: ")))
    (when (characterp key)
      (emacs-alacritty-write-input emacs-alacritty--term (char-to-string key)))))

;; Copy mode

(defun emacs-alacritty-copy-mode ()
  "Toggle copy mode.
In copy mode, the terminal acts like a normal buffer for
selecting and copying text."
  (interactive)
  (setq emacs-alacritty--copy-mode (not emacs-alacritty--copy-mode))
  (if emacs-alacritty--copy-mode
      (progn
        (setq buffer-read-only t)
        (use-local-map emacs-alacritty-copy-mode-map)
        (message "Copy mode enabled. Press 'q' to exit, RET to copy selection."))
    (use-local-map emacs-alacritty-mode-map)
    (message "Copy mode disabled.")))

(defun emacs-alacritty-copy-mode-done ()
  "Exit copy mode, copying the selection if active."
  (interactive)
  (when (use-region-p)
    (kill-ring-save (region-beginning) (region-end)))
  (deactivate-mark)
  (emacs-alacritty-copy-mode))

;; Utility commands

(defun emacs-alacritty-clear-scrollback ()
  "Clear the terminal scrollback buffer."
  (interactive)
  (when emacs-alacritty--term
    (emacs-alacritty-write-input emacs-alacritty--term "\033[3J\033[H\033[2J")))

(defun emacs-alacritty-reset ()
  "Reset the terminal."
  (interactive)
  (when emacs-alacritty--term
    (emacs-alacritty-write-input emacs-alacritty--term "\033c")))

(defun emacs-alacritty-send-string (string)
  "Send STRING to the terminal."
  (interactive "sSend string: ")
  (when emacs-alacritty--term
    (emacs-alacritty-write-input emacs-alacritty--term string)))

(provide 'emacs-alacritty)
;;; emacs-alacritty.el ends here
