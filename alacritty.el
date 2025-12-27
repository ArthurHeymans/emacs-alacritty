;;; alacritty.el --- Alacritty terminal emulator in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: alacritty contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: terminals
;; URL: https://github.com/alacritty/alacritty.el

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

;; Alacritty is a terminal emulator for Emacs using the
;; alacritty_terminal library.  It provides a fully-featured terminal
;; experience similar to vterm but using Alacritty's terminal emulation.
;;
;; This version uses Emacs's make-process to create the PTY and feeds
;; data to the terminal via a process filter, eliminating polling.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tramp)
(require 'bookmark)

;; Load the dynamic module
(defvar alacritty-module-path nil
  "Path to the alacritty dynamic module.")

(defvar alacritty-module-loaded nil
  "Whether the alacritty module has been loaded.")

(defun alacritty--get-module-dir ()
  "Get the directory containing the alacritty source (with Cargo.toml)."
  (if alacritty-source-dir
      alacritty-source-dir
    (let ((dir (or (and load-file-name (file-name-directory load-file-name))
                   (and buffer-file-name (file-name-directory buffer-file-name))
                   (when-let ((lib (locate-library "alacritty.el" t)))
                     (file-name-directory lib))
                   default-directory)))
      (if (file-exists-p (expand-file-name "Cargo.toml" dir))
          dir
        (let ((repos-dir (replace-regexp-in-string
                          "/straight/build\\(-[^/]+\\)?/alacritty/?$"
                          "/straight/repos/alacritty/"
                          dir)))
          (if (file-exists-p (expand-file-name "Cargo.toml" repos-dir))
              repos-dir
            dir))))))

(defun alacritty--module-filename ()
  "Return the platform-specific module filename."
  (cond
   ((eq system-type 'darwin) "libalacritty_emacs.dylib")
   ((eq system-type 'windows-nt) "alacritty_emacs.dll")
   (t "libalacritty_emacs.so")))

(defun alacritty--find-module ()
  "Find the alacritty dynamic module."
  (let* ((module-name (alacritty--module-filename))
         (module-path (expand-file-name module-name alacritty-module-build-dir)))
    (when (file-exists-p module-path)
      module-path)))

(defun alacritty--load-module ()
  "Load the alacritty dynamic module."
  (unless alacritty-module-loaded
    (let ((module-path (or alacritty-module-path
                           (alacritty--find-module))))
      (unless module-path
        (if (or alacritty-always-compile-module
                (y-or-n-p "alacritty module not found.  Compile it now? "))
            (progn
              (alacritty-module-compile)
              (setq module-path (alacritty--find-module))
              (unless module-path
                (error "Compilation succeeded but module still not found")))
          (error "alacritty will not work until the module is compiled")))
      (module-load module-path)
      ;; Create aliases for module functions
      (defalias 'alacritty--module-create 'alacritty-emacs-alacritty--module-create)
      (defalias 'alacritty--module-process-bytes 'alacritty-emacs-alacritty--module-process-bytes)
      (defalias 'alacritty--module-resize 'alacritty-emacs-alacritty--module-resize)
      (defalias 'alacritty--module-take-events 'alacritty-emacs-alacritty--module-take-events)
      (defalias 'alacritty--module-is-dirty 'alacritty-emacs-alacritty--module-is-dirty)
      (defalias 'alacritty--module-clear-dirty 'alacritty-emacs-alacritty--module-clear-dirty)
      (defalias 'alacritty--module-cursor-row 'alacritty-emacs-alacritty--module-cursor-row)
      (defalias 'alacritty--module-cursor-col 'alacritty-emacs-alacritty--module-cursor-col)
      (defalias 'alacritty--module-get-title 'alacritty-emacs-alacritty--module-get-title)
      (defalias 'alacritty--module-alt-screen-mode 'alacritty-emacs-alacritty--module-alt-screen-mode)
      (defalias 'alacritty--module-app-cursor-mode 'alacritty-emacs-alacritty--module-app-cursor-mode)
      (defalias 'alacritty--module-bracketed-paste-mode 'alacritty-emacs-alacritty--module-bracketed-paste-mode)
      (defalias 'alacritty--module-history-size 'alacritty-emacs-alacritty--module-history-size)
      (defalias 'alacritty--module-redraw 'alacritty-emacs-alacritty--module-redraw)
      (setq alacritty-module-loaded t))))

;; Customization

(defgroup alacritty nil
  "Alacritty terminal emulator for Emacs."
  :group 'terminals)

(defcustom alacritty-source-dir nil
  "Directory containing the alacritty source code (with Cargo.toml)."
  :type '(choice (const :tag "Auto-detect" nil)
                 (directory :tag "Source directory"))
  :group 'alacritty)

(defcustom alacritty-module-build-dir
  (locate-user-emacs-file "alacritty")
  "Directory where the compiled module will be stored."
  :type 'directory
  :group 'alacritty)

(defcustom alacritty-always-compile-module nil
  "If non-nil, automatically compile the module without prompting."
  :type 'boolean
  :group 'alacritty)

(defcustom alacritty-compile-release t
  "If non-nil, compile in release mode (optimized)."
  :type 'boolean
  :group 'alacritty)

(defcustom alacritty-cargo-args ""
  "Additional arguments to pass to cargo when building the module."
  :type 'string
  :group 'alacritty)

(defvar alacritty-install-buffer-name " *Install alacritty* "
  "Name of the buffer used for compiling alacritty.")

(defun alacritty--cargo-is-available ()
  "Check if cargo is available in PATH."
  (executable-find "cargo"))

(defun alacritty-module-compile ()
  "Compile the alacritty module using cargo."
  (interactive)
  (unless (alacritty--cargo-is-available)
    (error "Cargo not found.  Please install Rust and Cargo first"))
  (let* ((source-dir (expand-file-name (alacritty--get-module-dir)))
         (build-dir (expand-file-name alacritty-module-build-dir))
         (temp-dir (make-temp-file "alacritty-build-" t))
         (module-name (alacritty--module-filename))
         (cargo-args (concat
                      (if alacritty-compile-release "--release " "")
                      alacritty-cargo-args))
         (target-subdir (if alacritty-compile-release "release" "debug"))
         (make-commands
          (format "cp %s %s %s && cp -r %s %s && cd %s && cargo build %s && mkdir -p %s && cp %s %s"
                  (shell-quote-argument (expand-file-name "Cargo.toml" source-dir))
                  (shell-quote-argument (expand-file-name "Cargo.lock" source-dir))
                  (shell-quote-argument temp-dir)
                  (shell-quote-argument (expand-file-name "src" source-dir))
                  (shell-quote-argument temp-dir)
                  (shell-quote-argument temp-dir)
                  cargo-args
                  (shell-quote-argument build-dir)
                  (shell-quote-argument (concat "target/" target-subdir "/" module-name))
                  (shell-quote-argument build-dir)))
         (buffer (get-buffer-create alacritty-install-buffer-name)))
    (unless (file-exists-p (expand-file-name "Cargo.toml" source-dir))
      (error "Cargo.toml not found in %s" source-dir))
    (pop-to-buffer buffer)
    (compilation-mode)
    (unwind-protect
        (if (zerop (let ((inhibit-read-only t))
                     (call-process "sh" nil buffer t "-c" make-commands)))
            (message "Compilation succeeded. Module installed to %s" build-dir)
          (error "Compilation failed!"))
      (delete-directory temp-dir t))))

(defcustom alacritty-shell (or (getenv "SHELL") "/bin/sh")
  "Shell to run in the terminal."
  :type 'string
  :group 'alacritty)

(defcustom alacritty-max-scrollback 10000
  "Maximum number of scrollback lines."
  :type 'integer
  :group 'alacritty)

(defcustom alacritty-kill-buffer-on-exit t
  "Kill the buffer when the terminal process exits."
  :type 'boolean
  :group 'alacritty)

(defcustom alacritty-timer-delay 0.033
  "Delay for coalescing redraws (in seconds).
A larger delay improves performance when receiving bursts of data.
If nil, redraw immediately."
  :type 'number
  :group 'alacritty)

(defcustom alacritty-buffer-name-string "alacritty %s"
  "Format string for buffer names.  %s is replaced with the title."
  :type 'string
  :group 'alacritty)

(defcustom alacritty-copy-mode-remove-fake-newlines t
  "When non-nil, remove fake newlines when copying text in copy mode."
  :type 'boolean
  :group 'alacritty)

(defcustom alacritty-ignore-blink-cursor t
  "When t, ignore requests to turn on/off cursor blink."
  :type 'boolean
  :group 'alacritty)

(defcustom alacritty-exit-functions nil
  "List of functions called when the terminal process exits."
  :type 'hook
  :group 'alacritty)

;; Faces for terminal colors

(defface alacritty-color-black
  '((t :foreground "black" :background "black"))
  "Face for black color."
  :group 'alacritty)

(defface alacritty-color-red
  '((t :foreground "red3" :background "red3"))
  "Face for red color."
  :group 'alacritty)

(defface alacritty-color-green
  '((t :foreground "green3" :background "green3"))
  "Face for green color."
  :group 'alacritty)

(defface alacritty-color-yellow
  '((t :foreground "yellow3" :background "yellow3"))
  "Face for yellow color."
  :group 'alacritty)

(defface alacritty-color-blue
  '((t :foreground "blue2" :background "blue2"))
  "Face for blue color."
  :group 'alacritty)

(defface alacritty-color-magenta
  '((t :foreground "magenta3" :background "magenta3"))
  "Face for magenta color."
  :group 'alacritty)

(defface alacritty-color-cyan
  '((t :foreground "cyan3" :background "cyan3"))
  "Face for cyan color."
  :group 'alacritty)

(defface alacritty-color-white
  '((t :foreground "white" :background "white"))
  "Face for white color."
  :group 'alacritty)

;; Buffer-local variables

(defvar-local alacritty--term nil
  "The terminal instance for this buffer.")

(defvar-local alacritty--process nil
  "Shell process of current terminal.")

(defvar-local alacritty--redraw-timer nil
  "Timer for deferred redrawing.")

(defvar-local alacritty--title ""
  "Current terminal title.")

(defvar-local alacritty--copy-mode nil
  "Whether copy mode is enabled.")

(defvar-local alacritty--fake-newlines nil
  "List of line numbers (1-indexed) that have fake newlines.")

;; Mode map

(defvar alacritty-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'alacritty--self-insert)
    
    ;; Special keys
    (define-key map (kbd "RET") #'alacritty-send-return)
    (define-key map (kbd "TAB") #'alacritty-send-tab)
    (define-key map (kbd "DEL") #'alacritty-send-backspace)
    (define-key map (kbd "<backspace>") #'alacritty-send-backspace)
    (define-key map (kbd "<delete>") #'alacritty-send-delete)
    (define-key map (kbd "<escape>") #'alacritty-send-escape)
    
    ;; Arrow keys
    (define-key map (kbd "<up>") #'alacritty-send-up)
    (define-key map (kbd "<down>") #'alacritty-send-down)
    (define-key map (kbd "<left>") #'alacritty-send-left)
    (define-key map (kbd "<right>") #'alacritty-send-right)
    
    ;; Navigation keys
    (define-key map (kbd "<home>") #'alacritty-send-home)
    (define-key map (kbd "<end>") #'alacritty-send-end)
    (define-key map (kbd "<prior>") #'alacritty-send-page-up)
    (define-key map (kbd "<next>") #'alacritty-send-page-down)
    (define-key map (kbd "<insert>") #'alacritty-send-insert)
    
    ;; Function keys
    (define-key map (kbd "<f1>") (lambda () (interactive) (alacritty--send-key "f1")))
    (define-key map (kbd "<f2>") (lambda () (interactive) (alacritty--send-key "f2")))
    (define-key map (kbd "<f3>") (lambda () (interactive) (alacritty--send-key "f3")))
    (define-key map (kbd "<f4>") (lambda () (interactive) (alacritty--send-key "f4")))
    (define-key map (kbd "<f5>") (lambda () (interactive) (alacritty--send-key "f5")))
    (define-key map (kbd "<f6>") (lambda () (interactive) (alacritty--send-key "f6")))
    (define-key map (kbd "<f7>") (lambda () (interactive) (alacritty--send-key "f7")))
    (define-key map (kbd "<f8>") (lambda () (interactive) (alacritty--send-key "f8")))
    (define-key map (kbd "<f9>") (lambda () (interactive) (alacritty--send-key "f9")))
    (define-key map (kbd "<f10>") (lambda () (interactive) (alacritty--send-key "f10")))
    (define-key map (kbd "<f11>") (lambda () (interactive) (alacritty--send-key "f11")))
    (define-key map (kbd "<f12>") (lambda () (interactive) (alacritty--send-key "f12")))
    
    ;; Control keys
    (define-key map (kbd "C-a") (lambda () (interactive) (alacritty--send-char ?a "C")))
    (define-key map (kbd "C-b") (lambda () (interactive) (alacritty--send-char ?b "C")))
    (define-key map (kbd "C-c C-c") (lambda () (interactive) (alacritty--send-char ?c "C")))
    (define-key map (kbd "C-d") (lambda () (interactive) (alacritty--send-char ?d "C")))
    (define-key map (kbd "C-e") (lambda () (interactive) (alacritty--send-char ?e "C")))
    (define-key map (kbd "C-f") (lambda () (interactive) (alacritty--send-char ?f "C")))
    (define-key map (kbd "C-g") (lambda () (interactive) (alacritty--send-char ?g "C")))
    (define-key map (kbd "C-k") (lambda () (interactive) (alacritty--send-char ?k "C")))
    (define-key map (kbd "C-l") (lambda () (interactive) (alacritty--send-char ?l "C")))
    (define-key map (kbd "C-n") (lambda () (interactive) (alacritty--send-char ?n "C")))
    (define-key map (kbd "C-p") (lambda () (interactive) (alacritty--send-char ?p "C")))
    (define-key map (kbd "C-r") (lambda () (interactive) (alacritty--send-char ?r "C")))
    (define-key map (kbd "C-t") (lambda () (interactive) (alacritty--send-char ?t "C")))
    (define-key map (kbd "C-u") (lambda () (interactive) (alacritty--send-char ?u "C")))
    (define-key map (kbd "C-w") (lambda () (interactive) (alacritty--send-char ?w "C")))
    (define-key map (kbd "C-y") #'alacritty-yank)
    (define-key map (kbd "M-y") #'alacritty-yank-pop)
    (define-key map (kbd "C-z") (lambda () (interactive) (alacritty--send-char ?z "C")))
    (define-key map [mouse-2] #'alacritty-yank-primary)
    (define-key map [remap mouse-yank-primary] #'alacritty-yank-primary)
    
    ;; Emacs-specific commands
    (define-key map (kbd "C-c C-t") #'alacritty-copy-mode)
    (define-key map (kbd "C-c C-l") #'alacritty-clear-scrollback)
    (define-key map (kbd "C-c C-q") #'alacritty-send-next-key)
    
    map)
  "Keymap for `alacritty-mode'.")

(defvar alacritty-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'alacritty-copy-mode)
    (define-key map (kbd "RET") #'alacritty-copy-mode-done)
    (define-key map (kbd "C-c C-t") #'alacritty-copy-mode)
    (define-key map (kbd "<prior>") #'scroll-down-command)
    (define-key map (kbd "<next>") #'scroll-up-command)
    (define-key map (kbd "C-v") #'scroll-up-command)
    (define-key map (kbd "M-v") #'scroll-down-command)
    (define-key map (kbd "<up>") #'previous-line)
    (define-key map (kbd "<down>") #'next-line)
    (define-key map (kbd "<left>") #'backward-char)
    (define-key map (kbd "<right>") #'forward-char)
    (define-key map (kbd "C-p") #'previous-line)
    (define-key map (kbd "C-n") #'next-line)
    (define-key map (kbd "C-b") #'backward-char)
    (define-key map (kbd "C-f") #'forward-char)
    (define-key map (kbd "C-a") #'beginning-of-line)
    (define-key map (kbd "C-e") #'end-of-line)
    (define-key map (kbd "M-<") #'beginning-of-buffer)
    (define-key map (kbd "M->") #'end-of-buffer)
    (define-key map (kbd "<home>") #'beginning-of-buffer)
    (define-key map (kbd "<end>") #'end-of-buffer)
    (define-key map (kbd "C-s") #'isearch-forward)
    (define-key map (kbd "C-r") #'isearch-backward)
    (define-key map (kbd "C-SPC") #'set-mark-command)
    map)
  "Keymap for `alacritty-copy-mode'.")

;; Internal functions

(defun alacritty--send-string (string)
  "Send STRING to the terminal process."
  (when (and alacritty--process (process-live-p alacritty--process))
    (process-send-string alacritty--process string)))

(defun alacritty--send-key (key &optional modifiers)
  "Send KEY with optional MODIFIERS to the terminal."
  (when (and alacritty--term (not alacritty--copy-mode))
    (let* ((app-cursor (alacritty--module-app-cursor-mode alacritty--term))
           (mods (or modifiers ""))
           (ctrl (string-match-p "C" mods))
           (alt (string-match-p "M" mods))
           (shift (string-match-p "S" mods))
           (seq (pcase key
                  ("up" (if app-cursor "\eOA" "\e[A"))
                  ("down" (if app-cursor "\eOB" "\e[B"))
                  ("right" (if app-cursor "\eOC" "\e[C"))
                  ("left" (if app-cursor "\eOD" "\e[D"))
                  ("home" "\e[H")
                  ("end" "\e[F")
                  ("page-up" "\e[5~")
                  ("page-down" "\e[6~")
                  ("tab" (if shift "\e[Z" "\t"))
                  ("backspace" (cond (ctrl "\b") (alt "\e\177") (t "\177")))
                  ("delete" "\e[3~")
                  ("insert" "\e[2~")
                  ((or "enter" "return") (if alt "\e\r" "\r"))
                  ("escape" "\e")
                  ("f1" "\eOP")
                  ("f2" "\eOQ")
                  ("f3" "\eOR")
                  ("f4" "\eOS")
                  ("f5" "\e[15~")
                  ("f6" "\e[17~")
                  ("f7" "\e[18~")
                  ("f8" "\e[19~")
                  ("f9" "\e[20~")
                  ("f10" "\e[21~")
                  ("f11" "\e[23~")
                  ("f12" "\e[24~")
                  (_ nil))))
      (when seq
        (alacritty--send-string seq)))))

(defun alacritty--send-char (char &optional modifiers)
  "Send CHAR with optional MODIFIERS to the terminal."
  (when (and alacritty--term (not alacritty--copy-mode))
    (let* ((mods (or modifiers ""))
           (ctrl (string-match-p "C" mods))
           (alt (string-match-p "M" mods))
           (data (concat
                  (when alt "\e")
                  (if (and ctrl (>= char ?a) (<= char ?z))
                      (string (- char ?a -1))  ; Ctrl+a = 0x01, etc.
                    (string char)))))
      (alacritty--send-string data))))

(defun alacritty--self-insert ()
  "Send the last input character to the terminal."
  (interactive)
  (when (and alacritty--term (not alacritty--copy-mode))
    (let ((char last-command-event))
      (when (characterp char)
        (alacritty--send-string (char-to-string char))))))

(defun alacritty--get-window-size ()
  "Get the current window size in columns and lines."
  (cons (window-body-width) (window-body-height)))

(defun alacritty--do-render ()
  "Perform the actual rendering of terminal content to the buffer."
  (let ((inhibit-redisplay t)
        (inhibit-read-only t))
    (when alacritty--term
      (let* ((result (alacritty--module-redraw alacritty--term))
             (history-size (nth 0 result))
             (cursor-row (nth 1 result))
             (cursor-col (nth 2 result))
             (alt-screen (nth 3 result))
             (wrap-flags (nth 4 result)))
        ;; Update fake newlines for copy mode
        (setq alacritty--fake-newlines (mapcar #'1+ wrap-flags))
        ;; Position cursor
        (goto-char (point-min))
        (if alt-screen
            (forward-line cursor-row)
          (forward-line (+ history-size cursor-row)))
        ;; Move to correct column, accounting for wide characters
        (let ((line-end (line-end-position))
              (target-col cursor-col)
              (visual-col 0))
          (while (and (< (point) line-end)
                      (< visual-col target-col))
            (let* ((c (char-after))
                   (w (if c (char-width c) 1)))
              (setq visual-col (+ visual-col w))
              (forward-char 1))))
        ;; Recenter if needed
        (when (eq (current-buffer) (window-buffer))
          (let ((win-height (window-body-height)))
            (recenter (min cursor-row (1- win-height)))))))))

(defun alacritty--invalidate ()
  "Mark the terminal as needing a redraw."
  (if alacritty-timer-delay
      (unless alacritty--redraw-timer
        (setq alacritty--redraw-timer
              (run-with-timer alacritty-timer-delay nil
                              #'alacritty--delayed-redraw (current-buffer))))
    (alacritty--do-render)))

(defun alacritty--delayed-redraw (buffer)
  "Redraw the terminal BUFFER after a delay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq alacritty--redraw-timer nil)
      (when alacritty--term
        (alacritty--do-render)))))

(defun alacritty--handle-events (events)
  "Handle terminal events.
EVENTS is a list of (type . data) pairs."
  (dolist (event events)
    (pcase (car event)
      ('title
       (let ((title (cdr event)))
         (setq alacritty--title title)
         (alacritty--update-buffer-name)
         (when-let ((dir-info (alacritty--parse-title-for-directory title)))
           (apply #'alacritty--set-directory dir-info))))
      ('bell
       (ding))
      ('clipboard-store
       (kill-new (cdr event)))
      ('clipboard-load
       (when (current-kill 0 t)
         (alacritty--send-string (current-kill 0))))
      ('cursor-blink-change
       (unless alacritty-ignore-blink-cursor
         (blink-cursor-mode (if (cdr event) 1 -1))))
      ('pty-write
       ;; Terminal wants to write back to PTY (e.g., DA1 response)
       (alacritty--send-string (cdr event))))))

(defun alacritty--filter (process input)
  "Process filter for terminal PROCESS.
Feeds INPUT to the terminal and triggers a redraw.
This is the key function - called by Emacs when PTY data arrives."
  (let ((inhibit-redisplay t)
        (inhibit-read-only t)
        (buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when alacritty--term
          ;; Feed data to terminal - this is where the magic happens!
          (alacritty--module-process-bytes alacritty--term input)
          ;; Handle any events generated
          (let ((events (alacritty--module-take-events alacritty--term)))
            (when events
              (alacritty--handle-events events)))
          ;; Trigger redraw
          (alacritty--invalidate))))))

(defun alacritty--sentinel (process event)
  "Sentinel for terminal PROCESS.
Handles the EVENT when the process exits."
  (let ((buf (process-buffer process)))
    (run-hook-with-args 'alacritty-exit-functions
                        (if (buffer-live-p buf) buf nil)
                        event)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when alacritty--redraw-timer
          (cancel-timer alacritty--redraw-timer)
          (setq alacritty--redraw-timer nil))
        (setq alacritty--term nil)
        (if alacritty-kill-buffer-on-exit
            (kill-buffer buf)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert "\n\nProcess exited.\n")))))))

(defun alacritty--parse-title-for-directory (title)
  "Parse TITLE for directory information."
  (when (and title (stringp title))
    (cond
     ((string-match "^\\([^@]+\\)@\\([^:]+\\):\\(.+\\)$" title)
      (list (match-string 1 title)
            (match-string 2 title)
            (match-string 3 title)))
     ((string-match "^\\([^:]+\\):\\(.+\\)$" title)
      (list nil
            (match-string 1 title)
            (match-string 2 title))))))

(defun alacritty--set-directory (user host path)
  "Set `default-directory' based on USER, HOST, and PATH."
  (when path
    (let ((dir (alacritty--get-directory-from-remote user host path)))
      (when (and dir (file-directory-p dir))
        (setq default-directory dir)))))

(defun alacritty--get-directory-from-remote (user host path)
  "Construct directory from USER, HOST, and PATH."
  (let ((local-host-p (or (null host)
                          (string= host "")
                          (string= host "localhost")
                          (string= host (system-name))
                          (string= host (car (split-string (system-name) "\\.")))))
        (expanded-path (if (string-prefix-p "~" path)
                           (expand-file-name path)
                         path)))
    (if local-host-p
        (file-name-as-directory expanded-path)
      (let ((tramp-path (if user
                            (format "/-:%s@%s:%s" user host expanded-path)
                          (format "/-:%s:%s" host expanded-path))))
        (file-name-as-directory tramp-path)))))

(defun alacritty--update-buffer-name ()
  "Update the buffer name based on the terminal title."
  (when (and alacritty-buffer-name-string alacritty--title)
    (rename-buffer (format alacritty-buffer-name-string alacritty--title) t)))

(defun alacritty--setup-window-hooks ()
  "Set up hooks to handle window size changes."
  (add-hook 'window-size-change-functions #'alacritty--window-size-change nil t)
  (add-hook 'window-configuration-change-hook #'alacritty--window-config-change nil t))

(defun alacritty--window-size-change (_frame)
  "Handle window size changes."
  (when (and alacritty--term
             alacritty--process
             (process-live-p alacritty--process)
             (eq (current-buffer) (window-buffer)))
    (let ((size (alacritty--get-window-size)))
      (alacritty--module-resize alacritty--term (car size) (cdr size))
      ;; Also tell Emacs to resize the PTY
      (set-process-window-size alacritty--process (cdr size) (car size)))))

(defun alacritty--window-config-change ()
  "Handle window configuration changes."
  (alacritty--window-size-change nil))

;; Public commands

;;;###autoload
(defun alacritty ()
  "Create a new terminal buffer."
  (interactive)
  (alacritty--load-module)
  (let ((buffer (generate-new-buffer "*alacritty*")))
    (with-current-buffer buffer
      (alacritty-mode))
    ;; Switch to buffer first so we get correct window dimensions
    (switch-to-buffer buffer)
    (let ((size (alacritty--get-window-size)))
      (with-current-buffer buffer
        ;; Create terminal state (no PTY - Emacs will handle that)
        (setq alacritty--term
              (alacritty--module-create (car size) (cdr size)
                                        alacritty-max-scrollback))
        ;; Create process with PTY - this is like vterm!
        (setq alacritty--process
              (make-process
               :name "alacritty"
               :buffer (current-buffer)
               :command `("/bin/sh" "-c"
                          ,(format "stty -nl sane iutf8 erase ^? rows %d columns %d >/dev/null && exec %s"
                                   (cdr size) (car size) alacritty-shell))
               :connection-type 'pty
               :filter #'alacritty--filter
               :sentinel #'alacritty--sentinel))
        ;; Setup window hooks
        (alacritty--setup-window-hooks)))))

;;;###autoload
(defun alacritty-other-window ()
  "Create a new terminal buffer in another window."
  (interactive)
  (alacritty--load-module)
  (let ((buffer (generate-new-buffer "*alacritty*")))
    (with-current-buffer buffer
      (alacritty-mode))
    (switch-to-buffer-other-window buffer)
    (let ((size (alacritty--get-window-size)))
      (with-current-buffer buffer
        (setq alacritty--term
              (alacritty--module-create (car size) (cdr size)
                                        alacritty-max-scrollback))
        (setq alacritty--process
              (make-process
               :name "alacritty"
               :buffer (current-buffer)
               :command `("/bin/sh" "-c"
                          ,(format "stty -nl sane iutf8 erase ^? rows %d columns %d >/dev/null && exec %s"
                                   (cdr size) (car size) alacritty-shell))
               :connection-type 'pty
               :filter #'alacritty--filter
               :sentinel #'alacritty--sentinel))
        (alacritty--setup-window-hooks)))))

(defun alacritty--cleanup ()
  "Clean up terminal resources when buffer is killed."
  (when alacritty--redraw-timer
    (cancel-timer alacritty--redraw-timer)
    (setq alacritty--redraw-timer nil))
  (when (and alacritty--process (process-live-p alacritty--process))
    (delete-process alacritty--process))
  (setq alacritty--term nil))

(define-derived-mode alacritty-mode fundamental-mode "Alacritty"
  "Major mode for Alacritty terminal emulator.

\\{alacritty-mode-map}"
  :group 'alacritty
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq buffer-undo-list t)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 101)
  (setq-local hscroll-margin 0)
  (setq-local hscroll-step 1)
  (setq-local auto-hscroll-mode 'current-line)
  (setq-local bookmark-make-record-function #'alacritty--bookmark-make-record)
  (add-hook 'kill-buffer-hook #'alacritty--cleanup nil t)
  (setenv "TERM" "xterm-256color")
  (setenv "COLORTERM" "truecolor")
  (setenv "INSIDE_EMACS" "alacritty"))

;; Key sending commands

(defun alacritty-send-return ()
  "Send return key to terminal."
  (interactive)
  (alacritty--send-key "return"))

(defun alacritty-send-tab ()
  "Send tab key to terminal."
  (interactive)
  (alacritty--send-key "tab"))

(defun alacritty-send-backspace ()
  "Send backspace key to terminal."
  (interactive)
  (alacritty--send-key "backspace"))

(defun alacritty-send-delete ()
  "Send delete key to terminal."
  (interactive)
  (alacritty--send-key "delete"))

(defun alacritty-send-escape ()
  "Send escape key to terminal."
  (interactive)
  (alacritty--send-key "escape"))

(defun alacritty-send-up ()
  "Send up arrow key to terminal."
  (interactive)
  (alacritty--send-key "up"))

(defun alacritty-send-down ()
  "Send down arrow key to terminal."
  (interactive)
  (alacritty--send-key "down"))

(defun alacritty-send-left ()
  "Send left arrow key to terminal."
  (interactive)
  (alacritty--send-key "left"))

(defun alacritty-send-right ()
  "Send right arrow key to terminal."
  (interactive)
  (alacritty--send-key "right"))

(defun alacritty-send-home ()
  "Send home key to terminal."
  (interactive)
  (alacritty--send-key "home"))

(defun alacritty-send-end ()
  "Send end key to terminal."
  (interactive)
  (alacritty--send-key "end"))

(defun alacritty-send-page-up ()
  "Send page up key to terminal."
  (interactive)
  (alacritty--send-key "page-up"))

(defun alacritty-send-page-down ()
  "Send page down key to terminal."
  (interactive)
  (alacritty--send-key "page-down"))

(defun alacritty-send-insert ()
  "Send insert key to terminal."
  (interactive)
  (alacritty--send-key "insert"))

(defun alacritty--insert-for-yank (text)
  "Send TEXT to the terminal instead of inserting in buffer."
  (when text
    (let ((bracketed (and alacritty--term
                          (alacritty--module-bracketed-paste-mode alacritty--term))))
      (when bracketed
        (alacritty--send-string "\e[200~"))
      (alacritty--send-string text)
      (when bracketed
        (alacritty--send-string "\e[201~")))))

(defun alacritty-yank (&optional arg)
  "Paste from kill ring to terminal.
ARG is passed to `yank'."
  (interactive "P")
  (deactivate-mark)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function 'insert-for-yank) #'alacritty--insert-for-yank))
      (yank arg))))

(defun alacritty-yank-pop (&optional arg)
  "Cycle through kill ring and paste to terminal.
ARG is passed to `yank-pop'."
  (interactive "p")
  (let ((inhibit-read-only t)
        (yank-undo-function (lambda (_start _end) nil)))
    (cl-letf (((symbol-function 'insert-for-yank) #'alacritty--insert-for-yank))
      (yank-pop arg))))

(defun alacritty-yank-primary ()
  "Paste the primary selection to the terminal."
  (interactive)
  (let ((primary (gui-get-primary-selection)))
    (when (and primary (not (string-empty-p primary)))
      (alacritty--insert-for-yank primary))))

(defun alacritty-send-next-key ()
  "Read the next key and send it to the terminal."
  (interactive)
  (let ((key (read-key "Send key: ")))
    (when (characterp key)
      (alacritty--send-string (char-to-string key)))))

;; Copy mode

(defun alacritty--remove-fake-newlines (text start-line)
  "Remove fake newlines from TEXT.
START-LINE is the 1-indexed line number where the text starts."
  (if (or (not alacritty-copy-mode-remove-fake-newlines)
          (null alacritty--fake-newlines))
      text
    (let ((lines (split-string text "\n"))
          (result nil)
          (current-line start-line))
      (dolist (line lines)
        (if (memq current-line alacritty--fake-newlines)
            (setq result (concat result line))
          (setq result (if result
                           (concat result "\n" line)
                         line)))
        (setq current-line (1+ current-line)))
      result)))

(defun alacritty--filter-buffer-substring (beg end &optional _delete)
  "Filter text between BEG and END, removing fake newlines."
  (let* ((text (buffer-substring beg end))
         (start-line (line-number-at-pos beg)))
    (alacritty--remove-fake-newlines text start-line)))

(defun alacritty-copy-mode ()
  "Toggle copy mode."
  (interactive)
  (setq alacritty--copy-mode (not alacritty--copy-mode))
  (if alacritty--copy-mode
      (progn
        (setq buffer-read-only t)
        (use-local-map alacritty-copy-mode-map)
        (setq-local filter-buffer-substring-function
                    #'alacritty--filter-buffer-substring)
        (message "Copy mode enabled. Press 'q' to exit, RET to copy selection."))
    (use-local-map alacritty-mode-map)
    (kill-local-variable 'filter-buffer-substring-function)
    (message "Copy mode disabled.")))

(defun alacritty-copy-mode-done ()
  "Exit copy mode, copying the selection if active."
  (interactive)
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (start-line (line-number-at-pos beg))
           (text (buffer-substring-no-properties beg end))
           (filtered-text (alacritty--remove-fake-newlines text start-line)))
      (kill-new filtered-text)))
  (deactivate-mark)
  (alacritty-copy-mode))

;; Utility commands

(defun alacritty-clear-scrollback ()
  "Clear the terminal scrollback buffer."
  (interactive)
  (alacritty--send-string "\e[3J\e[H\e[2J"))

(defun alacritty-reset ()
  "Reset the terminal."
  (interactive)
  (alacritty--send-string "\ec"))

(defun alacritty-send-string (string)
  "Send STRING to the terminal."
  (interactive "sSend string: ")
  (alacritty--send-string string))

;; Bookmark support

(defun alacritty--bookmark-make-record ()
  "Create a bookmark for the current terminal."
  `(nil
    (handler . alacritty--bookmark-handler)
    (thisdir . ,default-directory)
    (buf-name . ,(buffer-name))
    (defaults . nil)))

;;;###autoload
(defun alacritty--bookmark-handler (bmk)
  "Handler to restore a terminal bookmark BMK."
  (let* ((thisdir (bookmark-prop-get bmk 'thisdir))
         (buf-name (bookmark-prop-get bmk 'buf-name))
         (buf (get-buffer buf-name))
         (thismode (and buf (with-current-buffer buf major-mode))))
    (when (or (not buf) (not (eq thismode 'alacritty-mode)))
      (setq buf (generate-new-buffer buf-name))
      (with-current-buffer buf
        (setq default-directory thisdir)
        (alacritty-mode)
        (let ((size (alacritty--get-window-size)))
          (setq alacritty--term
                (alacritty--module-create (car size) (cdr size)
                                          alacritty-max-scrollback))
          (setq alacritty--process
                (make-process
                 :name "alacritty"
                 :buffer (current-buffer)
                 :command `("/bin/sh" "-c"
                            ,(format "stty -nl sane iutf8 erase ^? rows %d columns %d >/dev/null && exec %s"
                                     (cdr size) (car size) alacritty-shell))
                 :connection-type 'pty
                 :filter #'alacritty--filter
                 :sentinel #'alacritty--sentinel))
          (alacritty--setup-window-hooks))))
    (set-buffer buf)))

(provide 'alacritty)
;;; alacritty.el ends here
