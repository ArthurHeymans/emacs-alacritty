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
(require 'tramp)
(require 'bookmark)

;; Declare functions provided by the dynamic module
(declare-function emacs-alacritty-create "emacs-alacritty")
(declare-function emacs-alacritty-resize "emacs-alacritty")
(declare-function emacs-alacritty-write-input "emacs-alacritty")
(declare-function emacs-alacritty-get-text "emacs-alacritty")
(declare-function emacs-alacritty-get-styled-content "emacs-alacritty")
(declare-function emacs-alacritty-cursor-row "emacs-alacritty")
(declare-function emacs-alacritty-cursor-col "emacs-alacritty")
(declare-function emacs-alacritty-is-exited "emacs-alacritty")
(declare-function emacs-alacritty-get-title "emacs-alacritty")
(declare-function emacs-alacritty-poll-events "emacs-alacritty")
(declare-function emacs-alacritty-send-key "emacs-alacritty")
(declare-function emacs-alacritty-send-char "emacs-alacritty")
(declare-function emacs-alacritty-paste "emacs-alacritty")
(declare-function emacs-alacritty-line-wraps "emacs-alacritty")
(declare-function emacs-alacritty-cursor-blink "emacs-alacritty")
(declare-function emacs-alacritty-is-dirty "emacs-alacritty")
(declare-function emacs-alacritty-clear-dirty "emacs-alacritty")
(declare-function emacs-alacritty-get-full-styled-content "emacs-alacritty")
(declare-function emacs-alacritty-history-size "emacs-alacritty")

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
  "Find the emacs-alacritty dynamic module.
Returns the path to the module if found, or nil if not found."
  (let* ((dir (emacs-alacritty--get-module-dir))
         (release-path (expand-file-name "target/release/libemacs_alacritty.so" dir))
         (debug-path (expand-file-name "target/debug/libemacs_alacritty.so" dir)))
    (cond
     ((file-exists-p release-path) release-path)
     ((file-exists-p debug-path) debug-path)
     (t nil))))

(defun emacs-alacritty--load-module ()
  "Load the emacs-alacritty dynamic module.
If the module is not found, offers to compile it."
  (unless emacs-alacritty-module-loaded
    (let ((module-path (or emacs-alacritty-module-path
                           (emacs-alacritty--find-module))))
      (unless module-path
        ;; Module not found, offer to compile
        (if (or emacs-alacritty-always-compile-module
                (y-or-n-p "emacs-alacritty module not found.  Compile it now? "))
            (progn
              (emacs-alacritty-module-compile)
              (setq module-path (emacs-alacritty--find-module))
              (unless module-path
                (error "Compilation succeeded but module still not found")))
          (error "emacs-alacritty will not work until the module is compiled")))
      (module-load module-path)
      (setq emacs-alacritty-module-loaded t))))

;; Customization

(defgroup emacs-alacritty nil
  "Alacritty terminal emulator for Emacs."
  :group 'terminals)

;; Module compilation options

(defcustom emacs-alacritty-always-compile-module nil
  "If non-nil, automatically compile the module without prompting.
When nil, the user is prompted before compilation."
  :type 'boolean
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-compile-release t
  "If non-nil, compile in release mode (optimized).
If nil, compile in debug mode (faster compilation, slower runtime)."
  :type 'boolean
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-cargo-args ""
  "Additional arguments to pass to cargo when building the module.
Example: \"--features some-feature\""
  :type 'string
  :group 'emacs-alacritty)

(defvar emacs-alacritty-install-buffer-name " *Install emacs-alacritty* "
  "Name of the buffer used for compiling emacs-alacritty.")

(defun emacs-alacritty--cargo-is-available ()
  "Check if cargo is available in PATH."
  (executable-find "cargo"))

(defun emacs-alacritty-module-compile ()
  "Compile the emacs-alacritty module using cargo."
  (interactive)
  (unless (emacs-alacritty--cargo-is-available)
    (error "Cargo not found.  Please install Rust and Cargo first"))
  (let* ((emacs-alacritty-directory (emacs-alacritty--get-module-dir))
         (cargo-args (concat
                      (if emacs-alacritty-compile-release "--release " "")
                      emacs-alacritty-cargo-args))
         (make-commands
          (format "cd %s && cargo build %s"
                  (shell-quote-argument emacs-alacritty-directory)
                  cargo-args))
         (buffer (get-buffer-create emacs-alacritty-install-buffer-name)))
    (pop-to-buffer buffer)
    (compilation-mode)
    (if (zerop (let ((inhibit-read-only t))
                 (call-process "sh" nil buffer t "-c" make-commands)))
        (message "Compilation of `emacs-alacritty' module succeeded")
      (error "Compilation of `emacs-alacritty' module failed!"))))

(defcustom emacs-alacritty-shell (or (getenv "SHELL") "/bin/sh")
  "Shell to run in the terminal."
  :type 'string
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-tramp-shells
  '(("ssh" login-shell)
    ("scp" login-shell)
    ("docker" "/bin/sh"))
  "The shell that gets run in the terminal for tramp.

`emacs-alacritty-tramp-shells' has to be a list of pairs of the format:
\(TRAMP-METHOD SHELL)

Use t as TRAMP-METHOD to specify a default shell for all methods.
Specific methods always take precedence over t.

Set SHELL to \\='login-shell to use the user's login shell on the host.
The login-shell detection currently works for POSIX-compliant remote
hosts that have the getent command (regular GNU/Linux distros, *BSDs,
but not MacOS X unfortunately).

You can specify an additional second SHELL command as a fallback
that is used when the login-shell detection fails, e.g.,
\\='((\"ssh\" login-shell \"/bin/bash\") ...)
If no second SHELL command is specified with \\='login-shell, the terminal
will fall back to tramp's shell."
  :type '(alist :key-type string :value-type string)
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

(defcustom emacs-alacritty-copy-exclude-prompt nil
  "When non-nil, exclude the prompt from copied lines in copy mode.
This uses prompt tracking to identify prompt regions."
  :type 'boolean
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-copy-mode-remove-fake-newlines t
  "When non-nil, remove fake newlines when copying text in copy mode.
Fake newlines are inserted by the terminal when a line wraps due to
the terminal width. Removing them produces cleaner output."
  :type 'boolean
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

(defcustom emacs-alacritty-ignore-blink-cursor t
  "When t, ignore requests from applications to turn on/off cursor blink.

If nil, cursor in any window may begin to blink or not blink because
`blink-cursor-mode' is a global minor mode in Emacs.
You can use `M-x blink-cursor-mode' to toggle manually."
  :type 'boolean
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-bookmark-check-dir t
  "When non-nil, restore directory when restoring a bookmark."
  :type 'boolean
  :group 'emacs-alacritty)

(defcustom emacs-alacritty-exit-functions nil
  "List of functions called when the terminal process exits.
Each function is called with two arguments: the emacs-alacritty
buffer of the process (if still live), and a string describing
the exit event.

This can be used to perform cleanup, log exit events, or customize
behavior when terminals close.

Example:
  (add-hook \\='emacs-alacritty-exit-functions
            (lambda (buffer event)
              (message \"Terminal %s exited: %s\"
                       (if buffer (buffer-name buffer) \"<killed>\")
                       event)))"
  :type 'hook
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

(defvar-local emacs-alacritty--fake-newlines nil
  "List of line numbers (1-indexed) that have fake newlines.
These are lines that wrap due to terminal width rather than having
actual newline characters.")

(defvar-local emacs-alacritty--prompt-end nil
  "Buffer position of the end of the last prompt.
Used for excluding prompts when copying.")

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
    (define-key map (kbd "M-y") #'emacs-alacritty-yank-pop)
    (define-key map (kbd "C-z") (lambda () (interactive) (emacs-alacritty--send-char ?z "C")))
    (define-key map [mouse-2] #'emacs-alacritty-yank-primary)
    (define-key map [remap mouse-yank-primary] #'emacs-alacritty-yank-primary)
    
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
    ;; Navigation keys for scrolling through buffer
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
    ;; Search
    (define-key map (kbd "C-s") #'isearch-forward)
    (define-key map (kbd "C-r") #'isearch-backward)
    ;; Selection
    (define-key map (kbd "C-SPC") #'set-mark-command)
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
          ;; Process any pending events (this also updates the dirty flag)
          (emacs-alacritty--process-events)
          
          ;; Check if term is still valid (may have been cleared by exit event)
          (when emacs-alacritty--term
            ;; Check if terminal has exited (in case exit wasn't delivered as event)
            (if (emacs-alacritty-is-exited emacs-alacritty--term)
                (emacs-alacritty--handle-exit)
              ;; Only redraw if terminal content has changed
              (when (emacs-alacritty-is-dirty emacs-alacritty--term)
                ;; Clear dirty flag before redrawing
                (emacs-alacritty-clear-dirty emacs-alacritty--term)
                ;; Update display with full styled content (including scrollback)
                (let* ((styled-lines (emacs-alacritty-get-full-styled-content emacs-alacritty--term))
                       (history-size (emacs-alacritty-history-size emacs-alacritty--term))
                       (cursor-row (emacs-alacritty-cursor-row emacs-alacritty--term))
                       (cursor-col (emacs-alacritty-cursor-col emacs-alacritty--term))
                       (line-num 0)
                       (fake-newlines nil))
                  (erase-buffer)
                  ;; Insert styled content and track fake newlines
                  (dolist (line styled-lines)
                    (dolist (segment line)
                      (emacs-alacritty--insert-styled-segment segment))
                    ;; Check if this line wraps (has a fake newline)
                    ;; line-num is 0-indexed from start of buffer (including scrollback)
                    (when (emacs-alacritty-line-wraps emacs-alacritty--term line-num)
                      (push (1+ line-num) fake-newlines))  ; Store 1-indexed line number
                    (insert "\n")
                    (setq line-num (1+ line-num)))
                  ;; Store fake newlines for copy mode
                  (setq emacs-alacritty--fake-newlines (nreverse fake-newlines))
                  ;; Position cursor
                  ;; The terminal cursor position is relative to the visible screen (0-indexed).
                  ;; In our buffer, visible lines start at history-size (0-indexed).
                  ;; So buffer line = history-size + cursor-row
                  (goto-char (point-min))
                  (forward-line (+ history-size cursor-row))
                  (let ((line-end (line-end-position))
                        (target-col cursor-col)
                        (visual-col 0))
                    ;; Move forward character by character, tracking visual column
                    (while (and (< (point) line-end)
                                (< visual-col target-col))
                      (let* ((c (char-after))
                             (w (if c (char-width c) 1)))
                        (setq visual-col (+ visual-col w))
                        (forward-char 1))))
                  ;; Ensure cursor is visible - recenter if needed
                  (let ((win-height (window-body-height)))
                    (recenter (min cursor-row (1- win-height)))))))))
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
      ;; Remote path via TRAMP - use the default method (/-:)
      ;; This allows TRAMP to automatically determine the method
      (let ((tramp-path (if user
                            (format "/-:%s@%s:%s" user host expanded-path)
                          (format "/-:%s:%s" host expanded-path))))
        (file-name-as-directory tramp-path)))))

(defun emacs-alacritty--tramp-get-shell (method)
  "Get the shell for a remote location as specified in `emacs-alacritty-tramp-shells'.
The argument METHOD is the method string (as used by tramp) to get the shell
for, or t to get the default shell for all methods."
  (let* ((specs (cdr (assoc method emacs-alacritty-tramp-shells)))
         (first (car specs))
         (second (cadr specs)))
    ;; Allow '(... login-shell) or '(... 'login-shell).
    (if (or (eq first 'login-shell)
            (and (consp first) (eq (cadr first) 'login-shell)))
        ;; If the first element is 'login-shell, try to determine the user's
        ;; login shell on the remote host.  This should work for all
        ;; POSIX-compliant systems with the getent command in PATH.  This
        ;; includes regular GNU/Linux distros, *BSDs, but not MacOS X.  If
        ;; the login-shell determination fails at any point, the second
        ;; element in the shell spec is used (if present, otherwise nil is
        ;; returned).
        (let* ((entry (ignore-errors
                        (with-output-to-string
                          (with-current-buffer standard-output
                            ;; The getent command returns the passwd entry
                            ;; for the specified user independently of the
                            ;; used name service (i.e., not only for static
                            ;; passwd files, but also for LDAP, etc).
                            ;;
                            ;; Use a shell command here to get $LOGNAME.
                            ;; Using the tramp user does not always work as
                            ;; it can be nil, e.g., with ssh host configs.
                            ;; $LOGNAME is defined in all POSIX-compliant
                            ;; systems.
                            (unless (= 0 (process-file-shell-command
                                          "getent passwd $LOGNAME"
                                          nil (current-buffer) nil))
                              (error "Unexpected return value"))
                            ;; If we have more than one line, the output is
                            ;; not the expected single passwd entry.
                            ;; Most likely, $LOGNAME is not set.
                            (when (> (count-lines (point-min) (point-max)) 1)
                              (error "Unexpected output"))))))
               (shell (when entry
                        ;; The returned Unix passwd entry is a colon-
                        ;; separated line.  The 6th (last) element specifies
                        ;; the user's shell.
                        (nth 6 (split-string entry ":" nil "[ \t\n\r]+")))))
          (or shell second))
      first)))

(defun emacs-alacritty--get-shell ()
  "Get the shell that gets run in the terminal.
For remote directories (via TRAMP), uses `emacs-alacritty-tramp-shells'
to determine the appropriate shell."
  (if (ignore-errors (file-remote-p default-directory))
      (with-parsed-tramp-file-name default-directory nil
        (or (emacs-alacritty--tramp-get-shell method)
            (emacs-alacritty--tramp-get-shell t)
            (with-connection-local-variables shell-file-name)
            emacs-alacritty-shell))
    emacs-alacritty-shell))

(defun emacs-alacritty--quote-for-remote-shell (str)
  "Quote STR for use in a remote shell command.
Uses single quotes with proper escaping for embedded single quotes."
  (concat "'" (replace-regexp-in-string "'" "'\\\\''" str) "'"))

(defun emacs-alacritty--build-remote-command ()
  "Build a command string to connect to a remote host via TRAMP.
Returns a command that, when executed locally, will connect to the
remote host and start a shell in the appropriate directory.
Returns nil if `default-directory' is not a remote path."
  (when (ignore-errors (file-remote-p default-directory))
    (with-parsed-tramp-file-name default-directory nil
      (let ((shell (emacs-alacritty--get-shell))
            (remote-dir (or localname "~")))
        (pcase method
          ;; SSH-based methods
          ((or "ssh" "scp" "scpx" "sshx" "rsync")
           (let* ((port-option (if (and (boundp 'port) port)
                                   (format "-p %s " port)
                                 ""))
                  (host-arg (if user (format "%s@%s" user host) host))
                  ;; Build the remote command: cd to directory and exec shell
                  ;; The whole command needs to be quoted for SSH
                  (remote-cmd (format "cd %s && exec %s -l"
                                      (emacs-alacritty--quote-for-remote-shell remote-dir)
                                      shell)))
             ;; Quote the remote command so && is passed to remote shell
             (format "ssh %s-t %s %s"
                     port-option
                     host-arg
                     (emacs-alacritty--quote-for-remote-shell remote-cmd))))
          ;; Docker methods
          ((or "docker" "podman")
           (let ((container host)
                 (exec-cmd (if (string= method "podman") "podman" "docker"))
                 (remote-cmd (format "cd %s && exec %s"
                                     (emacs-alacritty--quote-for-remote-shell remote-dir)
                                     shell)))
             (format "%s exec -it %s sh -c %s"
                     exec-cmd
                     container
                     (emacs-alacritty--quote-for-remote-shell remote-cmd))))
          ;; Kubernetes/kubectl
          ("kubectl"
           (let* ((pod host)
                  (namespace (when (and (boundp 'hop) hop)
                               (tramp-file-name-host
                                (tramp-dissect-file-name hop))))
                  (remote-cmd (format "cd %s && exec %s"
                                      (emacs-alacritty--quote-for-remote-shell remote-dir)
                                      shell)))
             (format "kubectl exec -it %s%s -- sh -c %s"
                     (if namespace (format "-n %s " namespace) "")
                     pod
                     (emacs-alacritty--quote-for-remote-shell remote-cmd))))
          ;; Default: try SSH as fallback
          (_
           (let* ((host-arg (if user (format "%s@%s" user host) host))
                  (remote-cmd (format "cd %s && exec %s -l"
                                      (emacs-alacritty--quote-for-remote-shell remote-dir)
                                      shell)))
             (format "ssh -t %s %s"
                     host-arg
                     (emacs-alacritty--quote-for-remote-shell remote-cmd)))))))))

(defun emacs-alacritty--get-command ()
  "Get the command to run in the terminal.
For remote directories, returns a command to connect to the remote host.
For local directories, returns a command to cd to the directory and start the shell."
  (or (emacs-alacritty--build-remote-command)
      ;; Local directory - cd to it and exec the shell
      (let ((dir (expand-file-name default-directory)))
        (if (file-directory-p dir)
            (format "cd %s && exec %s"
                    (emacs-alacritty--quote-for-remote-shell dir)
                    emacs-alacritty-shell)
          emacs-alacritty-shell))))

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
             (emacs-alacritty-paste emacs-alacritty--term (current-kill 0))))
          ('cursor-blink-change
           (unless emacs-alacritty-ignore-blink-cursor
             (let ((blink (cdr event)))
               (blink-cursor-mode (if blink 1 -1))))))))))

(defun emacs-alacritty--handle-exit ()
  "Handle terminal process exit."
  ;; Cancel timer first to prevent further refresh attempts
  (when emacs-alacritty--timer
    (cancel-timer emacs-alacritty--timer)
    (setq emacs-alacritty--timer nil))
  ;; Clear the term pointer to prevent access to freed resources
  (let ((buf (current-buffer))
        (event "finished"))
    (setq emacs-alacritty--term nil)
    ;; Run exit hook before potentially killing the buffer
    (run-hook-with-args 'emacs-alacritty-exit-functions
                        (if (buffer-live-p buf) buf nil)
                        event)
    (if emacs-alacritty-kill-buffer-on-exit
        (kill-buffer buf)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n\nProcess exited.\n")))))

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
  "Create a new terminal buffer.
When `default-directory' is a TRAMP remote path, the terminal will
connect to the remote host via SSH (or appropriate method) and start
a shell there using `emacs-alacritty-tramp-shells'."
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
              (emacs-alacritty-create (car size) (cdr size)
                                      (emacs-alacritty--get-command)
                                      emacs-alacritty-max-scrollback))
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
  "Create a new terminal buffer in another window.
When `default-directory' is a TRAMP remote path, the terminal will
connect to the remote host via SSH (or appropriate method) and start
a shell there using `emacs-alacritty-tramp-shells'."
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
              (emacs-alacritty-create (car size) (cdr size)
                                      (emacs-alacritty--get-command)
                                      emacs-alacritty-max-scrollback))
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
  ;; Bookmark support
  (setq-local bookmark-make-record-function #'emacs-alacritty--bookmark-make-record)
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

(defun emacs-alacritty--insert-for-yank (text)
  "Send TEXT to the terminal instead of inserting in buffer.
This is used to override `insert-for-yank' during yank operations."
  (when (and emacs-alacritty--term text)
    (emacs-alacritty-paste emacs-alacritty--term text)))

(defun emacs-alacritty-yank (&optional arg)
  "Paste from kill ring to terminal.
ARG is passed to `yank'."
  (interactive "P")
  (deactivate-mark)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function 'insert-for-yank) #'emacs-alacritty--insert-for-yank))
      (yank arg))))

(defun emacs-alacritty-yank-pop (&optional arg)
  "Cycle through kill ring and paste to terminal.
ARG is passed to `yank-pop'."
  (interactive "p")
  (let ((inhibit-read-only t)
        (yank-undo-function (lambda (_start _end) nil)))  ; No-op undo
    (cl-letf (((symbol-function 'insert-for-yank) #'emacs-alacritty--insert-for-yank))
      (yank-pop arg))))

(defun emacs-alacritty-yank-primary ()
  "Paste the primary selection to the terminal."
  (interactive)
  (when emacs-alacritty--term
    (let ((primary (gui-get-primary-selection)))
      (when (and primary (not (string-empty-p primary)))
        (emacs-alacritty-paste emacs-alacritty--term primary)))))

(defun emacs-alacritty-send-next-key ()
  "Read the next key and send it to the terminal."
  (interactive)
  (let ((key (read-key "Send key: ")))
    (when (characterp key)
      (emacs-alacritty-write-input emacs-alacritty--term (char-to-string key)))))

;; Copy mode

(defun emacs-alacritty--remove-fake-newlines (text start-line)
  "Remove fake newlines from TEXT.
START-LINE is the 1-indexed line number where the text starts.
Fake newlines are identified by checking `emacs-alacritty--fake-newlines'."
  (if (or (not emacs-alacritty-copy-mode-remove-fake-newlines)
          (null emacs-alacritty--fake-newlines))
      text
    (let ((lines (split-string text "\n"))
          (result nil)
          (current-line start-line))
      (dolist (line lines)
        (if (memq current-line emacs-alacritty--fake-newlines)
            ;; This line has a fake newline - don't add newline after it
            (setq result (concat result line))
          ;; Real newline
          (setq result (if result
                           (concat result "\n" line)
                         line)))
        (setq current-line (1+ current-line)))
      result)))

(defun emacs-alacritty--get-prompt-end ()
  "Get the buffer position of the end of the prompt on the current line.
Returns nil if no prompt is detected."
  ;; The prompt end is typically marked by shell integration.
  ;; For now, we use a heuristic: look for common prompt patterns.
  ;; Users can customize `emacs-alacritty--prompt-end' if needed.
  emacs-alacritty--prompt-end)

(defun emacs-alacritty--filter-buffer-substring (beg end &optional _delete)
  "Filter text between BEG and END, removing fake newlines.
This is used as `filter-buffer-substring-function' in copy mode."
  (let* ((text (buffer-substring beg end))
         (start-line (line-number-at-pos beg)))
    (emacs-alacritty--remove-fake-newlines text start-line)))

(defun emacs-alacritty-copy-mode ()
  "Toggle copy mode.
In copy mode, the terminal acts like a normal buffer for
selecting and copying text.  Fake newlines (from line wrapping)
are automatically removed when copying."
  (interactive)
  (setq emacs-alacritty--copy-mode (not emacs-alacritty--copy-mode))
  (if emacs-alacritty--copy-mode
      (progn
        (setq buffer-read-only t)
        (use-local-map emacs-alacritty-copy-mode-map)
        ;; Set up filter for removing fake newlines
        (setq-local filter-buffer-substring-function
                    #'emacs-alacritty--filter-buffer-substring)
        (message "Copy mode enabled. Press 'q' to exit, RET to copy selection."))
    (use-local-map emacs-alacritty-mode-map)
    ;; Restore default filter
    (kill-local-variable 'filter-buffer-substring-function)
    (message "Copy mode disabled.")))

(defun emacs-alacritty-copy-mode-done ()
  "Exit copy mode, copying the selection if active.
Fake newlines are automatically removed from the copied text."
  (interactive)
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (start-line (line-number-at-pos beg))
           (text (buffer-substring-no-properties beg end))
           (filtered-text (emacs-alacritty--remove-fake-newlines text start-line)))
      ;; Optionally exclude prompt
      (when (and emacs-alacritty-copy-exclude-prompt
                 emacs-alacritty--prompt-end
                 (>= emacs-alacritty--prompt-end beg)
                 (<= emacs-alacritty--prompt-end end))
        ;; Adjust text to exclude prompt portion
        (let ((prompt-offset (- emacs-alacritty--prompt-end beg)))
          (when (> prompt-offset 0)
            (setq filtered-text (substring filtered-text prompt-offset)))))
      (kill-new filtered-text)))
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

;; Bookmark support

(defun emacs-alacritty--bookmark-make-record ()
  "Create a bookmark for the current terminal.
Records the current directory and buffer name."
  `(nil
    (handler . emacs-alacritty--bookmark-handler)
    (thisdir . ,default-directory)
    (buf-name . ,(buffer-name))
    (defaults . nil)))

;;;###autoload
(defun emacs-alacritty--bookmark-handler (bmk)
  "Handler to restore a terminal bookmark BMK.
If a terminal buffer of the same name does not exist, creates a new one.
Also checks the current directory and sets it to the bookmarked directory
if `emacs-alacritty-bookmark-check-dir' is non-nil."
  (let* ((thisdir (bookmark-prop-get bmk 'thisdir))
         (buf-name (bookmark-prop-get bmk 'buf-name))
         (buf (get-buffer buf-name))
         (thismode (and buf (with-current-buffer buf major-mode))))
    ;; Create if no such terminal buffer exists
    (when (or (not buf) (not (eq thismode 'emacs-alacritty-mode)))
      (setq buf (generate-new-buffer buf-name))
      (with-current-buffer buf
        (when emacs-alacritty-bookmark-check-dir
          (setq default-directory thisdir))
        (emacs-alacritty-mode)
        ;; Initialize the terminal
        (let ((size (emacs-alacritty--get-window-size)))
          (setq emacs-alacritty--term
                (emacs-alacritty-create (car size) (cdr size)
                                        (emacs-alacritty--get-command)
                                        emacs-alacritty-max-scrollback))
          (setq emacs-alacritty--timer
                (run-with-timer 0.1 emacs-alacritty-timer-interval
                                (let ((buffer buf))
                                  (lambda ()
                                    (when (buffer-live-p buffer)
                                      (with-current-buffer buffer
                                        (emacs-alacritty--refresh)))))))
          (emacs-alacritty--setup-window-hooks))))
    ;; Check the current directory
    (with-current-buffer buf
      (when (and emacs-alacritty-bookmark-check-dir
                 (not (string-equal default-directory thisdir)))
        (when emacs-alacritty--copy-mode
          (emacs-alacritty-copy-mode))
        (emacs-alacritty-write-input emacs-alacritty--term (concat "cd " thisdir))
        (emacs-alacritty--send-key "return")))
    ;; Set to this terminal buffer
    (set-buffer buf)))

(provide 'emacs-alacritty)
;;; emacs-alacritty.el ends here
