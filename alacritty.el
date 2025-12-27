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
(require 'term)

;; Load the dynamic module
(defvar alacritty-module-path nil
  "Path to the alacritty dynamic module.")

(defvar alacritty-module-loaded nil
  "Whether the alacritty module has been loaded.")

;; Declare functions defined by the dynamic module to silence byte-compiler
(declare-function alacritty--module-create "alacritty")
(declare-function alacritty--module-process-bytes "alacritty")
(declare-function alacritty--module-resize "alacritty")
(declare-function alacritty--module-take-events "alacritty")
(declare-function alacritty--module-is-dirty "alacritty")
(declare-function alacritty--module-reset-damage "alacritty")
(declare-function alacritty--module-get-damage "alacritty")
(declare-function alacritty--module-cursor-row "alacritty")
(declare-function alacritty--module-cursor-col "alacritty")
(declare-function alacritty--module-get-title "alacritty")
(declare-function alacritty--module-alt-screen-mode "alacritty")
(declare-function alacritty--module-app-cursor-mode "alacritty")
(declare-function alacritty--module-bracketed-paste-mode "alacritty")
(declare-function alacritty--module-history-size "alacritty")
(declare-function alacritty--module-redraw "alacritty")
(declare-function alacritty--module-redraw-with-damage "alacritty")
(declare-function alacritty--module-get-prompt-positions "alacritty")
(declare-function alacritty--module-clear-prompt-positions "alacritty")

;; Customization - must be defined before functions that use them

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

;; Module loading functions

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
      (defalias 'alacritty--module-reset-damage 'alacritty-emacs-alacritty--module-reset-damage)
      (defalias 'alacritty--module-get-damage 'alacritty-emacs-alacritty--module-get-damage)
      (defalias 'alacritty--module-cursor-row 'alacritty-emacs-alacritty--module-cursor-row)
      (defalias 'alacritty--module-cursor-col 'alacritty-emacs-alacritty--module-cursor-col)
      (defalias 'alacritty--module-get-title 'alacritty-emacs-alacritty--module-get-title)
      (defalias 'alacritty--module-alt-screen-mode 'alacritty-emacs-alacritty--module-alt-screen-mode)
      (defalias 'alacritty--module-app-cursor-mode 'alacritty-emacs-alacritty--module-app-cursor-mode)
      (defalias 'alacritty--module-bracketed-paste-mode 'alacritty-emacs-alacritty--module-bracketed-paste-mode)
      (defalias 'alacritty--module-history-size 'alacritty-emacs-alacritty--module-history-size)
      (defalias 'alacritty--module-redraw 'alacritty-emacs-alacritty--module-redraw)
      (defalias 'alacritty--module-redraw-with-damage 'alacritty-emacs-alacritty--module-redraw-with-damage)
      (defalias 'alacritty--module-get-prompt-positions 'alacritty-emacs-alacritty--module-get-prompt-positions)
      (defalias 'alacritty--module-clear-prompt-positions 'alacritty-emacs-alacritty--module-clear-prompt-positions)
      (setq alacritty-module-loaded t))))

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

(defcustom alacritty-timer-delay 0.1
  "Delay for coalescing redraws (in seconds).
A larger delay improves performance when receiving bursts of data.
Set to 0.1 to match vterm's default (good for large output bursts).
Set to 0.033 for smoother visual updates (~30 FPS).
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

(defcustom alacritty-eval-cmds
  '(("find-file" find-file)
    ("message" message)
    ("alacritty-clear-scrollback" alacritty-clear-scrollback))
  "Whitelisted Emacs functions that can be executed from the terminal.

You can execute Emacs functions directly from the terminal using shell
integration scripts.  For example, `alacritty_cmd message \"Hello\"' will
display \"Hello\" in the minibuffer.

This is a list of (NAME-IN-SHELL EMACS-FUNCTION) pairs.  The shell sends
commands via OSC 51;E escape sequences, and only functions in this whitelist
will be executed for security reasons."
  :type '(alist :key-type string :value-type (list function))
  :group 'alacritty)

(defcustom alacritty-use-prompt-detection-method t
  "When non-nil, use shell-side prompt detection.

Prompt detection requires shell-side configuration: the shell must emit
the OSC 51;A escape sequence at the end of each prompt.  This allows
features like prompt navigation and smart beginning-of-line.

When nil, fall back to `term-prompt-regexp' for prompt detection."
  :type 'boolean
  :group 'alacritty)

(defcustom alacritty-copy-exclude-prompt t
  "When non-nil, exclude the prompt when copying the current line."
  :type 'boolean
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

(defvar-local alacritty--prompt-tracking-enabled-p nil
  "Non-nil if prompt tracking has been detected in this buffer.
This is set to t when the first OSC 51;A sequence is received from the shell.")

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
    (define-key map (kbd "C-c C-r") #'alacritty-redraw)
    (define-key map (kbd "C-c C-q") #'alacritty-send-next-key)
    ;; Prompt navigation
    (define-key map (kbd "C-c C-n") #'alacritty-next-prompt)
    (define-key map (kbd "C-c C-p") #'alacritty-previous-prompt)
    
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
    (define-key map (kbd "C-a") #'alacritty-beginning-of-line)
    (define-key map (kbd "C-e") #'alacritty-end-of-line)
    (define-key map (kbd "M-<") #'beginning-of-buffer)
    (define-key map (kbd "M->") #'end-of-buffer)
    (define-key map (kbd "<home>") #'beginning-of-buffer)
    (define-key map (kbd "<end>") #'end-of-buffer)
    (define-key map (kbd "C-s") #'isearch-forward)
    (define-key map (kbd "C-r") #'isearch-backward)
    (define-key map (kbd "C-SPC") #'set-mark-command)
    ;; Prompt navigation in copy mode
    (define-key map (kbd "C-c C-n") #'alacritty-next-prompt)
    (define-key map (kbd "C-c C-p") #'alacritty-previous-prompt)
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
  "Perform the actual rendering of terminal content to the buffer.
Uses damage tracking to only redraw changed portions when possible."
  (let ((inhibit-redisplay t)
        (inhibit-read-only t))
    (when alacritty--term
      ;; Handle any pending events first (title changes, bell, etc.)
      (let ((events (alacritty--module-take-events alacritty--term)))
        (when events
          (alacritty--handle-events events)))
      ;; Now do the redraw
      (let* ((result (alacritty--module-redraw-with-damage alacritty--term))
             (damage-type (nth 0 result))
             ;; history-size (nth 1) and alt-screen (nth 4) not used in this function
             (cursor-row (nth 2 result))
             (cursor-col (nth 3 result))
             (wrap-flags (nth 5 result)))
        ;; Only update if there was actual damage
        (unless (eq damage-type 'none)
          ;; Update fake newlines for copy mode
          (setq alacritty--fake-newlines (mapcar #'1+ wrap-flags))
          ;; Position cursor - we only render visible screen now, no history offset
          (goto-char (point-min))
          (forward-line cursor-row)
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
              (recenter (min cursor-row (1- win-height))))))))))

(defun alacritty--do-render-full ()
  "Perform a full redraw of terminal content (ignores damage tracking).
Use this when you need to force a complete refresh."
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
        ;; Reset damage state after full redraw
        (alacritty--module-reset-damage alacritty--term)
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
       (alacritty--send-string (cdr event)))
      ('prompt-end
       ;; OSC 51;A - prompt end with directory info (user@host:path)
       (setq alacritty--prompt-tracking-enabled-p t)
       (let ((data (cdr event)))
         (when-let ((dir-info (alacritty--parse-directory-info data)))
           (apply #'alacritty--set-directory dir-info))))
      ('eval-command
       ;; OSC 51;E - execute whitelisted Emacs command
       (alacritty--eval (cdr event))))))

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
          ;; Trigger redraw (events are handled in the redraw phase)
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
  (alacritty--parse-directory-info title))

(defun alacritty--parse-directory-info (info)
  "Parse INFO string for directory information.
INFO can be in format \"user@host:path\" or \"host:path\".
Returns (user host path) or nil if parsing fails."
  (when (and info (stringp info))
    (cond
     ((string-match "^\\([^@]+\\)@\\([^:]+\\):\\(.+\\)$" info)
      (list (match-string 1 info)
            (match-string 2 info)
            (match-string 3 info)))
     ((string-match "^\\([^:]+\\):\\(.+\\)$" info)
      (list nil
            (match-string 1 info)
            (match-string 2 info))))))

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
      ;; Use standard Tramp ssh method instead of deprecated /-: syntax
      (let ((tramp-path (if user
                            (format "/ssh:%s@%s:%s" user host expanded-path)
                          (format "/ssh:%s:%s" host expanded-path))))
        (file-name-as-directory tramp-path)))))

(defun alacritty--update-buffer-name ()
  "Update the buffer name based on the terminal title."
  (when (and alacritty-buffer-name-string alacritty--title)
    (rename-buffer (format alacritty-buffer-name-string alacritty--title) t)))

(defun alacritty--eval (str)
  "Execute a command from the terminal if it's in `alacritty-eval-cmds'.
STR is the command string, which may include quoted arguments."
  (let* ((parts (split-string-and-unquote str))
         (command (car parts))
         (args (cdr parts))
         (entry (assoc command alacritty-eval-cmds)))
    (if entry
        (apply (cadr entry) args)
      (message "alacritty: Command '%s' not in whitelist.  Add it to `alacritty-eval-cmds' to enable."
               command))))

(defun alacritty--setup-window-hooks ()
  "Set up hooks to handle window size changes."
  (add-hook 'window-size-change-functions #'alacritty--window-size-change nil t)
  (add-hook 'window-configuration-change-hook #'alacritty--window-config-change nil t))

(defun alacritty--window-size-change (frame)
  "Handle window size changes on FRAME."
  (when (and alacritty--term
             alacritty--process
             (process-live-p alacritty--process))
    ;; Check if this buffer is visible in any window on the affected frame
    (let ((buf (current-buffer)))
      (dolist (win (window-list frame 'no-minibuf))
        (when (eq (window-buffer win) buf)
          (let* ((width (window-body-width win))
                 (height (window-body-height win)))
            (alacritty--module-resize alacritty--term width height)
            ;; Also tell Emacs to resize the PTY
            (set-process-window-size alacritty--process height width)
            ;; Only process once even if buffer is in multiple windows
            (cl-return)))))))

(defun alacritty--window-config-change ()
  "Handle window configuration changes."
  (alacritty--window-size-change nil))

;; Public commands

(defun alacritty--create-terminal-buffer ()
  "Create and initialize a new terminal buffer.
Returns the buffer.  The caller should display the buffer before
calling this function so window dimensions are available.
Preserves `default-directory' from the calling buffer."
  (let ((dir default-directory)
        (buffer (generate-new-buffer "*alacritty*")))
    (with-current-buffer buffer
      (setq default-directory dir)
      (alacritty-mode))
    buffer))

(defcustom alacritty-tramp-shells
  '(("ssh" login-shell)
    ("scp" login-shell)
    ("docker" "/bin/sh"))
  "The shell to use for remote Tramp connections.

This is a list of (TRAMP-METHOD SHELL) pairs.  Use t as TRAMP-METHOD
to specify a default shell for all methods.  Specific methods take
precedence over t.

Set SHELL to `login-shell' to use the user's login shell on the
remote host (requires getent command on POSIX systems).

You can specify a fallback shell as a second element:
  ((\"ssh\" login-shell \"/bin/bash\") ...)"
  :type '(alist :key-type string :value-type (repeat sexp))
  :group 'alacritty)

(defun alacritty--tramp-get-shell (method)
  "Get the shell for a remote connection using Tramp METHOD.
Returns the shell command to use, or nil if not configured."
  (let* ((specs (cdr (assoc method alacritty-tramp-shells)))
         (first (car specs))
         (second (cadr specs)))
    (if (or (eq first 'login-shell)
            (and (consp first) (eq (cadr first) 'login-shell)))
        ;; Try to determine login shell via getent
        (let* ((entry (ignore-errors
                        (with-output-to-string
                          (with-current-buffer standard-output
                            (unless (= 0 (process-file-shell-command
                                          "getent passwd $LOGNAME"
                                          nil (current-buffer) nil))
                              (error "Unexpected return value"))
                            (when (> (count-lines (point-min) (point-max)) 1)
                              (error "Unexpected output"))))))
               (shell (when entry
                        (nth 6 (split-string entry ":" nil "[ \t\n\r]+")))))
          (or shell second))
      first)))

(defun alacritty--get-shell ()
  "Get the shell to use for the terminal.
For remote directories, uses `alacritty-tramp-shells' configuration.
For local directories, uses `alacritty-shell'."
  (if (ignore-errors (file-remote-p default-directory))
      (with-parsed-tramp-file-name default-directory nil
        (or (alacritty--tramp-get-shell method)
            (alacritty--tramp-get-shell t)
            (with-connection-local-variables shell-file-name)
            alacritty-shell))
    alacritty-shell))

(defun alacritty--start-terminal-process ()
  "Start the terminal process in the current buffer.
Must be called after the buffer is displayed in a window.
If `default-directory' is a Tramp remote path, starts the shell
on the remote host using Tramp's file handlers."
  (let* ((size (alacritty--get-window-size))
         (shell (alacritty--get-shell))
         ;; Handle FreeBSD which doesn't support iutf8
         (iutf8-arg (if (eq system-type 'berkeley-unix) "" "iutf8")))
    ;; Create terminal state (no PTY - Emacs will handle that)
    (setq alacritty--term
          (alacritty--module-create (car size) (cdr size)
                                    alacritty-max-scrollback))
    ;; Create process with PTY
    ;; The :file-handler t option makes Emacs use Tramp's file handlers
    ;; when default-directory is a remote path, automatically starting
    ;; the process on the remote host
    (setq alacritty--process
          (make-process
           :name "alacritty"
           :buffer (current-buffer)
           :command `("/bin/sh" "-c"
                      ,(format "stty -nl sane %s erase ^? rows %d columns %d >/dev/null && exec %s"
                               iutf8-arg
                               (cdr size) (car size) shell))
           :connection-type 'pty
           :file-handler t
           :filter #'alacritty--filter
           :sentinel #'alacritty--sentinel))
    ;; Setup window hooks
    (alacritty--setup-window-hooks)))

;;;###autoload
(defun alacritty ()
  "Create a new terminal buffer."
  (interactive)
  (alacritty--load-module)
  (let ((buffer (alacritty--create-terminal-buffer)))
    ;; Switch to buffer first so we get correct window dimensions
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (alacritty--start-terminal-process))))

;;;###autoload
(defun alacritty-other-window ()
  "Create a new terminal buffer in another window."
  (interactive)
  (alacritty--load-module)
  (let ((buffer (alacritty--create-terminal-buffer)))
    (switch-to-buffer-other-window buffer)
    (with-current-buffer buffer
      (alacritty--start-terminal-process))))

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
        ;; Do a full redraw to include scrollback history
        (alacritty--do-render-full)
        (message "Copy mode enabled. Press 'q' to exit, RET to copy selection."))
    (use-local-map alacritty-mode-map)
    (kill-local-variable 'filter-buffer-substring-function)
    ;; Redraw without scrollback for normal mode
    (alacritty--do-render)
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

;; Prompt navigation commands

(defun alacritty--prompt-tracking-enabled-p ()
  "Return non-nil if prompt tracking is enabled and working.
Prompt tracking requires shell-side configuration (OSC 51;A sequences)."
  (or alacritty--prompt-tracking-enabled-p
      ;; Check if we have any prompt positions stored
      (and alacritty--term
           (not (null (alacritty--module-get-prompt-positions alacritty--term))))))

(defun alacritty--get-prompt-point ()
  "Get the buffer position of the end of the current prompt.
Returns nil if prompt tracking is not enabled or no prompt is found."
  (when (and alacritty-use-prompt-detection-method
             (alacritty--prompt-tracking-enabled-p)
             alacritty--term)
    (let* ((positions (alacritty--module-get-prompt-positions alacritty--term))
           (history-size (alacritty--module-history-size alacritty--term))
           (current-line (line-number-at-pos)))
      ;; Find the prompt that's on or before the current line
      (catch 'found
        (dolist (pos (reverse positions))
          (let* ((term-line (car pos))
                 ;; Convert terminal line to buffer line
                 ;; Terminal lines are relative to viewport, buffer lines are 1-indexed
                 (buffer-line (+ term-line history-size 1)))
            (when (<= buffer-line current-line)
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- buffer-line))
                (let ((col (cdr pos)))
                  (move-to-column col)
                  (throw 'found (point)))))))
        nil))))

(defun alacritty-next-prompt (n)
  "Move to the end of the Nth next prompt in the buffer.
With prefix argument N, move forward N prompts."
  (interactive "p")
  (if (and alacritty-use-prompt-detection-method
           (alacritty--prompt-tracking-enabled-p)
           alacritty--term)
      (let* ((positions (alacritty--module-get-prompt-positions alacritty--term))
             (history-size (alacritty--module-history-size alacritty--term))
             (current-line (line-number-at-pos))
             (found-count 0))
        (catch 'done
          (dolist (pos positions)
            (let* ((term-line (car pos))
                   (buffer-line (+ term-line history-size 1)))
              (when (> buffer-line current-line)
                (setq found-count (1+ found-count))
                (when (= found-count n)
                  (goto-char (point-min))
                  (forward-line (1- buffer-line))
                  (move-to-column (cdr pos))
                  (throw 'done t))))))
        (unless (= found-count n)
          (message "No more prompts")))
    ;; Fallback to term-prompt-regexp
    (term-next-prompt n)))

(defun alacritty-previous-prompt (n)
  "Move to the end of the Nth previous prompt in the buffer.
With prefix argument N, move backward N prompts."
  (interactive "p")
  (if (and alacritty-use-prompt-detection-method
           (alacritty--prompt-tracking-enabled-p)
           alacritty--term)
      (let* ((positions (alacritty--module-get-prompt-positions alacritty--term))
             (history-size (alacritty--module-history-size alacritty--term))
             (current-line (line-number-at-pos))
             (found-count 0))
        (catch 'done
          (dolist (pos (reverse positions))
            (let* ((term-line (car pos))
                   (buffer-line (+ term-line history-size 1)))
              (when (< buffer-line current-line)
                (setq found-count (1+ found-count))
                (when (= found-count n)
                  (goto-char (point-min))
                  (forward-line (1- buffer-line))
                  (move-to-column (cdr pos))
                  (throw 'done t))))))
        (unless (= found-count n)
          (message "No more prompts")))
    ;; Fallback to term-prompt-regexp
    (term-previous-prompt n)))

(defun alacritty--get-beginning-of-line (&optional pt)
  "Find the start of the line, bypassing line wraps.
If PT is specified, find its beginning of the line instead of the beginning
of the line at cursor."
  (save-excursion
    (when pt (goto-char pt))
    (beginning-of-line)
    ;; Skip over fake newlines (from line wrapping)
    (while (and (not (bobp))
                (memq (1- (line-number-at-pos)) alacritty--fake-newlines))
      (forward-char -1)
      (beginning-of-line))
    (point)))

(defun alacritty--get-end-of-line (&optional pt)
  "Find the end of the line, bypassing line wraps.
If PT is specified, find its end of the line instead of the end
of the line at cursor."
  (save-excursion
    (when pt (goto-char pt))
    (end-of-line)
    ;; Skip over fake newlines (from line wrapping)
    (while (memq (line-number-at-pos) alacritty--fake-newlines)
      (forward-char)
      (end-of-line))
    (point)))

(defun alacritty-beginning-of-line ()
  "Move point to the beginning of the line.
Move the point to the first character after the shell prompt on this line.
If the point is already there, move to the beginning of the line.
Effectively toggle between the two positions."
  (interactive "^")
  (let ((prompt-pt (alacritty--get-prompt-point))
        (line-start (alacritty--get-beginning-of-line)))
    (if (and prompt-pt
             (>= prompt-pt line-start)
             (<= prompt-pt (alacritty--get-end-of-line))
             (/= (point) prompt-pt))
        (goto-char prompt-pt)
      (goto-char line-start))))

(defun alacritty-end-of-line ()
  "Move point to the end of the line, bypassing line wraps."
  (interactive "^")
  (goto-char (alacritty--get-end-of-line)))

;; Utility commands

(defun alacritty-clear-scrollback ()
  "Clear the terminal scrollback buffer."
  (interactive)
  (when alacritty--term
    (alacritty--module-clear-prompt-positions alacritty--term))
  (alacritty--send-string "\e[3J\e[H\e[2J"))

(defun alacritty-reset ()
  "Reset the terminal."
  (interactive)
  (alacritty--send-string "\ec"))

(defun alacritty-send-string (string)
  "Send STRING to the terminal."
  (interactive "sSend string: ")
  (alacritty--send-string string))

(defun alacritty-redraw ()
  "Force a full redraw of the terminal.
This ignores damage tracking and redraws everything.
Useful if the display gets out of sync."
  (interactive)
  (alacritty--do-render-full))

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
  (alacritty--load-module)
  (let* ((thisdir (bookmark-prop-get bmk 'thisdir))
         (buf-name (bookmark-prop-get bmk 'buf-name))
         (buf (get-buffer buf-name))
         (thismode (and buf (with-current-buffer buf major-mode))))
    (when (or (not buf) (not (eq thismode 'alacritty-mode)))
      (setq buf (generate-new-buffer buf-name))
      (with-current-buffer buf
        (setq default-directory thisdir)
        (alacritty-mode))
      ;; Display the buffer first so window dimensions are available
      (set-buffer buf)
      (display-buffer buf)
      (with-current-buffer buf
        (alacritty--start-terminal-process)))
    (set-buffer buf)))

(provide 'alacritty)
;;; alacritty.el ends here
