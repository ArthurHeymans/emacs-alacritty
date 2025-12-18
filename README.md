# alacritty.el

A terminal emulator for Emacs using the [alacritty_terminal](https://crates.io/crates/alacritty_terminal) library. Similar to [vterm](https://github.com/akermu/emacs-libvterm) but powered by Alacritty's terminal emulation.

## Features

- Full terminal emulation via alacritty_terminal
- True color (24-bit) support
- TRAMP integration for remote terminals (SSH, Docker, kubectl)
- Directory tracking via shell integration
- Copy mode for selecting and copying text
- Bookmark support
- Shell integration scripts (bash, zsh, fish)

## Requirements

- Emacs 25.1+ with dynamic module support
- Rust toolchain (for building)

## Installation

### Building from source

```bash
# Clone the repository
git clone https://github.com/alacritty/alacritty.el
cd alacritty.el

# Build the dynamic module
cargo build --release
```

### With Nix

```bash
nix build
# Or enter development shell
nix develop
```

### Emacs configuration

```elisp
;; Add to load-path
(add-to-list 'load-path "/path/to/alacritty.el")
(require 'alacritty)

;; Optional: set default shell
(setq alacritty-shell "/bin/zsh")
```

## Usage

### Basic commands

- `M-x alacritty` - Open a new terminal in current window
- `M-x alacritty-other-window` - Open terminal in another window

### Key bindings

| Key | Action |
|-----|--------|
| `C-c C-t` | Toggle copy mode |
| `C-c C-l` | Clear scrollback |
| `C-c C-q` | Send next key directly to terminal |
| `C-y` | Yank from kill ring |
| `M-y` | Yank pop |

Standard terminal keys (arrows, function keys, etc.) are passed through to the terminal.

### Copy mode

Press `C-c C-t` to enter copy mode. In copy mode:
- Navigate with standard Emacs movement commands
- Select text with the region
- Press `RET` to copy selection and exit
- Press `q` to exit without copying

Fake newlines (from line wrapping) are automatically removed when copying.

### Remote terminals (TRAMP)

When `default-directory` is a TRAMP path, alacritty automatically connects to the remote host:

```elisp
;; Open terminal on remote host
(let ((default-directory "/ssh:user@host:/home/user/"))
  (alacritty))
```

Configure remote shells via `alacritty-tramp-shells`:

```elisp
(setq alacritty-tramp-shells
      '(("ssh" login-shell)
        ("docker" "/bin/sh")))
```

### Shell integration

Source the appropriate script in your shell configuration for directory tracking and additional features:

**Bash** (`~/.bashrc`):
```bash
if [[ "$INSIDE_EMACS" = 'alacritty' ]]; then
    source /path/to/alacritty.el/etc/alacritty-bash.sh
fi
```

**Zsh** (`~/.zshrc`):
```zsh
if [[ "$INSIDE_EMACS" = 'alacritty' ]]; then
    source /path/to/alacritty.el/etc/alacritty-zsh.sh
fi
```

**Fish** (`~/.config/fish/config.fish`):
```fish
if test "$INSIDE_EMACS" = 'alacritty'
    source /path/to/alacritty.el/etc/alacritty.fish
end
```

## Configuration

### Customization options

```elisp
;; Shell to run (default: $SHELL or /bin/sh)
(setq alacritty-shell "/bin/zsh")

;; Kill buffer when terminal exits (default: t)
(setq alacritty-kill-buffer-on-exit t)

;; Refresh interval in seconds (default: 0.05)
(setq alacritty-timer-interval 0.05)

;; Buffer name format (default: "alacritty %s")
(setq alacritty-buffer-name-string "alacritty %s")

;; Remove fake newlines when copying (default: t)
(setq alacritty-copy-mode-remove-fake-newlines t)

;; Ignore cursor blink requests from applications (default: t)
(setq alacritty-ignore-blink-cursor t)
```

### Exit hook

Run custom code when terminals exit:

```elisp
(add-hook 'alacritty-exit-functions
          (lambda (buffer event)
            (message "Terminal %s exited: %s"
                     (if buffer (buffer-name buffer) "<killed>")
                     event)))
```

### Custom commands

Define Emacs commands callable from the terminal:

```elisp
(setq alacritty-eval-cmds
      '(("find-file" find-file)
        ("message" message)
        ("vterm-clear-scrollback" alacritty-clear-scrollback)))
```

Then call from shell: `alacritty_cmd find-file "/path/to/file"`

## License

GNU General Public License v3.0 or later. See [LICENSE](LICENSE) for details.
