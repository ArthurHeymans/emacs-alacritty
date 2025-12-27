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

## Comparison with vterm (emacs-libvterm)

This section compares emacs-alacritty with [vterm](https://github.com/akermu/emacs-libvterm), the most popular terminal emulator for Emacs.

### What alacritty.el does better

| Feature | alacritty.el | vterm |
|---------|-------------|-------|
| **Terminal library** | Uses alacritty_terminal (Rust, actively maintained) | Uses libvterm (C library) |
| **Build system** | Cargo (simple, cross-platform) | CMake with external dependencies |
| **Remote support** | Native TRAMP integration with `:file-handler t` | Requires manual shell configuration per method |
| **Architecture** | Emacs owns PTY, no polling needed | Module manages PTY with polling |
| **Damage tracking** | Built-in partial redraw support | Full redraws more common |

### What vterm does better (missing features)

| Feature | vterm | alacritty.el | Priority |
|---------|-------|--------------|----------|
| **Prompt tracking** | Yes - marks prompt regions with text properties | No - only directory tracking via title | High |
| **Prompt navigation** | `C-c C-n` / `C-c C-p` to jump between prompts | Not implemented | High |
| **OSC 52 clipboard** | Full support with security toggle | Basic clipboard via events | Medium |
| **vterm-eval-cmds whitelist** | Configurable command whitelist for security | Documented but not fully implemented | Medium |
| **Scrollback in normal mode** | Visible scrollback with prompt navigation | Only visible in copy mode | Medium |
| **vterm-send-next-key** | Sends any key including modifiers | Basic implementation | Low |
| **Cursor shape/style** | Supports block, bar, underline | Not exposed to Emacs | Low |
| **Multi-vterm support** | Mature ecosystem (multi-vterm, vterm-toggle) | Basic multi-buffer support | Low |
| **Color palette customization** | `vterm-color-palette` with 16 named colors | Basic face definitions | Low |
| **Keyboard exceptions** | `vterm-keymap-exceptions` list | Hardcoded C-c, C-x prefixes | Low |
| **Window size minimum** | `vterm-min-window-width` | Not implemented | Low |

### Feature parity checklist

- [x] Basic terminal emulation
- [x] True color (24-bit) support
- [x] Copy mode with text selection
- [x] Directory tracking via shell integration
- [x] Bookmark support
- [x] Shell integration scripts (bash, zsh, fish)
- [x] Bracketed paste mode
- [x] Alternate screen support
- [x] Application cursor mode
- [x] TRAMP remote support
- [x] Exit hooks
- [ ] Prompt tracking and navigation
- [ ] Scrollback visible in normal mode
- [ ] OSC 52 clipboard manipulation
- [ ] Eval command whitelist security
- [ ] Cursor shape control
- [ ] Configurable keyboard exceptions
- [ ] Color palette customization

## Improvement Plan

The following improvements are planned to reach feature parity with vterm. Items are ordered by priority.

### Phase 1: Core Functionality (High Priority)

1. **Prompt tracking and navigation**
   - Implement prompt detection using OSC 51;A escape sequence (already parsed in shell scripts)
   - Add `alacritty-prompt` text property to mark prompt regions
   - Implement `alacritty-next-prompt` and `alacritty-previous-prompt` commands
   - Add smart `alacritty-beginning-of-line` that respects prompt boundaries

2. **Scrollback in normal mode**
   - Currently scrollback history is only visible in copy mode
   - Modify rendering to show scrollback in normal mode (like vterm)
   - Keep cursor at correct position within visible area

3. **Eval command whitelist**
   - Implement `alacritty-eval-cmds` properly with security whitelist
   - Parse OSC 51;E sequences and dispatch to whitelisted functions
   - Document security implications

### Phase 2: Polish (Medium Priority)

4. **OSC 52 clipboard support**
   - Add `alacritty-enable-manipulate-selection-data-by-osc52` option
   - Implement clipboard load/store via OSC 52 sequences
   - Default to disabled for security

5. **Improved copy mode**
   - Add `alacritty-copy-exclude-prompt` option
   - Better integration with isearch

6. **Configurable keyboard exceptions**
   - Add `alacritty-keymap-exceptions` customization variable
   - Allow users to choose which key prefixes bypass the terminal

### Phase 3: Nice-to-Have (Low Priority)

7. **Cursor shape control**
   - Expose cursor style from terminal to Emacs
   - Support block, bar, and underline cursors

8. **Color palette customization**
   - Add `alacritty-color-palette` for easy theme integration
   - Inherit from term-color-* faces for consistency

9. **Window size constraints**
   - Add `alacritty-min-window-width` and `alacritty-min-window-height`
   - Prevent terminal from becoming too small

10. **Enhanced modifier key handling**
    - Improve `alacritty-send-next-key` to handle all modifier combinations
    - Better Meta key passthrough

## Contributing

Contributions are welcome! Please see the improvement plan above for areas that need work.

## License

GNU General Public License v3.0 or later. See [LICENSE](LICENSE) for details.
