//! Alacritty Terminal for Emacs - Terminal emulator for Emacs using alacritty_terminal
//!
//! This module provides a terminal emulator for Emacs using the alacritty_terminal
//! library. Unlike the previous implementation, this version does NOT create its own
//! PTY - instead, Emacs creates the PTY via make-process and feeds data to us via
//! a process filter. This eliminates polling and matches vterm's architecture.

use alacritty_terminal::event::{Event, EventListener};
use alacritty_terminal::grid::Dimensions;
use alacritty_terminal::index::{Column, Line};
use alacritty_terminal::term::cell::Flags as CellFlags;
use alacritty_terminal::term::{Config, Term, TermDamage, TermMode};
use alacritty_terminal::vte::ansi::{Color, CursorShape, NamedColor, Processor};
use emacs::{defun, Env, IntoLisp, Result, Value};
use parking_lot::Mutex;

use std::sync::Arc;

emacs::plugin_is_GPL_compatible!();

/// Terminal dimensions
#[derive(Clone, Copy)]
struct TermSize {
    cols: usize,
    lines: usize,
}

impl TermSize {
    fn new(cols: usize, lines: usize) -> Self {
        Self { cols, lines }
    }
}

impl Dimensions for TermSize {
    fn columns(&self) -> usize {
        self.cols
    }
    fn screen_lines(&self) -> usize {
        self.lines
    }
    fn total_lines(&self) -> usize {
        self.lines
    }
}

/// Position of a prompt in the terminal (for prompt navigation)
#[derive(Debug, Clone, Copy)]
pub struct PromptPosition {
    /// Line number in the terminal grid (can be negative for scrollback)
    pub line: i32,
    /// Column where the prompt ends (cursor position after prompt)
    pub column: usize,
}

/// Events collected during terminal processing
#[derive(Debug, Clone)]
pub enum TermEvent {
    Title(String),
    Bell,
    ClipboardStore(String),
    ClipboardLoad,
    CursorBlinkingChange,
    PtyWrite(String),
    /// OSC 51;A - Directory and prompt tracking: user@host:path
    /// This marks the end of a prompt and provides directory info
    PromptEnd(String),
    /// OSC 51;E - Eval command from shell
    EvalCommand(String),
}

/// Event listener that collects events to be returned to Emacs
#[derive(Clone, Default)]
struct EmacsEventListener {
    events: Arc<Mutex<Vec<TermEvent>>>,
}

impl EmacsEventListener {
    fn new() -> Self {
        Self {
            events: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn take_events(&self) -> Vec<TermEvent> {
        std::mem::take(&mut *self.events.lock())
    }
}

impl EventListener for EmacsEventListener {
    fn send_event(&self, event: Event) {
        let mut events = self.events.lock();
        match event {
            Event::Title(title) => events.push(TermEvent::Title(title)),
            Event::Bell => events.push(TermEvent::Bell),
            Event::ClipboardStore(_, data) => events.push(TermEvent::ClipboardStore(data)),
            Event::ClipboardLoad(_, _) => events.push(TermEvent::ClipboardLoad),
            Event::CursorBlinkingChange => events.push(TermEvent::CursorBlinkingChange),
            Event::PtyWrite(data) => events.push(TermEvent::PtyWrite(data)),
            _ => {}
        }
    }
}

/// The main terminal structure - no PTY, no event loop
/// Emacs owns the PTY and feeds us data via process filter
pub struct AlacrittyTerm {
    term: Arc<Mutex<Term<EmacsEventListener>>>,
    processor: Mutex<Processor>,
    event_listener: EmacsEventListener,
    size: Mutex<TermSize>,
    title: Mutex<String>,
    /// Track the current cursor position for prompt tracking
    /// This stores the (line, column) where the last prompt ended
    prompt_positions: Mutex<Vec<PromptPosition>>,
}

impl AlacrittyTerm {
    fn new(cols: usize, lines: usize, scrollback_lines: usize) -> Self {
        let size = TermSize::new(cols, lines);
        let event_listener = EmacsEventListener::new();

        let config = Config {
            scrolling_history: scrollback_lines,
            ..Config::default()
        };

        let term = Term::new(config, &size, event_listener.clone());

        Self {
            term: Arc::new(Mutex::new(term)),
            processor: Mutex::new(Processor::new()),
            event_listener,
            size: Mutex::new(size),
            title: Mutex::new(String::new()),
            prompt_positions: Mutex::new(Vec::new()),
        }
    }

    /// Process input bytes from the PTY (called from Emacs process filter)
    /// This is the key function - it feeds data to the terminal parser
    /// It also scans for vterm-style OSC 51 sequences before passing to the terminal.
    fn process_bytes(&self, bytes: &[u8]) {
        // Scan for OSC 51 sequences (vterm-style directory and command passing)
        // Format: ESC ] 51 ; <command> <data> ESC \
        // Where <command> is 'A' for directory/prompt, 'E' for eval
        let mut term = self.term.lock();
        let mut processor = self.processor.lock();

        let mut i = 0;
        let mut last_chunk_start = 0;

        while i < bytes.len() {
            // Look for ESC ] (OSC start)
            if bytes[i] == 0x1b && i + 1 < bytes.len() && bytes[i + 1] == b']' {
                // Check if this is OSC 51
                if i + 4 < bytes.len()
                    && bytes[i + 2] == b'5'
                    && bytes[i + 3] == b'1'
                    && bytes[i + 4] == b';'
                {
                    // Process any bytes before this OSC sequence
                    if last_chunk_start < i {
                        processor.advance(&mut *term, &bytes[last_chunk_start..i]);
                    }

                    // Find the end of the OSC sequence (ESC \ or BEL)
                    let osc_start = i + 5; // After "ESC ] 51 ;"
                    let mut osc_end = osc_start;
                    let mut found_end = false;

                    while osc_end < bytes.len() {
                        // Check for ESC \ (ST - String Terminator)
                        if bytes[osc_end] == 0x1b
                            && osc_end + 1 < bytes.len()
                            && bytes[osc_end + 1] == b'\\'
                        {
                            found_end = true;
                            break;
                        }
                        // Check for BEL (alternative terminator)
                        if bytes[osc_end] == 0x07 {
                            found_end = true;
                            break;
                        }
                        osc_end += 1;
                    }

                    if found_end && osc_start < osc_end {
                        let osc_content = &bytes[osc_start..osc_end];
                        if !osc_content.is_empty() {
                            let command = osc_content[0];
                            let data = if osc_content.len() > 1 {
                                String::from_utf8_lossy(&osc_content[1..]).to_string()
                            } else {
                                String::new()
                            };

                            match command {
                                b'A' => {
                                    // OSC 51;A - Prompt end / directory tracking
                                    // Record the current cursor position as a prompt position
                                    let cursor = term.grid().cursor.point;
                                    let prompt_pos = PromptPosition {
                                        line: cursor.line.0,
                                        column: cursor.column.0,
                                    };
                                    self.prompt_positions.lock().push(prompt_pos);
                                    self.event_listener
                                        .events
                                        .lock()
                                        .push(TermEvent::PromptEnd(data));
                                }
                                b'E' => {
                                    // OSC 51;E - Eval command
                                    self.event_listener
                                        .events
                                        .lock()
                                        .push(TermEvent::EvalCommand(data));
                                }
                                _ => {
                                    // Unknown command, pass through to terminal
                                    processor.advance(
                                        &mut *term,
                                        &bytes[i..osc_end
                                            + if bytes[osc_end] == 0x1b { 2 } else { 1 }],
                                    );
                                }
                            }
                        }

                        // Skip past the OSC sequence
                        i = if bytes[osc_end] == 0x1b {
                            osc_end + 2 // ESC \
                        } else {
                            osc_end + 1 // BEL
                        };
                        last_chunk_start = i;
                        continue;
                    }
                }
            }
            i += 1;
        }

        // Process remaining bytes
        if last_chunk_start < bytes.len() {
            processor.advance(&mut *term, &bytes[last_chunk_start..]);
        }
    }

    fn resize(&self, cols: usize, lines: usize) {
        let mut size = self.size.lock();
        size.cols = cols;
        size.lines = lines;
        self.term.lock().resize(*size);
    }

    /// Take collected events (title changes, bell, clipboard, pty writes)
    fn take_events(&self) -> Vec<TermEvent> {
        let events = self.event_listener.take_events();
        // Update title cache
        for event in &events {
            if let TermEvent::Title(title) = event {
                *self.title.lock() = title.clone();
            }
        }
        events
    }

    /// Check if terminal has any damage (needs redraw)
    /// Uses alacritty_terminal's built-in damage tracking
    fn is_dirty(&self) -> bool {
        let mut term = self.term.lock();
        match term.damage() {
            TermDamage::Full => true,
            TermDamage::Partial(iter) => iter.count() > 0,
        }
    }

    /// Reset damage state after redraw
    fn reset_damage(&self) {
        self.term.lock().reset_damage();
    }

    fn cursor_position(&self) -> (usize, usize) {
        let term = self.term.lock();
        let cursor = term.grid().cursor.point;
        let line = if cursor.line.0 < 0 {
            0
        } else {
            cursor.line.0 as usize
        };
        (line, cursor.column.0)
    }

    fn get_title(&self) -> String {
        self.title.lock().clone()
    }

    fn get_mode(&self) -> TermMode {
        *self.term.lock().mode()
    }

    /// Get all prompt positions
    fn get_prompt_positions(&self) -> Vec<PromptPosition> {
        self.prompt_positions.lock().clone()
    }

    /// Clear prompt positions (e.g., when scrollback is cleared)
    fn clear_prompt_positions(&self) {
        self.prompt_positions.lock().clear();
    }
}

/// Helper to convert Color to RGB values
fn color_to_rgb(color: &Color) -> (u8, u8, u8) {
    match color {
        Color::Spec(rgb) => (rgb.r, rgb.g, rgb.b),
        Color::Named(named) => match named {
            NamedColor::Black => (0, 0, 0),
            NamedColor::Red => (205, 0, 0),
            NamedColor::Green => (0, 205, 0),
            NamedColor::Yellow => (205, 205, 0),
            NamedColor::Blue => (0, 0, 238),
            NamedColor::Magenta => (205, 0, 205),
            NamedColor::Cyan => (0, 205, 205),
            NamedColor::White => (229, 229, 229),
            NamedColor::BrightBlack => (127, 127, 127),
            NamedColor::BrightRed => (255, 0, 0),
            NamedColor::BrightGreen => (0, 255, 0),
            NamedColor::BrightYellow => (255, 255, 0),
            NamedColor::BrightBlue => (92, 92, 255),
            NamedColor::BrightMagenta => (255, 0, 255),
            NamedColor::BrightCyan => (0, 255, 255),
            NamedColor::BrightWhite => (255, 255, 255),
            NamedColor::Foreground => (229, 229, 229),
            NamedColor::Background => (0, 0, 0),
            // Cursor and dim colors - use sensible defaults
            NamedColor::Cursor => (229, 229, 229),
            NamedColor::DimBlack => (0, 0, 0),
            NamedColor::DimRed => (127, 0, 0),
            NamedColor::DimGreen => (0, 127, 0),
            NamedColor::DimYellow => (127, 127, 0),
            NamedColor::DimBlue => (0, 0, 127),
            NamedColor::DimMagenta => (127, 0, 127),
            NamedColor::DimCyan => (0, 127, 127),
            NamedColor::DimWhite => (127, 127, 127),
            NamedColor::BrightForeground => (255, 255, 255),
            NamedColor::DimForeground => (127, 127, 127),
        },
        Color::Indexed(idx) => match idx {
            0 => (0, 0, 0),
            1 => (205, 0, 0),
            2 => (0, 205, 0),
            3 => (205, 205, 0),
            4 => (0, 0, 238),
            5 => (205, 0, 205),
            6 => (0, 205, 205),
            7 => (229, 229, 229),
            8 => (127, 127, 127),
            9 => (255, 0, 0),
            10 => (0, 255, 0),
            11 => (255, 255, 0),
            12 => (92, 92, 255),
            13 => (255, 0, 255),
            14 => (0, 255, 255),
            15 => (255, 255, 255),
            16..=231 => {
                let idx = idx - 16;
                let r = (idx / 36) * 51;
                let g = ((idx / 6) % 6) * 51;
                let b = (idx % 6) * 51;
                (r, g, b)
            }
            232..=255 => {
                let gray = (idx - 232) * 10 + 8;
                (gray, gray, gray)
            }
        },
    }
}

#[emacs::module]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

/// Create a new terminal with the given dimensions
/// NOTE: This no longer spawns a PTY - Emacs will do that with make-process
#[defun(user_ptr, name = "alacritty--module-create")]
fn create(cols: i64, lines: i64, scrollback: Option<i64>) -> Result<AlacrittyTerm> {
    let scrollback_lines = scrollback.unwrap_or(10000) as usize;
    Ok(AlacrittyTerm::new(
        cols as usize,
        lines as usize,
        scrollback_lines,
    ))
}

/// Process bytes from the PTY - called from Emacs process filter
/// This is the main entry point for terminal data
/// Note: Emacs sends process output as strings. For unibyte strings (raw bytes),
/// each character code represents a byte value. For multibyte strings,
/// we encode to UTF-8.
#[defun(name = "alacritty--module-process-bytes")]
fn process_bytes(term: &AlacrittyTerm, data: String) -> Result<()> {
    // Emacs process filters can send either unibyte or multibyte strings.
    // In both cases, converting to Rust String and then to bytes should work
    // because the emacs crate handles the encoding.
    term.process_bytes(data.as_bytes());
    Ok(())
}

/// Resize the terminal to new dimensions
#[defun(name = "alacritty--module-resize")]
fn resize(term: &AlacrittyTerm, cols: i64, lines: i64) -> Result<()> {
    term.resize(cols as usize, lines as usize);
    Ok(())
}

/// Take pending events and return them to Emacs
/// Returns list of (type . data) pairs
/// Types: title, bell, clipboard-store, clipboard-load, cursor-blink-change, pty-write
#[defun(name = "alacritty--module-take-events")]
fn take_events<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let events = term.take_events();
    let nil = env.intern("nil")?;
    let t = env.intern("t")?;

    let mut result = nil;
    for event in events.into_iter().rev() {
        let event_val = match event {
            TermEvent::Title(title) => {
                let sym = env.intern("title")?;
                let val = title.into_lisp(env)?;
                env.cons(sym, val)?
            }
            TermEvent::Bell => {
                let sym = env.intern("bell")?;
                env.cons(sym, t)?
            }
            TermEvent::ClipboardStore(data) => {
                let sym = env.intern("clipboard-store")?;
                let val = data.into_lisp(env)?;
                env.cons(sym, val)?
            }
            TermEvent::ClipboardLoad => {
                let sym = env.intern("clipboard-load")?;
                env.cons(sym, t)?
            }
            TermEvent::CursorBlinkingChange => {
                let sym = env.intern("cursor-blink-change")?;
                let blink = term.term.lock().cursor_style().blinking;
                env.cons(sym, if blink { t } else { nil })?
            }
            TermEvent::PtyWrite(data) => {
                // This is data that should be written back to the PTY
                // (e.g., terminal responses like DA1)
                let sym = env.intern("pty-write")?;
                let val = data.into_lisp(env)?;
                env.cons(sym, val)?
            }
            TermEvent::PromptEnd(info) => {
                // OSC 51;A - prompt end with directory info (user@host:path)
                let sym = env.intern("prompt-end")?;
                let val = info.into_lisp(env)?;
                env.cons(sym, val)?
            }
            TermEvent::EvalCommand(cmd) => {
                // OSC 51;E - eval command from shell
                let sym = env.intern("eval-command")?;
                let val = cmd.into_lisp(env)?;
                env.cons(sym, val)?
            }
        };
        result = env.cons(event_val, result)?;
    }

    Ok(result)
}

/// Check if terminal is dirty (needs redraw)
#[defun(name = "alacritty--module-is-dirty")]
fn is_dirty(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.is_dirty())
}

/// Reset damage state after redraw
#[defun(name = "alacritty--module-reset-damage")]
fn reset_damage(term: &AlacrittyTerm) -> Result<()> {
    term.reset_damage();
    Ok(())
}

/// Get terminal damage information
/// Returns: 'full if entire terminal needs redraw, or a list of (line left right) for damaged regions
/// Each element represents a line with damaged columns from left to right (inclusive)
#[defun(name = "alacritty--module-get-damage")]
fn get_damage<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let mut term_lock = term.term.lock();
    let nil = env.intern("nil")?;

    match term_lock.damage() {
        TermDamage::Full => env.intern("full"),
        TermDamage::Partial(iter) => {
            let mut result = nil;
            // Collect damage info in reverse order so we can build list with cons
            let damages: Vec<_> = iter.collect();
            for damage in damages.into_iter().rev() {
                let line_val = (damage.line as i64).into_lisp(env)?;
                let left_val = (damage.left as i64).into_lisp(env)?;
                let right_val = (damage.right as i64).into_lisp(env)?;
                // Build (line left right) list
                let entry = env.cons(right_val, nil)?;
                let entry = env.cons(left_val, entry)?;
                let entry = env.cons(line_val, entry)?;
                result = env.cons(entry, result)?;
            }
            Ok(result)
        }
    }
}

/// Get cursor row
#[defun(name = "alacritty--module-cursor-row")]
fn cursor_row(term: &AlacrittyTerm) -> Result<i64> {
    Ok(term.cursor_position().0 as i64)
}

/// Get cursor column
#[defun(name = "alacritty--module-cursor-col")]
fn cursor_col(term: &AlacrittyTerm) -> Result<i64> {
    Ok(term.cursor_position().1 as i64)
}

/// Get terminal title
#[defun(name = "alacritty--module-get-title")]
fn get_title(term: &AlacrittyTerm) -> Result<String> {
    Ok(term.get_title())
}

/// Check if in alternate screen mode
#[defun(name = "alacritty--module-alt-screen-mode")]
fn alt_screen_mode(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.get_mode().contains(TermMode::ALT_SCREEN))
}

/// Check if in application cursor mode
#[defun(name = "alacritty--module-app-cursor-mode")]
fn app_cursor_mode(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.get_mode().contains(TermMode::APP_CURSOR))
}

/// Check if bracketed paste mode is enabled
#[defun(name = "alacritty--module-bracketed-paste-mode")]
fn bracketed_paste_mode(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.get_mode().contains(TermMode::BRACKETED_PASTE))
}

/// Get cursor style information
/// Returns a cons cell: (SHAPE . BLINKING)
/// where SHAPE is one of: 'block, 'underline, 'beam, 'hollow-block, or 'hidden
/// and BLINKING is t or nil
#[defun(name = "alacritty--module-cursor-style")]
fn cursor_style<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let term_lock = term.term.lock();
    let style = term_lock.cursor_style();

    let shape_sym = match style.shape {
        CursorShape::Block => env.intern("block")?,
        CursorShape::Underline => env.intern("underline")?,
        CursorShape::Beam => env.intern("beam")?,
        CursorShape::HollowBlock => env.intern("hollow-block")?,
        CursorShape::Hidden => env.intern("hidden")?,
    };

    let blinking_val = if style.blinking {
        env.intern("t")?
    } else {
        env.intern("nil")?
    };

    env.cons(shape_sym, blinking_val)
}

/// Get prompt positions for prompt navigation
/// Returns a list of (line . column) pairs where prompts end
/// Line numbers are relative to viewport (can be negative for scrollback)
#[defun(name = "alacritty--module-get-prompt-positions")]
fn get_prompt_positions<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let positions = term.get_prompt_positions();
    let nil = env.intern("nil")?;

    let mut result = nil;
    // Build list in reverse order for cons efficiency
    for pos in positions.into_iter().rev() {
        let line_val = (pos.line as i64).into_lisp(env)?;
        let col_val = (pos.column as i64).into_lisp(env)?;
        let pair = env.cons(line_val, col_val)?;
        result = env.cons(pair, result)?;
    }

    Ok(result)
}

/// Clear prompt positions (call when scrollback is cleared)
#[defun(name = "alacritty--module-clear-prompt-positions")]
fn clear_prompt_positions(term: &AlacrittyTerm) -> Result<()> {
    term.clear_prompt_positions();
    Ok(())
}

/// Get history (scrollback) size
#[defun(name = "alacritty--module-history-size")]
fn history_size(term: &AlacrittyTerm) -> Result<i64> {
    let term_lock = term.term.lock();
    let grid = term_lock.grid();
    let history = grid.total_lines().saturating_sub(grid.screen_lines());
    Ok(history as i64)
}

/// Redraw the terminal buffer directly from Rust
/// Returns: (history-size cursor-row cursor-col is-alt-screen wrap-flags)
#[defun(name = "alacritty--module-redraw")]
fn redraw<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let term_lock = term.term.lock();
    let alt_screen = term_lock.mode().contains(TermMode::ALT_SCREEN);
    let grid = term_lock.grid();

    let cursor = grid.cursor.point;
    let cursor_row = if cursor.line.0 < 0 {
        0
    } else {
        cursor.line.0 as i64
    };
    let cursor_col = cursor.column.0 as i64;
    let num_cols = grid.columns();
    let screen_lines = grid.screen_lines() as i32;
    let history_size = grid.total_lines().saturating_sub(grid.screen_lines()) as i64;

    // Cache symbols
    let syms = RenderSymbols::new(env)?;

    // Always do full redraw for simplicity (damage tracking can be added later)
    env.call(syms.erase_buffer, [])?;

    if alt_screen {
        // Alt screen: only visible lines
        for line_idx in 0..screen_lines {
            let row = &grid[Line(line_idx)];
            // On cursor line, preserve whitespace up to cursor position
            let min_cols = if line_idx == cursor.line.0 {
                Some(cursor.column.0 + 1)
            } else {
                None
            };
            insert_line_content(env, row, num_cols, min_cols, &syms)?;
            if line_idx < screen_lines - 1 {
                env.call(syms.insert_fn, [syms.newline])?;
            }
        }
    } else {
        // Normal mode: include scrollback
        let start_line = -(history_size as i32);
        for line_idx in start_line..screen_lines {
            let row = &grid[Line(line_idx)];
            // On cursor line, preserve whitespace up to cursor position
            let min_cols = if line_idx == cursor.line.0 {
                Some(cursor.column.0 + 1)
            } else {
                None
            };
            insert_line_content(env, row, num_cols, min_cols, &syms)?;
            env.call(syms.insert_fn, [syms.newline])?;
        }
    }

    // Build wrap flags list
    let mut wrap_flags = syms.nil;
    for line_idx in (0..screen_lines).rev() {
        let row = &grid[Line(line_idx)];
        let last_col = num_cols.saturating_sub(1);
        if row[Column(last_col)].flags.contains(CellFlags::WRAPLINE) {
            let idx_val = (line_idx as i64).into_lisp(env)?;
            wrap_flags = env.cons(idx_val, wrap_flags)?;
        }
    }

    // Return: (history-size cursor-row cursor-col is-alt-screen wrap-flags)
    let result = env.cons(wrap_flags, syms.nil)?;
    let alt_val = if alt_screen { syms.t } else { syms.nil };
    let result = env.cons(alt_val, result)?;
    let result = env.cons(cursor_col.into_lisp(env)?, result)?;
    let result = env.cons(cursor_row.into_lisp(env)?, result)?;
    let result = env.cons(history_size.into_lisp(env)?, result)?;

    Ok(result)
}

/// Redraw terminal using damage tracking for partial updates
/// Now always includes scrollback history (like vterm) unless in alt screen mode.
/// Returns: (damage-type history-size cursor-row cursor-col is-alt-screen wrap-flags)
/// damage-type is 'full, 'partial, or 'none
#[defun(name = "alacritty--module-redraw-with-damage")]
fn redraw_with_damage<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let mut term_lock = term.term.lock();
    let alt_screen = term_lock.mode().contains(TermMode::ALT_SCREEN);

    // Get damage info before we borrow grid
    let damage = term_lock.damage();
    let is_full_damage = matches!(damage, TermDamage::Full);
    let damaged_lines: Vec<_> = match damage {
        TermDamage::Full => Vec::new(),
        TermDamage::Partial(iter) => iter.collect(),
    };

    let grid = term_lock.grid();
    let cursor = grid.cursor.point;
    let cursor_row = if cursor.line.0 < 0 {
        0
    } else {
        cursor.line.0 as i64
    };
    let cursor_col = cursor.column.0 as i64;
    let num_cols = grid.columns();
    let screen_lines = grid.screen_lines() as i32;
    let history_size = grid.total_lines().saturating_sub(grid.screen_lines()) as i64;

    // Cache symbols
    let syms = RenderSymbolsWithDamage::new(env)?;

    let damage_type_sym = if is_full_damage {
        // Full redraw needed
        env.call(syms.base.erase_buffer, [])?;

        if alt_screen {
            // Alt screen: only visible lines (no scrollback)
            for line_idx in 0..screen_lines {
                let row = &grid[Line(line_idx)];
                let min_cols = if line_idx == cursor.line.0 {
                    Some(cursor.column.0 + 1)
                } else {
                    None
                };
                insert_line_content(env, row, num_cols, min_cols, &syms.base)?;
                if line_idx < screen_lines - 1 {
                    env.call(syms.base.insert_fn, [syms.base.newline])?;
                }
            }
        } else {
            // Normal mode: include scrollback history (like vterm)
            let start_line = -(history_size as i32);
            for line_idx in start_line..screen_lines {
                let row = &grid[Line(line_idx)];
                // On cursor line, preserve whitespace up to cursor position
                let min_cols = if line_idx == cursor.line.0 {
                    Some(cursor.column.0 + 1)
                } else {
                    None
                };
                insert_line_content(env, row, num_cols, min_cols, &syms.base)?;
                // Add newline after each line except the last visible line
                if line_idx < screen_lines - 1 {
                    env.call(syms.base.insert_fn, [syms.base.newline])?;
                }
            }
        }
        env.intern("full")?
    } else if damaged_lines.is_empty() {
        // No damage, nothing to redraw
        env.intern("none")?
    } else {
        // Partial redraw - only update damaged lines
        // Note: damage tracking only applies to visible screen, not scrollback
        for damage in &damaged_lines {
            // Calculate buffer line number (1-based for Emacs)
            // Account for scrollback: buffer_line = history_size + damage.line + 1
            let buffer_line = if alt_screen {
                (damage.line as i64) + 1
            } else {
                history_size + (damage.line as i64) + 1
            };

            // Go to the damaged line
            env.call(syms.goto_line, [buffer_line.into_lisp(env)?])?;

            // Delete the current line content (but not the newline)
            let line_start = env.call(syms.line_beginning_position, [])?;
            let line_end = env.call(syms.line_end_position, [])?;
            env.call(syms.delete_region, [line_start, line_end])?;

            // Get the terminal line content
            // damage.line is viewport-relative, convert to grid index
            let grid_line = Line(damage.line as i32);
            let row = &grid[grid_line];

            // Render the damaged portion - for simplicity, redraw the whole line
            let min_cols = if grid_line.0 == cursor.line.0 {
                Some(cursor.column.0 + 1)
            } else {
                None
            };
            insert_line_content(env, row, num_cols, min_cols, &syms.base)?;
        }
        env.intern("partial")?
    };

    // Build wrap flags list for visible screen lines
    // Include history offset so Elisp can correctly identify wrapped lines
    let mut wrap_flags = syms.base.nil;
    if alt_screen {
        for line_idx in (0..screen_lines).rev() {
            let row = &grid[Line(line_idx)];
            let last_col = num_cols.saturating_sub(1);
            if row[Column(last_col)].flags.contains(CellFlags::WRAPLINE) {
                let idx_val = (line_idx as i64).into_lisp(env)?;
                wrap_flags = env.cons(idx_val, wrap_flags)?;
            }
        }
    } else {
        // Include scrollback lines in wrap flag calculation
        let start_line = -(history_size as i32);
        for line_idx in (start_line..screen_lines).rev() {
            let row = &grid[Line(line_idx)];
            let last_col = num_cols.saturating_sub(1);
            if row[Column(last_col)].flags.contains(CellFlags::WRAPLINE) {
                // Convert to buffer line number (0-indexed from start of buffer)
                let buffer_line_idx = (line_idx + history_size as i32) as i64;
                let idx_val = buffer_line_idx.into_lisp(env)?;
                wrap_flags = env.cons(idx_val, wrap_flags)?;
            }
        }
    }

    // Build result before resetting damage
    let result = env.cons(wrap_flags, syms.base.nil)?;
    let alt_val = if alt_screen {
        syms.base.t
    } else {
        syms.base.nil
    };
    let result = env.cons(alt_val, result)?;
    let result = env.cons(cursor_col.into_lisp(env)?, result)?;
    let result = env.cons(cursor_row.into_lisp(env)?, result)?;
    let result = env.cons(history_size.into_lisp(env)?, result)?;
    let result = env.cons(damage_type_sym, result)?;

    // Reset damage after processing - need to drop grid borrow first
    // The grid is borrowed from term_lock, so we need to end this scope
    drop(term_lock);
    term.reset_damage();

    Ok(result)
}

/// Extended symbols for damage-aware rendering
struct RenderSymbolsWithDamage<'e> {
    base: RenderSymbols<'e>,
    goto_line: Value<'e>,
    line_beginning_position: Value<'e>,
    line_end_position: Value<'e>,
    delete_region: Value<'e>,
}

impl<'e> RenderSymbolsWithDamage<'e> {
    fn new(env: &'e Env) -> Result<Self> {
        Ok(Self {
            base: RenderSymbols::new(env)?,
            goto_line: env.intern("goto-line")?,
            line_beginning_position: env.intern("line-beginning-position")?,
            line_end_position: env.intern("line-end-position")?,
            delete_region: env.intern("delete-region")?,
        })
    }
}

/// Cached Emacs symbols for rendering
struct RenderSymbols<'e> {
    erase_buffer: Value<'e>,
    insert_fn: Value<'e>,
    propertize: Value<'e>,
    nil: Value<'e>,
    t: Value<'e>,
    newline: Value<'e>,
    face_sym: Value<'e>,
    list_fn: Value<'e>,
    foreground_sym: Value<'e>,
    background_sym: Value<'e>,
    weight_sym: Value<'e>,
    bold_sym: Value<'e>,
    slant_sym: Value<'e>,
    italic_sym: Value<'e>,
    underline_sym: Value<'e>,
    inverse_video_sym: Value<'e>,
}

impl<'e> RenderSymbols<'e> {
    fn new(env: &'e Env) -> Result<Self> {
        Ok(Self {
            erase_buffer: env.intern("erase-buffer")?,
            insert_fn: env.intern("insert")?,
            propertize: env.intern("propertize")?,
            nil: env.intern("nil")?,
            t: env.intern("t")?,
            newline: "\n".into_lisp(env)?,
            face_sym: env.intern("face")?,
            list_fn: env.intern("list")?,
            foreground_sym: env.intern(":foreground")?,
            background_sym: env.intern(":background")?,
            weight_sym: env.intern(":weight")?,
            bold_sym: env.intern("bold")?,
            slant_sym: env.intern(":slant")?,
            italic_sym: env.intern("italic")?,
            underline_sym: env.intern(":underline")?,
            inverse_video_sym: env.intern(":inverse-video")?,
        })
    }
}

/// Insert a line's content with text properties
/// If min_columns is Some(n), preserve at least n columns (don't trim beyond that point)
fn insert_line_content<'e>(
    env: &'e Env,
    row: &alacritty_terminal::grid::Row<alacritty_terminal::term::cell::Cell>,
    columns: usize,
    min_columns: Option<usize>,
    syms: &RenderSymbols<'e>,
) -> Result<()> {
    let mut current_text = String::new();
    let mut current_fg: Option<(u8, u8, u8)> = None;
    let mut current_bg: Option<(u8, u8, u8)> = None;
    let mut current_flags: Option<CellFlags> = None;

    for col_idx in 0..columns {
        let cell = &row[Column(col_idx)];

        if cell.flags.contains(CellFlags::WIDE_CHAR_SPACER) {
            continue;
        }

        let c = if cell.c == '\0' { ' ' } else { cell.c };
        let fg = color_to_rgb(&cell.fg);
        let bg = color_to_rgb(&cell.bg);
        let flags = cell.flags;

        let attrs_match =
            current_fg == Some(fg) && current_bg == Some(bg) && current_flags == Some(flags);

        if !attrs_match && !current_text.is_empty() {
            insert_styled_text(
                env,
                &current_text,
                current_fg.unwrap(),
                current_bg.unwrap(),
                current_flags.unwrap(),
                syms,
            )?;
            current_text.clear();
        }

        current_text.push(c);
        current_fg = Some(fg);
        current_bg = Some(bg);
        current_flags = Some(flags);
    }

    // Insert final segment
    // Trim trailing whitespace, but preserve up to min_columns if specified
    if !current_text.is_empty() {
        let text_to_insert = if let Some(min_cols) = min_columns {
            // Find how many chars we need to keep to satisfy min_columns
            // We need to be careful: the text may have wide chars that take 2 columns
            let trimmed = current_text.trim_end();
            if trimmed.len() >= min_cols {
                trimmed
            } else {
                // Need to preserve some trailing spaces
                // Calculate visual column count
                let mut visual_col = 0;
                let mut byte_end = 0;
                for (idx, c) in current_text.char_indices() {
                    if visual_col >= min_cols {
                        break;
                    }
                    visual_col += unicode_width::UnicodeWidthChar::width(c).unwrap_or(1);
                    byte_end = idx + c.len_utf8();
                }
                &current_text[..byte_end]
            }
        } else {
            current_text.trim_end()
        };

        if !text_to_insert.is_empty() {
            insert_styled_text(
                env,
                text_to_insert,
                current_fg.unwrap(),
                current_bg.unwrap(),
                current_flags.unwrap(),
                syms,
            )?;
        }
    }

    Ok(())
}

/// Insert styled text using propertize
fn insert_styled_text<'e>(
    env: &'e Env,
    text: &str,
    fg: (u8, u8, u8),
    bg: (u8, u8, u8),
    flags: CellFlags,
    syms: &RenderSymbols<'e>,
) -> Result<()> {
    let text_val = text.into_lisp(env)?;

    let mut props = Vec::new();

    let is_default_fg = fg == (229, 229, 229);
    let is_default_bg = bg == (0, 0, 0);

    if !is_default_fg {
        let fg_val = format!("#{:02x}{:02x}{:02x}", fg.0, fg.1, fg.2).into_lisp(env)?;
        props.push(syms.foreground_sym);
        props.push(fg_val);
    }
    if !is_default_bg {
        let bg_val = format!("#{:02x}{:02x}{:02x}", bg.0, bg.1, bg.2).into_lisp(env)?;
        props.push(syms.background_sym);
        props.push(bg_val);
    }
    if flags.contains(CellFlags::BOLD) {
        props.push(syms.weight_sym);
        props.push(syms.bold_sym);
    }
    if flags.contains(CellFlags::ITALIC) {
        props.push(syms.slant_sym);
        props.push(syms.italic_sym);
    }
    if flags.contains(CellFlags::UNDERLINE) {
        props.push(syms.underline_sym);
        props.push(syms.t);
    }
    if flags.contains(CellFlags::INVERSE) {
        props.push(syms.inverse_video_sym);
        props.push(syms.t);
    }

    if props.is_empty() {
        env.call(syms.insert_fn, [text_val])?;
    } else {
        let face_plist = env.call(syms.list_fn, &props)?;
        let propertized = env.call(syms.propertize, [text_val, syms.face_sym, face_plist])?;
        env.call(syms.insert_fn, [propertized])?;
    }

    Ok(())
}
