//! Alacritty Terminal for Emacs - Terminal emulator for Emacs using alacritty_terminal
//!
//! This module provides a terminal emulator for Emacs using the alacritty_terminal
//! library, similar to vterm but using Alacritty's terminal emulation.

use alacritty_terminal::event::{Event, EventListener, WindowSize};
use alacritty_terminal::event_loop::{EventLoop, Msg, Notifier};
use alacritty_terminal::grid::Dimensions;
use alacritty_terminal::index::{Column, Line};
use alacritty_terminal::sync::FairMutex;
use alacritty_terminal::term::cell::Flags as CellFlags;
use alacritty_terminal::term::{Config, Term, TermMode};
use alacritty_terminal::tty::{self, Options as PtyOptions, Shell};
use alacritty_terminal::vte::ansi::{Color, NamedColor};
use emacs::{defun, Env, IntoLisp, Result, Value};
use parking_lot::Mutex;
use std::borrow::Cow;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;

emacs::plugin_is_GPL_compatible!();

/// Terminal dimensions
#[derive(Clone, Copy)]
struct TermSize {
    cols: usize,
    lines: usize,
    cell_width: u16,
    cell_height: u16,
}

impl TermSize {
    fn new(cols: usize, lines: usize) -> Self {
        Self {
            cols,
            lines,
            cell_width: 10,  // Default cell width in pixels
            cell_height: 20, // Default cell height in pixels
        }
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

impl From<TermSize> for WindowSize {
    fn from(size: TermSize) -> Self {
        WindowSize {
            num_cols: size.cols as u16,
            num_lines: size.lines as u16,
            cell_width: size.cell_width,
            cell_height: size.cell_height,
        }
    }
}

/// Events sent from the terminal to Emacs
#[derive(Debug, Clone)]
pub enum TermEvent {
    Title(String),
    Bell,
    Exit,
    ClipboardStore(String),
    ClipboardLoad,
    ColorRequest(usize),
    CursorBlinkingChange,
    Wakeup,
}

/// Event listener that collects events to be processed by Emacs
/// and writes PTY responses back immediately
#[derive(Clone)]
struct EmacEventListener {
    sender: Sender<TermEvent>,
    pty_writer: Sender<Msg>,
}

impl EmacEventListener {
    fn new(sender: Sender<TermEvent>, pty_writer: Sender<Msg>) -> Self {
        Self { sender, pty_writer }
    }
}

impl EventListener for EmacEventListener {
    fn send_event(&self, event: Event) {
        match event {
            Event::Title(title) => {
                let _ = self.sender.send(TermEvent::Title(title));
            }
            Event::Bell => {
                let _ = self.sender.send(TermEvent::Bell);
            }
            Event::Exit => {
                let _ = self.sender.send(TermEvent::Exit);
            }
            Event::ClipboardStore(_, data) => {
                let _ = self.sender.send(TermEvent::ClipboardStore(data));
            }
            Event::ClipboardLoad(_, _) => {
                let _ = self.sender.send(TermEvent::ClipboardLoad);
            }
            Event::ColorRequest(idx, _) => {
                let _ = self.sender.send(TermEvent::ColorRequest(idx));
            }
            Event::PtyWrite(data) => {
                // Write PTY responses (like DA1 response) immediately back to PTY
                let _ = self
                    .pty_writer
                    .send(Msg::Input(Cow::Owned(data.into_bytes())));
            }
            Event::CursorBlinkingChange => {
                let _ = self.sender.send(TermEvent::CursorBlinkingChange);
            }
            Event::Wakeup => {
                let _ = self.sender.send(TermEvent::Wakeup);
            }
            _ => {}
        }
    }
}

/// The main terminal structure holding all terminal state
pub struct AlacrittyTerm {
    term: Arc<FairMutex<Term<EmacEventListener>>>,
    pty_tx: Notifier,
    event_rx: Receiver<TermEvent>,
    size: Mutex<TermSize>,
    title: Mutex<String>,
    exited: Mutex<bool>,
    /// Dirty flag - set when terminal content has changed and needs redraw
    dirty: Arc<Mutex<bool>>,
}

impl AlacrittyTerm {
    fn new(
        cols: usize,
        lines: usize,
        shell: Option<String>,
        scrollback_lines: usize,
    ) -> std::result::Result<Self, String> {
        let size = TermSize::new(cols, lines);
        let (event_tx, event_rx) = channel();

        // Setup PTY options
        // The shell parameter can be either:
        // 1. A simple shell path like "/bin/bash"
        // 2. A complex command like "ssh user@host -t 'cd /path && exec /bin/bash'"
        // For complex commands (containing spaces), we wrap with /bin/sh -c
        let shell_cmd = shell
            .unwrap_or_else(|| std::env::var("SHELL").unwrap_or_else(|_| "/bin/sh".to_string()));

        let (shell_program, shell_args) = if shell_cmd.contains(' ') {
            // Complex command - wrap with sh -c
            ("/bin/sh".to_string(), vec!["-c".to_string(), shell_cmd])
        } else {
            // Simple shell path
            (shell_cmd, vec![])
        };

        let pty_config = PtyOptions {
            shell: Some(Shell::new(shell_program, shell_args)),
            working_directory: None,
            ..Default::default()
        };

        // Create the PTY first
        let window_size = WindowSize::from(size);
        let pty = tty::new(&pty_config, window_size, 0)
            .map_err(|e| format!("Failed to create PTY: {}", e))?;

        // We need to create a dummy event listener first, then update it
        // Actually, let's use a channel-based approach where we forward PTY writes
        let (pty_write_tx, pty_write_rx) = channel::<Msg>();

        // Create the event listener with the PTY write channel
        let event_listener = EmacEventListener::new(event_tx, pty_write_tx);

        // Create the terminal with custom scrollback size
        let config = Config {
            scrolling_history: scrollback_lines,
            ..Config::default()
        };
        let term = Term::new(config, &size, event_listener.clone());
        let term = Arc::new(FairMutex::new(term));

        // Create and spawn the event loop
        let event_loop = EventLoop::new(term.clone(), event_listener, pty, false, false)
            .map_err(|e| format!("Failed to create event loop: {}", e))?;

        let pty_channel = event_loop.channel();
        let pty_tx = Notifier(pty_channel.clone());

        // Spawn the event loop in a background thread
        let _event_loop_handle = event_loop.spawn();

        // Spawn a thread to forward PTY write requests from the event listener
        // to the actual PTY channel
        std::thread::spawn(move || {
            while let Ok(msg) = pty_write_rx.recv() {
                let _ = pty_channel.send(msg);
            }
        });

        Ok(Self {
            term,
            pty_tx,
            event_rx,
            size: Mutex::new(size),
            title: Mutex::new(String::new()),
            exited: Mutex::new(false),
            dirty: Arc::new(Mutex::new(true)), // Start dirty to trigger initial render
        })
    }

    fn write(&self, data: &[u8]) {
        let _ = self.pty_tx.0.send(Msg::Input(Cow::Owned(data.to_vec())));
    }

    fn resize(&self, cols: usize, lines: usize) {
        let mut size = self.size.lock();
        size.cols = cols;
        size.lines = lines;

        let window_size = WindowSize::from(*size);
        self.term.lock().resize(*size);
        let _ = self.pty_tx.0.send(Msg::Resize(window_size));
    }

    fn process_events(&self) -> Vec<TermEvent> {
        let mut events = Vec::new();
        while let Ok(event) = self.event_rx.try_recv() {
            match &event {
                TermEvent::Title(title) => {
                    *self.title.lock() = title.clone();
                }
                TermEvent::Exit => {
                    *self.exited.lock() = true;
                }
                TermEvent::Wakeup => {
                    // Terminal content has changed, mark as dirty
                    *self.dirty.lock() = true;
                }
                _ => {}
            }
            events.push(event);
        }
        events
    }

    fn is_dirty(&self) -> bool {
        *self.dirty.lock()
    }

    fn clear_dirty(&self) {
        *self.dirty.lock() = false;
    }

    fn get_content(&self) -> String {
        let term = self.term.lock();
        let grid = term.grid();
        let mut result = String::new();

        // In alacritty_terminal, Line(0) is the topmost visible line
        // and the grid uses negative indices for scrollback
        for line_idx in 0..grid.screen_lines() {
            let row = &grid[Line(line_idx as i32)];
            let mut line_str = String::new();
            for col_idx in 0..grid.columns() {
                let cell = &row[Column(col_idx)];
                // Skip wide character spacer cells
                if cell.flags.contains(CellFlags::WIDE_CHAR_SPACER) {
                    continue;
                }
                let c = cell.c;
                // Replace null chars with spaces for display
                if c == '\0' || c == ' ' {
                    line_str.push(' ');
                } else {
                    line_str.push(c);
                }
            }
            // Trim trailing spaces from each line
            let trimmed = line_str.trim_end();
            result.push_str(trimmed);
            if line_idx < grid.screen_lines() - 1 {
                result.push('\n');
            }
        }

        result
    }

    fn cursor_position(&self) -> (usize, usize) {
        let term = self.term.lock();
        let cursor = term.grid().cursor.point;
        // Line can be negative in alacritty (for scrollback), clamp to 0
        let line = if cursor.line.0 < 0 {
            0
        } else {
            cursor.line.0 as usize
        };
        (line, cursor.column.0)
    }

    fn is_exited(&self) -> bool {
        *self.exited.lock()
    }

    fn get_title(&self) -> String {
        self.title.lock().clone()
    }

    fn get_mode(&self) -> TermMode {
        self.term.lock().mode().clone()
    }
}

/// Helper to convert Color to RGB values
fn color_to_rgb(color: &Color) -> (u8, u8, u8) {
    match color {
        Color::Spec(rgb) => (rgb.r, rgb.g, rgb.b),
        Color::Named(named) => {
            // Return default colors for named colors
            match named {
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
                _ => (128, 128, 128), // Default gray for other cases
            }
        }
        Color::Indexed(idx) => {
            // Basic 16-color palette mapping
            match idx {
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
                // Extended 256-color palette - simplified
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
            }
        }
    }
}

#[emacs::module(name = "alacritty-module")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

/// Create a new terminal with the given dimensions
/// Returns a user pointer to the terminal state
#[defun(user_ptr, name = "alacritty--module-create")]
fn create(
    cols: i64,
    lines: i64,
    shell: Option<String>,
    scrollback: Option<i64>,
) -> Result<AlacrittyTerm> {
    let scrollback_lines = scrollback.unwrap_or(10000) as usize;
    AlacrittyTerm::new(cols as usize, lines as usize, shell, scrollback_lines)
        .map_err(|e| emacs::Error::msg(e))
}

/// Write data to the terminal PTY (send input to the shell)
#[defun(name = "alacritty--module-write-input")]
fn write_input(term: &AlacrittyTerm, data: String) -> Result<()> {
    term.write(data.as_bytes());
    Ok(())
}

/// Resize the terminal to new dimensions
#[defun(name = "alacritty--module-resize")]
fn resize(term: &AlacrittyTerm, cols: i64, lines: i64) -> Result<()> {
    term.resize(cols as usize, lines as usize);
    Ok(())
}

/// Get the terminal content as a plain string
#[defun(name = "alacritty--module-get-text")]
fn get_text(term: &AlacrittyTerm) -> Result<String> {
    Ok(term.get_content())
}

/// Get the terminal content with styling information (visible screen only)
/// Returns a list of lines, where each line is a list of styled segments:
/// ((text fg-color bg-color bold italic underline) ...)
#[defun(name = "alacritty--module-get-styled-content")]
fn get_styled_content<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let term_lock = term.term.lock();
    let grid = term_lock.grid();

    let mut lines_list = env.intern("nil")?;

    // Process lines in reverse order so we can build the list with cons
    // Only process visible screen lines (0..screen_lines)
    for line_idx in (0..grid.screen_lines()).rev() {
        let row = &grid[Line(line_idx as i32)];
        let segments_list = build_line_segments(env, row, grid.columns())?;
        lines_list = env.cons(segments_list, lines_list)?;
    }

    Ok(lines_list)
}

/// Get the full terminal content including scrollback history with styling information
/// Returns a list of lines, where each line is a list of styled segments:
/// ((text fg-color bg-color bold italic underline) ...)
/// Lines are returned from oldest (top of scrollback) to newest (bottom of screen)
#[defun(name = "alacritty--module-get-full-styled-content")]
fn get_full_styled_content<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let term_lock = term.term.lock();
    let grid = term_lock.grid();

    let mut lines_list = env.intern("nil")?;

    // Calculate the range of lines: from topmost (negative, oldest history) to bottommost (positive, visible)
    let history_size = grid.total_lines().saturating_sub(grid.screen_lines());
    let start_line = -(history_size as i32);
    let end_line = grid.screen_lines() as i32;

    // Process lines in reverse order so we can build the list with cons
    // Go from end_line-1 down to start_line
    for line_idx in (start_line..end_line).rev() {
        let row = &grid[Line(line_idx)];
        let segments_list = build_line_segments(env, row, grid.columns())?;
        lines_list = env.cons(segments_list, lines_list)?;
    }

    Ok(lines_list)
}

/// Get the number of scrollback (history) lines
#[defun(name = "alacritty--module-history-size")]
fn history_size(term: &AlacrittyTerm) -> Result<i64> {
    let term_lock = term.term.lock();
    let grid = term_lock.grid();
    let history = grid.total_lines().saturating_sub(grid.screen_lines());
    Ok(history as i64)
}

/// Helper to build segments for a single line
fn build_line_segments<'e>(
    env: &'e Env,
    row: &alacritty_terminal::grid::Row<alacritty_terminal::term::cell::Cell>,
    columns: usize,
) -> Result<Value<'e>> {
    let mut segments_list = env.intern("nil")?;

    // Group consecutive cells with the same attributes
    let mut current_text = String::new();
    let mut current_fg: Option<(u8, u8, u8)> = None;
    let mut current_bg: Option<(u8, u8, u8)> = None;
    let mut current_flags: Option<CellFlags> = None;

    for col_idx in (0..columns).rev() {
        let cell = &row[Column(col_idx)];

        // Skip wide character spacer cells - they're placeholders for the
        // second half of wide characters and shouldn't be rendered
        if cell.flags.contains(CellFlags::WIDE_CHAR_SPACER) {
            continue;
        }

        let c = if cell.c == '\0' { ' ' } else { cell.c };

        let fg = color_to_rgb(&cell.fg);
        let bg = color_to_rgb(&cell.bg);
        let flags = cell.flags;

        // Check if attributes changed
        let attrs_match =
            current_fg == Some(fg) && current_bg == Some(bg) && current_flags == Some(flags);

        if !attrs_match && !current_text.is_empty() {
            // Emit current segment (text is reversed, so reverse it back)
            let text: String = current_text.chars().rev().collect();
            let segment = make_segment(
                env,
                &text,
                current_fg.unwrap(),
                current_bg.unwrap(),
                current_flags.unwrap(),
            )?;
            segments_list = env.cons(segment, segments_list)?;
            current_text.clear();
        }

        current_text.push(c);
        current_fg = Some(fg);
        current_bg = Some(bg);
        current_flags = Some(flags);
    }

    // Emit final segment for the line
    if !current_text.is_empty() {
        let text: String = current_text.chars().rev().collect();
        let segment = make_segment(
            env,
            &text,
            current_fg.unwrap(),
            current_bg.unwrap(),
            current_flags.unwrap(),
        )?;
        segments_list = env.cons(segment, segments_list)?;
    }

    Ok(segments_list)
}

/// Helper to create a segment: (text fg-color bg-color bold italic underline inverse)
fn make_segment<'e>(
    env: &'e Env,
    text: &str,
    fg: (u8, u8, u8),
    bg: (u8, u8, u8),
    flags: CellFlags,
) -> Result<Value<'e>> {
    let text_val = text.into_lisp(env)?;
    let fg_val = format!("#{:02x}{:02x}{:02x}", fg.0, fg.1, fg.2).into_lisp(env)?;
    let bg_val = format!("#{:02x}{:02x}{:02x}", bg.0, bg.1, bg.2).into_lisp(env)?;

    let bold = if flags.contains(CellFlags::BOLD) {
        env.intern("t")?
    } else {
        env.intern("nil")?
    };
    let italic = if flags.contains(CellFlags::ITALIC) {
        env.intern("t")?
    } else {
        env.intern("nil")?
    };
    let underline = if flags.contains(CellFlags::UNDERLINE) {
        env.intern("t")?
    } else {
        env.intern("nil")?
    };
    let inverse = if flags.contains(CellFlags::INVERSE) {
        env.intern("t")?
    } else {
        env.intern("nil")?
    };

    // Build list: (text fg bg bold italic underline inverse)
    let nil = env.intern("nil")?;
    let result = env.cons(inverse, nil)?;
    let result = env.cons(underline, result)?;
    let result = env.cons(italic, result)?;
    let result = env.cons(bold, result)?;
    let result = env.cons(bg_val, result)?;
    let result = env.cons(fg_val, result)?;
    let result = env.cons(text_val, result)?;

    Ok(result)
}

/// Check if a line wraps (has a fake newline at the end)
/// Returns true if the line continues on the next line (i.e., should NOT have a real newline)
/// Line index is 0-based from the start of the buffer (including scrollback)
/// So line 0 is the first line of scrollback, and scrollback_size + screen_line is the visible area
#[defun(name = "alacritty--module-line-wraps")]
fn line_wraps(term: &AlacrittyTerm, line: i64) -> Result<bool> {
    let term_lock = term.term.lock();
    let grid = term_lock.grid();

    // Convert buffer-relative line (0-based from top of scrollback) to grid Line
    // Grid uses negative indices for scrollback: Line(-history_size) to Line(-1)
    // And positive for visible: Line(0) to Line(screen_lines-1)
    let history_size = grid.total_lines().saturating_sub(grid.screen_lines());
    let grid_line = (line as i32) - (history_size as i32);

    // Check bounds
    let topmost = -(history_size as i32);
    let bottommost = grid.screen_lines() as i32 - 1;
    if grid_line < topmost || grid_line > bottommost {
        return Ok(false);
    }

    let row = &grid[Line(grid_line)];
    // Check if the last column has the WRAPLINE flag
    let last_col = grid.columns().saturating_sub(1);
    Ok(row[Column(last_col)].flags.contains(CellFlags::WRAPLINE))
}

/// Get the cursor row (0-indexed)
#[defun(name = "alacritty--module-cursor-row")]
fn cursor_row(term: &AlacrittyTerm) -> Result<i64> {
    Ok(term.cursor_position().0 as i64)
}

/// Get the cursor column (0-indexed)
#[defun(name = "alacritty--module-cursor-col")]
fn cursor_col(term: &AlacrittyTerm) -> Result<i64> {
    Ok(term.cursor_position().1 as i64)
}

/// Check if the terminal process has exited
#[defun(name = "alacritty--module-is-exited")]
fn is_exited(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.is_exited())
}

/// Check if the terminal content has changed and needs redrawing
#[defun(name = "alacritty--module-is-dirty")]
fn is_dirty(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.is_dirty())
}

/// Clear the dirty flag after redrawing
#[defun(name = "alacritty--module-clear-dirty")]
fn clear_dirty(term: &AlacrittyTerm) -> Result<()> {
    term.clear_dirty();
    Ok(())
}

/// Get the terminal title (set by the shell/programs via escape sequences)
#[defun(name = "alacritty--module-get-title")]
fn get_title(term: &AlacrittyTerm) -> Result<String> {
    Ok(term.get_title())
}

/// Get the number of columns
#[defun(name = "alacritty--module-columns")]
fn columns(term: &AlacrittyTerm) -> Result<i64> {
    Ok(term.size.lock().cols as i64)
}

/// Get the number of lines
#[defun(name = "alacritty--module-lines")]
fn lines(term: &AlacrittyTerm) -> Result<i64> {
    Ok(term.size.lock().lines as i64)
}

/// Process pending events and return event info
/// Returns a list of events: ((type . data) ...)
#[defun(name = "alacritty--module-poll-events")]
fn poll_events<'e>(env: &'e Env, term: &AlacrittyTerm) -> Result<Value<'e>> {
    let events = term.process_events();
    let mut result = env.intern("nil")?;

    for event in events.into_iter().rev() {
        let event_val = match event {
            TermEvent::Title(title) => {
                let title_sym = env.intern("title")?;
                let title_val = title.into_lisp(env)?;
                env.cons(title_sym, title_val)?
            }
            TermEvent::Bell => {
                let bell_sym = env.intern("bell")?;
                env.cons(bell_sym, env.intern("t")?)?
            }
            TermEvent::Exit => {
                let exit_sym = env.intern("exit")?;
                env.cons(exit_sym, env.intern("t")?)?
            }
            TermEvent::ClipboardStore(data) => {
                let clip_sym = env.intern("clipboard-store")?;
                let data_val = data.into_lisp(env)?;
                env.cons(clip_sym, data_val)?
            }
            TermEvent::ClipboardLoad => {
                let clip_sym = env.intern("clipboard-load")?;
                env.cons(clip_sym, env.intern("t")?)?
            }
            TermEvent::Wakeup => {
                let wakeup_sym = env.intern("wakeup")?;
                env.cons(wakeup_sym, env.intern("t")?)?
            }
            TermEvent::CursorBlinkingChange => {
                let blink_sym = env.intern("cursor-blink-change")?;
                // Get current blink state from terminal
                let blink_state = term.term.lock().cursor_style().blinking;
                if blink_state {
                    env.cons(blink_sym, env.intern("t")?)?
                } else {
                    env.cons(blink_sym, env.intern("nil")?)?
                }
            }
            _ => continue,
        };
        result = env.cons(event_val, result)?;
    }

    Ok(result)
}

/// Check if the terminal is in application cursor mode
#[defun(name = "alacritty--module-app-cursor-mode")]
fn app_cursor_mode(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.get_mode().contains(TermMode::APP_CURSOR))
}

/// Check if the terminal is in application keypad mode
#[defun(name = "alacritty--module-app-keypad-mode")]
fn app_keypad_mode(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.get_mode().contains(TermMode::APP_KEYPAD))
}

/// Check if the terminal is in alternate screen mode
#[defun(name = "alacritty--module-alt-screen-mode")]
fn alt_screen_mode(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.get_mode().contains(TermMode::ALT_SCREEN))
}

/// Check if bracketed paste mode is enabled
#[defun(name = "alacritty--module-bracketed-paste-mode")]
fn bracketed_paste_mode(term: &AlacrittyTerm) -> Result<bool> {
    Ok(term.get_mode().contains(TermMode::BRACKETED_PASTE))
}

/// Check if the cursor should be blinking
#[defun(name = "alacritty--module-cursor-blink")]
fn cursor_blink(term: &AlacrittyTerm) -> Result<bool> {
    let term_lock = term.term.lock();
    Ok(term_lock.cursor_style().blinking)
}

/// Get a single line of terminal content (0-indexed)
#[defun(name = "alacritty--module-get-line")]
fn get_line(term: &AlacrittyTerm, line: i64) -> Result<String> {
    let term_lock = term.term.lock();
    let grid = term_lock.grid();
    let line_idx = line as i32;

    if line_idx < 0 || line_idx >= grid.screen_lines() as i32 {
        return Ok(String::new());
    }

    let row = &grid[Line(line_idx)];
    let mut result = String::new();
    for col in 0..grid.columns() {
        let cell = &row[Column(col)];
        // Skip wide character spacer cells
        if cell.flags.contains(CellFlags::WIDE_CHAR_SPACER) {
            continue;
        }
        let c = cell.c;
        if c == '\0' {
            result.push(' ');
        } else {
            result.push(c);
        }
    }

    Ok(result.trim_end().to_string())
}

/// Get cell at position with attributes
/// Returns (char fg-color bg-color flags) or nil if out of bounds
#[defun(name = "alacritty--module-get-cell")]
fn get_cell<'e>(env: &'e Env, term: &AlacrittyTerm, row: i64, col: i64) -> Result<Value<'e>> {
    let term_lock = term.term.lock();
    let grid = term_lock.grid();

    if row < 0 || row >= grid.screen_lines() as i64 || col < 0 || col >= grid.columns() as i64 {
        return env.intern("nil");
    }

    let cell = &grid[Line(row as i32)][Column(col as usize)];

    // Create a list with cell information
    let char_str = cell.c.to_string().into_lisp(env)?;

    let (fg_r, fg_g, fg_b) = color_to_rgb(&cell.fg);
    let (bg_r, bg_g, bg_b) = color_to_rgb(&cell.bg);

    let fg_color = format!("#{:02x}{:02x}{:02x}", fg_r, fg_g, fg_b).into_lisp(env)?;
    let bg_color = format!("#{:02x}{:02x}{:02x}", bg_r, bg_g, bg_b).into_lisp(env)?;

    // Flags as a list of symbols
    let mut flags = env.intern("nil")?;
    if cell.flags.contains(CellFlags::BOLD) {
        let bold = env.intern("bold")?;
        flags = env.cons(bold, flags)?;
    }
    if cell.flags.contains(CellFlags::ITALIC) {
        let italic = env.intern("italic")?;
        flags = env.cons(italic, flags)?;
    }
    if cell.flags.contains(CellFlags::UNDERLINE) {
        let underline = env.intern("underline")?;
        flags = env.cons(underline, flags)?;
    }
    if cell.flags.contains(CellFlags::INVERSE) {
        let inverse = env.intern("inverse")?;
        flags = env.cons(inverse, flags)?;
    }
    if cell.flags.contains(CellFlags::STRIKEOUT) {
        let strikeout = env.intern("strikeout")?;
        flags = env.cons(strikeout, flags)?;
    }
    if cell.flags.contains(CellFlags::HIDDEN) {
        let hidden = env.intern("hidden")?;
        flags = env.cons(hidden, flags)?;
    }

    // Build result list
    let nil = env.intern("nil")?;
    let result = env.cons(flags, nil)?;
    let result = env.cons(bg_color, result)?;
    let result = env.cons(fg_color, result)?;
    let result = env.cons(char_str, result)?;

    Ok(result)
}

/// Send a special key to the terminal
/// Key can be: up, down, left, right, home, end, page-up, page-down,
/// tab, backspace, delete, insert, enter, escape, f1-f12
#[defun(name = "alacritty--module-send-key")]
fn send_key(term: &AlacrittyTerm, key: String, modifiers: Option<String>) -> Result<()> {
    let mode = term.get_mode();
    let app_cursor = mode.contains(TermMode::APP_CURSOR);
    let _app_keypad = mode.contains(TermMode::APP_KEYPAD);

    let mods = modifiers.unwrap_or_default();
    let ctrl = mods.contains("C");
    let alt = mods.contains("M");
    let shift = mods.contains("S");

    let seq: &[u8] = match key.as_str() {
        "up" => {
            if app_cursor {
                b"\x1bOA"
            } else {
                b"\x1b[A"
            }
        }
        "down" => {
            if app_cursor {
                b"\x1bOB"
            } else {
                b"\x1b[B"
            }
        }
        "right" => {
            if app_cursor {
                b"\x1bOC"
            } else {
                b"\x1b[C"
            }
        }
        "left" => {
            if app_cursor {
                b"\x1bOD"
            } else {
                b"\x1b[D"
            }
        }
        "home" => b"\x1b[H",
        "end" => b"\x1b[F",
        "page-up" => b"\x1b[5~",
        "page-down" => b"\x1b[6~",
        "tab" => {
            if shift {
                b"\x1b[Z"
            } else {
                b"\t"
            }
        }
        "backspace" => {
            if ctrl {
                b"\x08"
            } else if alt {
                b"\x1b\x7f"
            } else {
                b"\x7f"
            }
        }
        "delete" => b"\x1b[3~",
        "insert" => b"\x1b[2~",
        "enter" | "return" => {
            if alt {
                b"\x1b\r"
            } else {
                b"\r"
            }
        }
        "escape" => b"\x1b",
        "f1" => b"\x1bOP",
        "f2" => b"\x1bOQ",
        "f3" => b"\x1bOR",
        "f4" => b"\x1bOS",
        "f5" => b"\x1b[15~",
        "f6" => b"\x1b[17~",
        "f7" => b"\x1b[18~",
        "f8" => b"\x1b[19~",
        "f9" => b"\x1b[20~",
        "f10" => b"\x1b[21~",
        "f11" => b"\x1b[23~",
        "f12" => b"\x1b[24~",
        _ => return Ok(()),
    };

    term.write(seq);
    Ok(())
}

/// Send a character with modifiers
/// Handles ctrl+char, meta+char combinations
#[defun(name = "alacritty--module-send-char")]
fn send_char(term: &AlacrittyTerm, c: i64, modifiers: Option<String>) -> Result<()> {
    let ch = char::from_u32(c as u32).unwrap_or('\0');
    let mods = modifiers.unwrap_or_default();
    let ctrl = mods.contains("C");
    let alt = mods.contains("M");

    let mut data = Vec::new();

    if alt {
        data.push(0x1b);
    }

    if ctrl && ch.is_ascii_alphabetic() {
        // Ctrl+A = 0x01, Ctrl+Z = 0x1A
        let ctrl_char = (ch.to_ascii_lowercase() as u8) - b'a' + 1;
        data.push(ctrl_char);
    } else {
        let mut buf = [0u8; 4];
        let s = ch.encode_utf8(&mut buf);
        data.extend_from_slice(s.as_bytes());
    }

    term.write(&data);
    Ok(())
}

/// Send a paste with optional bracketed paste mode support
#[defun(name = "alacritty--module-paste")]
fn paste(term: &AlacrittyTerm, text: String) -> Result<()> {
    let mode = term.get_mode();
    let bracketed = mode.contains(TermMode::BRACKETED_PASTE);

    if bracketed {
        term.write(b"\x1b[200~");
    }
    term.write(text.as_bytes());
    if bracketed {
        term.write(b"\x1b[201~");
    }
    Ok(())
}
