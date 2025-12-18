use emacs::{defun, Env, Result};
use alacritty_terminal::term::{Term, Config};
use alacritty_terminal::grid::Dimensions;
use alacritty_terminal::event::{Event, EventListener};
use alacritty_terminal::vte::ansi::Processor;

emacs::plugin_is_GPL_compatible!();

#[derive(Clone)]
struct NoopEventListener;
impl EventListener for NoopEventListener {
    fn send_event(&self, _event: Event) {}
}

struct TermDimensions {
    cols: usize,
    lines: usize,
}

impl Dimensions for TermDimensions {
    fn columns(&self) -> usize { self.cols }
    fn screen_lines(&self) -> usize { self.lines }
    fn total_lines(&self) -> usize { self.lines }
}

struct AlacrittyTerm {
    term: Term<NoopEventListener>,
    processor: Processor,
}

#[emacs::module(name = "emacs-alacritty")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

#[defun(user_ptr)]
fn create_term(cols: i64, lines: i64) -> Result<AlacrittyTerm> {
    let config = Config::default();
    let dimensions = TermDimensions {
        cols: cols as usize,
        lines: lines as usize,
    };
    let term = Term::new(config, &dimensions, NoopEventListener);
    let processor = Processor::new();
    Ok(AlacrittyTerm { term, processor })
}

#[defun]
fn feed(at: &mut AlacrittyTerm, data: String) -> Result<()> {
    at.processor.advance(&mut at.term, data.as_bytes());
    Ok(())
}

#[defun]
fn resize(at: &mut AlacrittyTerm, cols: i64, lines: i64) -> Result<()> {
    let dimensions = TermDimensions {
        cols: cols as usize,
        lines: lines as usize,
    };
    at.term.resize(dimensions);
    Ok(())
}

#[defun]
fn get_contents(at: &AlacrittyTerm) -> Result<String> {
    let mut s = String::new();
    let grid = at.term.grid();
    for line in 0..grid.screen_lines() {
        let row = &grid[alacritty_terminal::index::Line(line as i32)];
        for col in 0..grid.columns() {
            let cell = &row[alacritty_terminal::index::Column(col)];
            s.push(cell.c);
        }
        s.push('\n');
    }
    Ok(s)
}

#[defun]
fn get_line(at: &AlacrittyTerm, line: i64) -> Result<String> {
    let mut s = String::new();
    let grid = at.term.grid();
    if (line as i32) < 0 || line as usize >= grid.screen_lines() {
        return Ok("".to_string());
    }
    let row = &grid[alacritty_terminal::index::Line(line as i32)];
    for col in 0..grid.columns() {
        let cell = &row[alacritty_terminal::index::Column(col)];
        s.push(cell.c);
    }
    Ok(s)
}

#[defun]
fn cursor_row(at: &AlacrittyTerm) -> Result<i64> {
    Ok(at.term.grid().cursor.point.line.0 as i64)
}

#[defun]
fn cursor_col(at: &AlacrittyTerm) -> Result<i64> {
    Ok(at.term.grid().cursor.point.column.0 as i64)
}
