use crate::chess_errors::*;
use crate::game_state::GameState;
use crate::moves::*;
use crate::pieces::Color;

/// An engine is an object that drives a GameState with the corresponding memory
/// (stack of past moves etc).
#[derive(Clone)]
pub struct StandardEngine {
    state: GameState,
    move_stack: Vec<Move>,
}

impl StandardEngine {
    pub fn new(g: GameState) -> Self {
        StandardEngine {
            state: g,
            move_stack: Vec::new(),
        }
    }

    pub fn next(&mut self, m: Move) {
        self.state.make_move(&m);
        self.move_stack.push(m);
    }

    /// Undoes the last move. Returns an error if no move is left.
    pub fn undo(&mut self) -> ChessResult<()> {
        let last_move = self
            .move_stack
            .pop()
            .ok_or(ChessError::from("No move left to undo"))?;
        self.state.undo_move(&last_move);
        Ok(())
    }

    pub fn gen_moves(&self) -> Option<Vec<Move>> {
        self.state.gen_moves()
    }

    pub fn get_state(&self) -> &GameState {
        &self.state
    }

    pub fn get_moves(&self) -> Vec<Move> {
        self.move_stack.clone()
    }
}
