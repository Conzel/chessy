use crate::boards::*;
use crate::chess_errors::*;
use crate::pieces::*;
use crate::positions::*;

pub trait Game: std::fmt::Display {
    /// Returns a game with the chess pieces on the standard positions
    fn standard_setup() -> Self;
    /// Moves piece p from start to end. Checking for legality
    /// is implementation dependent. May panic on illegal moves.
    fn player_move(&mut self, p: Piece, start: Position, end: Position) -> ChessResult<()>;
}
