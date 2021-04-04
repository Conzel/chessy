use crate::pieces::*;
/// Describing the moves that can be done in a chessboard.
use crate::positions::*;
use std::fmt;

#[derive(Debug, Clone)]
pub struct Move {
    pub start: Position,
    pub end: Position,
    pub piece: Piece,
    pub kind: MoveType,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            self.piece.algebraic(),
            self.start,
            if self.kind == MoveType::Capture {
                "x"
            } else {
                ""
            },
            self.end
        )
    }
}

impl Move {
    pub fn new(start: Position, end: Position, piece: Piece, kind: MoveType) -> Self {
        Move {
            start: start,
            end: end,
            piece: piece,
            kind: kind,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MoveType {
    Standard,
    Capture,
    EnPassant,
    Castle,
    PawnTwostep,
}
