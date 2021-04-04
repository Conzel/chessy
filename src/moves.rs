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
            if let MoveType::Capture(_) = self.kind {
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

#[derive(Debug, Clone, PartialEq)]
pub enum MoveType {
    Standard,
    Capture(Piece),
    EnPassant,
    Castle,
    PawnTwostep,
}
