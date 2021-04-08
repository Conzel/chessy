use crate::pieces::*;
/// Describing the moves that can be done in a chessboard.
use crate::positions::*;
use std::fmt;

/// Move from ... to ...
pub struct PlayerMove(pub Position, pub Position);

impl From<Move> for PlayerMove {
    fn from(mv: Move) -> Self {
        PlayerMove(mv.start, mv.end)
    }
}

impl From<&Move> for PlayerMove {
    fn from(mv: &Move) -> Self {
        PlayerMove(mv.start, mv.end)
    }
}

impl fmt::Display for PlayerMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

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
