use std::fmt::{self, Display};

// ---------------------------------------------
// Pieces
// ---------------------------------------------

#[derive(Debug, Clone, Copy)]
pub enum Piece {
    PawnWhite,
    KnightWhite,
    BishopWhite,
    RookWhite,
    QueenWhite,
    KingWhite,
    PawnBlack,
    KnightBlack,
    BishopBlack,
    RookBlack,
    QueenBlack,
    KingBlack,
    Empty,
}

#[derive(Debug, Clone)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    Empty,
}

#[derive(Debug, Clone)]
pub enum Color {
    Black,
    White,
    None,
}

impl Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Piece::*;
        let symbol = match self {
            KingWhite => '\u{2654}',
            QueenWhite => '\u{2655}',
            RookWhite => '\u{2656}',
            BishopWhite => '\u{2657}',
            KnightWhite => '\u{2658}',
            PawnWhite => '\u{2659}',
            KingBlack => '\u{265a}',
            QueenBlack => '\u{265b}',
            RookBlack => '\u{265c}',
            BishopBlack => '\u{265d}',
            KnightBlack => '\u{265e}',
            PawnBlack => '\u{265f}',
            Empty => ' ',
        };
        write!(f, "{}", symbol)?;
        Ok(())
    }
}
