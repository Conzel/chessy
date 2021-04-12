use crate::Position;
use std::fmt::{self, Display};

// ---------------------------------------------
// Pieces
// ---------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Piece {
    PawnWhite = 1,
    KnightWhite = 2,
    BishopWhite = 3,
    RookWhite = 4,
    QueenWhite = 5,
    KingWhite = 6,
    PawnBlack = 7,
    KnightBlack = 8,
    BishopBlack = 9,
    RookBlack = 10,
    QueenBlack = 11,
    KingBlack = 12,
    Empty = 13,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    Empty,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

impl Color {
    pub fn opposite(self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
            Color::None => Color::None,
        }
    }
}

impl Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Color::*;
        let name = match self {
            White => "White",
            Black => "Black",
            _ => "Empty",
        };
        write!(f, "{}", name)
    }
}

impl Piece {
    pub fn get_color(&self) -> Color {
        use Piece::*;

        match self {
            PawnWhite | KnightWhite | BishopWhite | RookWhite | QueenWhite | KingWhite => {
                Color::White
            }
            PawnBlack | KnightBlack | BishopBlack | RookBlack | QueenBlack | KingBlack => {
                Color::Black
            }
            _ => Color::None,
        }
    }

    /// Returns algebraic notational of piece
    pub fn algebraic(&self) -> &str {
        use PieceType::*;
        match self.get_type() {
            Pawn => "",
            King => "K",
            Queen => "Q",
            Knight => "N",
            Rook => "R",
            Bishop => "B",
            Empty => panic!("Empty Piece has no algebraic depiction"),
        }
    }

    pub const fn as_index(&self) -> usize {
        (*self) as usize
    }

    /// Returns true if black pawn is in row 1, white pawn in row 8
    pub fn is_promotion_row_pawn(&self, pos: Position) -> bool {
        let p = pos.get();
        self == &Piece::PawnWhite && p >= 0 && p <= 7
            || self == &Piece::PawnBlack && p >= 56 && p <= 63
    }

    pub fn get_type(&self) -> PieceType {
        use Piece::*;

        match self {
            PawnWhite | PawnBlack => PieceType::Pawn,
            KnightWhite | KnightBlack => PieceType::Knight,
            BishopWhite | BishopBlack => PieceType::Bishop,
            RookWhite | RookBlack => PieceType::Rook,
            QueenWhite | QueenBlack => PieceType::Queen,
            KingWhite | KingBlack => PieceType::King,
            _ => PieceType::Empty,
        }
    }
}
