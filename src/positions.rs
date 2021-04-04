use crate::chess_errors::*;
use std::fmt::{self, Display};
use std::ops;
use std::str::FromStr;

// Chessboard positions on a 8x8 board.
//
// Numbered as follows:
//
//     a  b  c  d  e  f  g  h
//   ---------------------------
// 8 | 0  1  2  3  4  5  6  7  | 8
// 7 | 8  9  10 11 12 13 14 15 | 7
// 6 | 16 17 18 19 20 21 22 23 | 6
// 5 | 24 25 26 27 28 29 30 31 | 5
// 4 | 32 33 34 35 36 37 38 39 | 4
// 3 | 40 41 42 43 44 45 46 47 | 3
// 2 | 48 49 50 51 52 53 54 55 | 2
// 1 | 56 57 58 59 60 61 62 63 | 1
//   ---------------------------
//    a  b  c  d  e  f  g  h
//
// ---------------------------------------------
// Positions
// ---------------------------------------------

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position(u8);

impl From<u8> for Position {
    fn from(u: u8) -> Self {
        debug_assert!(u < 64, "Invalid position: {}", u);
        Position(u)
    }
}

impl From<usize> for Position {
    fn from(u: usize) -> Self {
        (u as u8).into()
    }
}

impl From<i32> for Position {
    fn from(u: i32) -> Self {
        (u as u8).into()
    }
}

impl FromStr for Position {
    type Err = ChessError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Error is rather big, so we use a closure to avoid copies
        let err_closure = || -> ChessError { format!("Invalid Chess position {}", s).into() };
        let mut chars = s.chars();

        let col = chars.next().ok_or_else(err_closure)?;
        let row = chars
            .next()
            .map(|r| r.to_digit(10))
            .flatten()
            .ok_or_else(err_closure)?;

        // We need to catch invalid early rows, else we will have a panic on unsigned integer underflow
        //    Too many characters || row is invalid
        if chars.next().is_some() || row > 8 {
            // Too many characters
            return Err(err_closure());
        }

        // number part v               v letter part
        let pos: u8 = ((8 - row) * 8) as u8 + col as u8 - 'a' as u8;
        if pos >= 8 * 8 {
            Err(err_closure())
        } else {
            Ok(Position::from(pos))
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (row, col) = self.to_row_col();
        write!(
            f,
            "{}{}",
            ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'][col as usize],
            8 - row,
        )
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct PositionIterator(u8);

impl Iterator for PositionIterator {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 > 63 {
            None
        } else {
            self.0 = self.0 + 1u8;
            Some((self.0 - 1).into())
        }
    }
}

impl Position {
    /// Returns row and col from position. If the position is illegal, an illegal row and col
    /// will be returned.
    /// Example: Position 63 (H1 in chess board) is mapped to (7,7)
    pub const fn to_row_col(self) -> (u8, u8) {
        (self.0 / 8, self.0 % 8)
    }

    /// Need const fn for some tables. Unchecked position, as
    /// this is not allowed in const fns.
    pub const fn const_new(u: u8) -> Position {
        Position(u)
    }

    /// Transforms a row and a col to Position on the board.
    /// Row and col must correspond to a legal board position,
    /// else the returned value also doesn't correspond to a legal board position.
    pub fn from_row_col(row: u8, col: u8) -> Position {
        debug_assert!(Position::in_board(row as i16, col as i16));
        (row * 8 + col).into()
    }

    /// Transforms a row and a col to Position on the board.
    /// Row and col must correspond to a legal board position,
    /// else the returned value also doesn't correspond to a legal board position.
    pub const fn const_from_row_col(row: u8, col: u8) -> Position {
        Self::const_new(row * 8 + col)
    }

    /// Checks if row and col belong to a legal board position.
    pub const fn in_board(row: i16, col: i16) -> bool {
        row >= 0 && col >= 0 && row < 8 && col < 8
    }

    /// Allows access to underlying u8. Should only be used when necessary.
    pub const fn get(self) -> u8 {
        self.0
    }

    /// Allows to iterate over all positions on the board
    pub fn all_positions() -> PositionIterator {
        PositionIterator(0)
    }

    pub const fn const_index(self) -> usize {
        self.0 as usize
    }
}

impl<T> ops::Index<Position> for [T; 64] {
    type Output = T;

    fn index(&self, index: Position) -> &T {
        &self[index.0 as usize]
    }
}

impl<T> ops::IndexMut<Position> for [T; 64] {
    fn index_mut(&mut self, index: Position) -> &mut Self::Output {
        &mut self[index.0 as usize]
    }
}

impl_op_ex_commutative!(+ |a: &Position, b: &u8| -> Position { Position::from(a.0 + b) });
