use crate::chess_errors::*;

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

pub type Position = u8;

pub fn pos_from_string(s: &str) -> ChessResult<Position> {
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
    let pos = (8 - row) * 8 + col as u32 - 'a' as u32;
    if pos >= 8 * 8 {
        Err(err_closure())
    } else {
        Ok(pos as u8)
    }
}

/// Returns row and col from position. If the position is illegal, an illegal row and col
/// will be returned.
/// Example: Position 63 (H1 in chess board) is mapped to (7,7)
pub const fn pos_to_row_col(p: Position) -> (u8, u8) {
    (p / 8, p % 8)
}

/// Transforms a row and a col to Position on the board.
/// Row and col must correspond to a legal board position,
/// else the returned value also doesn't correspond to a legal board position.
pub const fn row_col_to_pos(row: u8, col: u8) -> Position {
    row * 8 + col
}

/// Checks if row and col belong to a legal board position.
pub const fn in_board(row: i16, col: i16) -> bool {
    row >= 0 && col >= 0 && row < 8 && col < 8
}

/// Returns algebraic notation equivalent of chess position. Error if the position is not valid.
pub fn pos_to_algebraic(pos: Position) -> ChessResult<String> {
    if pos > 63 {
        Err("Invalid Position".to_string().into())
    } else {
        let (row, col) = pos_to_row_col(pos);
        Ok(format!(
            "{}{}",
            ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'][col as usize],
            8 - row,
        ))
    }
}
