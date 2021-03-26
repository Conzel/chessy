use crate::boards::*;
use crate::game::*;
use crate::pieces::*;

// GOALS:
// * Create Chess Engine:
//   * Move generator & validator
//   * Move heuristic
//   * Search strategy
// * Include tests
//

// Move to game
struct Move {
    start: Position,
    end: Position,
    piece: Piece,
}

impl Move {
    fn new(start: Position, end: Position, piece: Piece) -> Self {
        Move {
            start: start,
            end: end,
            piece: piece,
        }
    }
}

impl BitBoardGame {
    fn gen_moves() -> Vec<Move> {
        todo!()
    }
}

/// Calculates an attacking board, a bitboard where 1 is set true if the piece
/// at pos can attack the space. The attacking board is calculated using offsets
/// (spaces the piece can attack relative to pos)
/// so this is viable for Knight, Pawn and King (jumping, not gliding pieces).
/// As this will be precomputed and stored in a table, it does not matter too much
/// how efficient it is.
/// Offsets are saved as (row,col).
fn offset_attack_board<'a>(
    pos: Position,
    offsets: impl Iterator<Item = &'a (i16, i16)>,
) -> BitBoard {
    // A board that has a 1 at position pos
    let mut res = BitBoard::empty();
    let (row_u, col_u) = pos_to_row_col(pos);
    let (row, col) = (row_u as i16, col_u as i16);
    for (off_row, off_col) in offsets {
        let new_row = row + off_row;
        let new_col = col + off_col;
        if new_row < 8 && new_row >= 0 && new_col < 8 && new_col >= 0 {
            res = res ^ BitBoard::singular(row_col_to_pos(new_row as u8, new_col as u8));
        }
    }
    res
}

/// TODO: precompute this in a table
fn knight_attack_board(pos: Position) -> BitBoard {
    static KNIGHT_OFFSETS: [(i16, i16); 8] = [
        (1, 2),
        (2, 1),
        (-1, 2),
        (2, -1),
        (1, -2),
        (-2, 1),
        (-1, -2),
        (-2, -1),
    ];
    offset_attack_board(pos, KNIGHT_OFFSETS.iter())
}

/// TODO: precompute this in a table
fn king_attack_board(pos: Position) -> BitBoard {
    static KING_OFFSETS: [(i16, i16); 8] = [
        (0, 1),
        (1, 0),
        (0, -1),
        (-1, 0),
        (1, 1),
        (-1, -1),
        (-1, 1),
        (1, -1),
    ];
    offset_attack_board(pos, KING_OFFSETS.iter())
}

/// TODO: precompute this in a table
fn white_pawn_attack_board(pos: Position) -> BitBoard {
    static PAWN_OFFSETS: [(i16, i16); 2] = [(-1, -1), (-1, 1)];
    offset_attack_board(pos, PAWN_OFFSETS.iter())
}

/// TODO: precompute this in a table
fn black_pawn_attack_board(pos: Position) -> BitBoard {
    static PAWN_OFFSETS: [(i16, i16); 2] = [(1, -1), (1, 1)];
    offset_attack_board(pos, PAWN_OFFSETS.iter())
}

// TODO: remove
pub fn temp_examine() {
    println!("{}", BitBoard::singular(1));
    println!("{}", knight_attack_board(1));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pos_from_string() {
        assert_eq!(pos_from_string("h1").unwrap(), 63);
        assert_eq!(pos_from_string("a8").unwrap(), 0);
        assert_eq!(pos_from_string("b6").unwrap(), 17);
        assert!(pos_from_string("sdlfj").is_err());
        assert!(pos_from_string("a9").is_err());
    }

    #[test]
    fn test_pos_row_col_conversion() {
        assert_eq!(pos_to_row_col(0), (0, 0));
        assert_eq!(pos_to_row_col(1), (0, 1));
        assert_eq!(pos_to_row_col(8), (1, 0));
        assert_eq!(pos_to_row_col(9), (1, 1));
        let (r1, c1) = pos_to_row_col(9);
        assert_eq!(row_col_to_pos(r1, c1), 9);
        let (r2, c2) = pos_to_row_col(23);
        assert_eq!(row_col_to_pos(r2, c2), 23);
    }

    #[test]
    fn test_knight_attack_board() {
        assert_eq!(
            knight_attack_board(0),
            BitBoard::from(0b0000000000100000010000000000000000000000000000000000000000000000)
        );
        assert_eq!(
            knight_attack_board(27),
            BitBoard::from(0b0000000000101000010001000000000001000100001010000000000000000000)
        );
    }

    #[test]
    fn test_king_attack_board() {
        assert_eq!(
            king_attack_board(0),
            BitBoard::from(0b0100000011000000000000000000000000000000000000000000000000000000)
        );
        assert_eq!(
            king_attack_board(27),
            BitBoard::from(0b0000000000000000001110000010100000111000000000000000000000000000)
        );
    }

    #[test]
    fn test_pawn_attack_board() {
        assert_eq!(white_pawn_attack_board(62), BitBoard::from(0b10100000000));
        assert_eq!(
            black_pawn_attack_board(1),
            BitBoard::from(0b0000000010100000000000000000000000000000000000000000000000000000)
        );
        assert_eq!(
            black_pawn_attack_board(0),
            BitBoard::from(0b0000000001000000000000000000000000000000000000000000000000000000)
        );
    }
}
