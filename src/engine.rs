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

// ---------------------------------------------------------------------
// Jumping Attack Boards
// ---------------------------------------------------------------------

/// Calculates an attacking board, a bitboard where 1 is set true if the piece
/// at pos can attack the space. The attacking board is calculated using offsets
/// (spaces the piece can attack relative to pos)
/// so this is viable for Knight, Pawn and King (jumping, not gliding pieces).
/// As this will be precomputed and stored in a table, it does not matter too much
/// how efficient it is.
/// Offsets are saved as (row,col).
const fn offset_attack_board<'a, const N: usize>(
    pos: Position,
    offsets: [(i16, i16); N],
) -> BitBoard {
    // A board that has a 1 at position pos
    let mut res = BitBoard::empty();
    let (row_u, col_u) = pos_to_row_col(pos);
    let (row, col) = (row_u as i16, col_u as i16);

    let mut i = 0;
    // const fns need while loop
    while i < N {
        let (off_row, off_col) = offsets[i];
        let new_row = row + off_row;
        let new_col = col + off_col;
        if new_row < 8 && new_row >= 0 && new_col < 8 && new_col >= 0 {
            // res = res ^ BitBoard::singular(row_col_to_pos(new_row as u8, new_col as u8));
            res = BitBoard::singular(row_col_to_pos(new_row as u8, new_col as u8)).const_xor(res);
        }
        i += 1;
    }
    res
}

/// TODO: precompute this in a table
const fn knight_attack_board(pos: Position) -> BitBoard {
    const KNIGHT_OFFSETS: [(i16, i16); 8] = [
        (1, 2),
        (2, 1),
        (-1, 2),
        (2, -1),
        (1, -2),
        (-2, 1),
        (-1, -2),
        (-2, -1),
    ];
    offset_attack_board(pos, KNIGHT_OFFSETS)
}

/// TODO: precompute this in a table
const fn king_attack_board(pos: Position) -> BitBoard {
    const KING_OFFSETS: [(i16, i16); 8] = [
        (0, 1),
        (1, 0),
        (0, -1),
        (-1, 0),
        (1, 1),
        (-1, -1),
        (-1, 1),
        (1, -1),
    ];
    offset_attack_board(pos, KING_OFFSETS)
}

/// TODO: precompute this in a table
const fn white_pawn_attack_board(pos: Position) -> BitBoard {
    const PAWN_OFFSETS: [(i16, i16); 2] = [(-1, -1), (-1, 1)];
    offset_attack_board(pos, PAWN_OFFSETS)
}

/// TODO: precompute this in a table
const fn black_pawn_attack_board(pos: Position) -> BitBoard {
    const PAWN_OFFSETS: [(i16, i16); 2] = [(1, -1), (1, 1)];
    offset_attack_board(pos, PAWN_OFFSETS)
}

// ---------------------------------------------------------------------
// Sliding Attack Boards
// ---------------------------------------------------------------------
//
// Rank Attacks
// ------------

/// Returns the attacked bits when a rank-attacking piece is on rank_pos
/// and the other pieces in the rank are occupied
/// according to rank_state_occ
///
/// Very conclusive description is here:
/// https://web.archive.org/web/20050428023318/http://www.cis.uab.edu/info/faculty/hyatt/bitmaps.html
/// under 3. Rotated Bitmaps
///
/// Example:
/// Rook on H1, pieces on B1, E1
/// => rank_pos = 0 (rank starts from right and counts up to the left)
/// => rank_state_occ = 0b0100100x
/// The x doesn't matter, as the rook itself is on it anyways
/// => return:
/// 0b0000111x
///
// TODO: Precompute in a 8x64 table (8: rook positions, 64: possible states, not counting MSB and
// LSB, as they can always be seen as occupied)
fn rank_attack_bit(rank_pos: u8, rank_state_occ: u8) -> u8 {
    // The Bitscan is a bit naive, but we cannot use inline assembly (BSF/BSR) in a const function, and this
    // will be precomputed anyways.
    let left_mask: u8 = if rank_pos == 7 {
        0
    } else {
        0xFF << (rank_pos + 1)
    };
    let right_mask: u8 = if rank_pos == 0 {
        0
    } else {
        0xFF >> (8 - rank_pos)
    };
    let left_occ = rank_state_occ & left_mask;
    let right_occ = rank_state_occ & right_mask;

    // Example:
    // Attack:     <-----------   -------------->
    // Rook:                    R
    // Occ:               x o x - o o x o
    // left_mask:         x x x o o o o o
    // left_occ:          x o x o o o o o
    // right_mask:        o o o o x x x x
    // right_occ:         o o o o o o x o
    // Accordingly, we just need a BSR for left_occ and a BSF for right_occ
    let first_occ_left = if left_occ == 0 { 7 } else { bsr(left_occ) };
    let first_occ_right = if right_occ == 0 { 0 } else { bsf(right_occ) };
    // Going with the example again:
    // attack_until_mask_left:
    //                    o o x x x x x x
    // attack_until_mask_right:
    //                    x x x x x x x o
    let attack_until_mask_left = 0xFF >> (8 - first_occ_left - 1);
    let attack_until_mask_right = 0xFF << first_occ_right;

    //          o o o o x x x o           XOR           o o x o o o o o
    (attack_until_mask_right & right_mask) ^ (attack_until_mask_left & left_mask)
}

/// Returns Bitboard from rank attacking information and rank.
/// Rank 0 here means the 0th row, so A8 to H8.
/// Example: 00000001 at Rank 0 returns
/// 00000001
/// 0000000
/// ...
/// 0000000
const fn rank_attack_to_bitboard(rank_attack: u8, rank: u8) -> BitBoard {
    BitBoard::new((rank_attack as u64) << ((8 - rank - 1) * 8))
}

/// Returns Bitboard from file attacking information and file.
/// File 0 here means the 0th col, so A8 to A1
/// Example: 00000001 at Rank 0 returns
/// 00..0
/// 00..0
/// ...
/// 10..0
fn file_attack_to_bitboard(file_attack: u8, file: u8) -> BitBoard {
    rank_attack_to_bitboard(file_attack, 7 - file).rotate90()
}

/// Returns index of most significant bit set.
/// Panics when a 0 is passed in.
const fn bsf(a: u8) -> u8 {
    let mut i = 0;
    let mut curr: u8;
    let mut a_ = a;
    loop {
        a_ = a_.rotate_left(1);
        curr = a_ % 2;
        if curr == 1 {
            return 8 - i - 1;
        }
        if i >= 8 {
            return 7;
        }
        i += 1;
    }
}

/// Returns index of least significant bit set.
/// Panics when a 0 is passed in.
const fn bsr(a: u8) -> u8 {
    let mut i = 0;
    let mut curr: u8;
    let mut a_ = a;
    loop {
        curr = a_ % 2;
        if curr == 1 {
            return i;
        }
        if i >= 8 {
            return 0;
        }
        a_ = a_ >> 1;
        i += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn test_bitscans() {
        assert_eq!(bsf(0b01000000), 6);
        assert_eq!(bsf(0b10000000), 7);
        assert_eq!(bsf(0b00000001), 0);
        assert_eq!(bsr(0b01000000), 6);
        assert_eq!(bsr(0b10000000), 7);
        assert_eq!(bsr(0b00000001), 0);

        assert_eq!(bsf(0b01000101), 6);
        assert_eq!(bsf(0b10100000), 7);
        assert_eq!(bsr(0b11000000), 6);
        assert_eq!(bsr(0b11111101), 0);
    }

    #[test]
    fn test_rank_attack_bits() {
        assert_eq!(
            rank_attack_bit(4, 0b10100010),
            0b00101110,
            "{:08b} != {:08b}",
            rank_attack_bit(4, 0b10100010),
            0b00101110
        );
        assert_eq!(
            rank_attack_bit(0, 0b10100010),
            0b00000010,
            "{:08b} != {:08b}",
            rank_attack_bit(0, 0b10100010),
            0b00000010
        );
        assert_eq!(
            rank_attack_bit(7, 0b10100010),
            0b01100000,
            "{:08b} != {:08b}",
            rank_attack_bit(0, 0b10100010),
            0b01100000
        );
        assert_eq!(
            rank_attack_bit(0, 0b00000000),
            0b11111110,
            "{:08b} != {:08b}",
            rank_attack_bit(0, 0b00000000),
            0b11111110
        );
        assert_eq!(
            rank_attack_bit(7, 0b00000000),
            0b01111111,
            "{:08b} != {:08b}",
            rank_attack_bit(0, 0b00000000),
            0b01111111
        );
        assert_eq!(
            rank_attack_bit(4, 0b00000000),
            0b11101111,
            "{:08b} != {:08b}",
            rank_attack_bit(0, 0b00000000),
            0b11101111
        );
    }

    #[test]
    fn test_rank_attack_bitboard() {
        assert_eq!(rank_attack_to_bitboard(0b1, 0), BitBoard::singular(7));
        assert_eq!(rank_attack_to_bitboard(0b1, 1), BitBoard::singular(15));
        assert_eq!(rank_attack_to_bitboard(0b1, 7), BitBoard::singular(63));

        assert_eq!(
            rank_attack_to_bitboard(0b10000001, 3),
            BitBoard::singular(24) ^ BitBoard::singular(31)
        );
    }

    #[test]
    fn test_file_attack_bitboard() {
        assert_eq!(file_attack_to_bitboard(0b1, 0), BitBoard::singular(56));
        assert_eq!(file_attack_to_bitboard(0b1, 1), BitBoard::singular(57));
        assert_eq!(file_attack_to_bitboard(0b1, 7), BitBoard::singular(63));

        assert_eq!(
            file_attack_to_bitboard(0b10000001, 3),
            BitBoard::singular(3) ^ BitBoard::singular(59)
        );
    }
}
