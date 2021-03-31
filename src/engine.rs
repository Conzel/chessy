use crate::boards::*;
use crate::game::*;
use crate::magic_numbers::MAGIC_NUMBERS;
use crate::make_usize_wrapper;
use crate::pieces::*;
use array_const_fn_init::array_const_fn_init;
use array_init::array_init;
use lazy_static::lazy_static;
use rand::rngs::SmallRng;
use rand::{Rng, SeedableRng};

// GOALS:
// * Create Chess Engine:
//   * Move generator & validator
//   * Move heuristic
//   * Search strategy
// * Include tests
//

// For most of this, a board with positions will be helpful for visualizations.
// Here is an example:
//     a   b   c   d   e   f   g   h
//    --------------------------------
// 8 | 0   1   2   3   4   5   6   7  | 8
// 7 | 8   9   10  11  12  13  14  15 | 7
// 6 | 16  17  18  19  20  21  22  23 | 6
// 5 | 24  25  26  27  28  29  30  31 | 5
// 4 | 32  33  34  35  36  37  38  39 | 4
// 3 | 40  41  42  43  44  45  46  47 | 3
// 2 | 48  49  50  51  52  53  54  55 | 2
// 1 | 56  57  58  59  60  61  62  63 | 1
//    --------------------------------
//    a   b   c   d   e   f   g   h

const MAGIC_NUMBER_SEED: u64 = 42;
const MAX_NUM_BISHOP_OCCS: usize = 1 << MAX_BISHOP_ATTACKED_RELEVANT;

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
        if in_board(new_row, new_col) {
            // Check guarantees that new_row, new_col are u8
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

// File Attacks
// ------------
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

// ---------------------------------------------------------------------
// Magic Bitboards
// ---------------------------------------------------------------------
// Bishop staying on the middle of the board can attack 9 squares, excluding
// the border (which doesn't matter, as it is always attacked)
const MAX_BISHOP_ATTACKED_RELEVANT: u8 = 9;

//
// Guidance can be found f.e. here:
// https://stackoverflow.com/questions/30680559/how-to-find-magic-bitboards
//
// For a given positions, returns a mask that contains 1s where relevant blockers are located
// TODO: Precompute in a table
const fn blocker_mask_bishop(pos: Position) -> BitBoard {
    // Very stupid and slow solution, but it works
    #[rustfmt::skip]
    let offsets = [ (-6, -6), (-5, -5), (-4, -4), (-3, -3), (-2, -2), (-1, -1),
        (6, 6), (5, 5), (4, 4), (3, 3), (2, 2), (1, 1), 
        (6, -6), (5, -5), (4, -4), (3, -3), (2, -2), (1, -1), 
        (-6, 6), (-5, 5), (-4, 4), (-3, 3), (-2, 2), (-1, 1),
    ];
    // Board that masks the edges since occupancy is
    // irrelevant there                                           v Edges mask
    offset_attack_board(pos, offsets).const_and(BitBoard::new(!0xFF818181818181FF))
}

make_usize_wrapper!(blocker_mask_bishop_wrapper, blocker_mask_bishop);

const BLOCKER_MASKS_BISHOP: [BitBoard; 64] = array_const_fn_init![blocker_mask_bishop_wrapper; 64];

/// For a given position, returns an occupancy as bitboards.
/// Const fn makes filling vectors impossible, so we loop through with an index.
/// Every blocker mask for a bishop allows for a max. of 9 possible positions
/// (max diagonal - edges), so idx is max. 2^9 == 512
/// Example:
///      Assume that the Bishop is on position 54,
///      then the positions that are relevant for blocking (not erased by the
///      blocker mask) are 9, 17, 18, 27, 36, 45.
///      We can now describe all the relevant occupancies by setting these
///      squares to blocked / not blocked. This is exactly done via
///      our index. Assume idx = 000101, then squares 27 and 45 are blocked,
///      while the rest are unblocked.
///
const fn all_occupancies_bishop(idx: u16, pos: Position) -> Option<BitBoard> {
    let blocker_mask = BLOCKER_MASKS_BISHOP[pos as usize];

    let mut num_bits_set_board = 0;
    let mut board = BitBoard::empty();

    let mut board_pos = 63;
    while board_pos > 0 {
        if blocker_mask.bit_set_at(board_pos) {
            if (idx >> num_bits_set_board) % 2 == 1 {
                board = board.const_xor(BitBoard::singular(board_pos))
            }

            num_bits_set_board += 1;
        }

        board_pos -= 1;
    }

    // I don't know if this is really more efficient or elegant than just
    // returning any weird bitboard if the index is not in range (since this
    // will be irrelevant for the hashing function in the magic bitboards).
    // This will return a blocker
    if ((1 << num_bits_set_board) - 1) < idx {
        None
    } else {
        Some(board)
    }
}

const fn calculate_attack_board_bishop(pos: Position, occ: BitBoard) -> BitBoard {
    let directions: [(i16, i16); 4] = [(1, 1), (-1, 1), (1, -1), (-1, -1)];
    let (row, col) = pos_to_row_col(pos);
    let mut board = BitBoard::empty();

    let mut i = 0;
    // Diagonal scan in every direction
    while i < 4 {
        let (delta_row, delta_col) = directions[i];
        let mut new_row = row as i16 + delta_row;
        let mut new_col = col as i16 + delta_col;

        while in_board(new_row, new_col) {
            // in_board being true guarantees that new_row and new_col are u8
            let new_pos = row_col_to_pos(new_row as u8, new_col as u8);
            board = board.const_xor(BitBoard::singular(new_pos));
            if occ.bit_set_at(new_pos) {
                // we encountered the first piece in the occupancy and stop scanning in the
                // direction
                break;
            }

            new_row = new_row + delta_row;
            new_col = new_col + delta_col;
        }
        i += 1;
    }
    board
}

// More can be read here
// https://www.chessprogramming.org/Magic_Bitboards
/// Expects MASKED Occupancy and returns hash value for magic number
const fn hash_board_to_index_bishop(pos: Position, masked_occ: BitBoard, magic: u64) -> usize {
    // Truncation & Overflow is on purpose
    (masked_occ.get().wrapping_mul(magic) >> (64 - MAX_BISHOP_ATTACKED_RELEVANT)) as usize
}

pub fn find_magic_number_bishops(pos: Position, r: &mut impl Rng) -> u64 {
    loop {
        let current_magic = crate::utils::random_u64_few_bits(r);
        if !magic_number_collision(current_magic, pos) {
            return current_magic;
        }
    }
}

/// Checks whether the the current magic number for pos has collisions
/// on relevant occupancy boards
fn magic_number_collision(magic: u64, pos: Position) -> bool {
    let mut i = 0;
    let mut temp_occ_map: [Option<BitBoard>; MAX_NUM_BISHOP_OCCS] = [None; MAX_NUM_BISHOP_OCCS];
    let mut collision_detected = false;

    while let Some(occ) = all_occupancies_bishop(i, pos) {
        i += 1;
        let current_entry = &mut temp_occ_map[hash_board_to_index_bishop(pos, occ, magic)];
        let current_attacking_board = calculate_attack_board_bishop(pos, occ);

        // Horrible code repetition, but neither unwrap nor mutable references are stable in
        // const fns :(

        if current_entry.is_none() || current_entry.unwrap().get() == current_attacking_board.get()
        {
            *current_entry = Some(current_attacking_board);
        } else {
            return true;
        };
    }
    false
}

// TODO: Can be made const fn. Would be cool to do this as const on release and lazy static on
// debug.
fn bishop_attacking_table() -> [[BitBoard; MAX_NUM_BISHOP_OCCS]; 64] {
    let mut attacking_array: [[BitBoard; MAX_NUM_BISHOP_OCCS]; 64] =
        [[BitBoard::empty(); MAX_NUM_BISHOP_OCCS]; 64];

    let mut pos = 0u8;
    while pos < 64 {
        let magic_number = MAGIC_NUMBERS[pos as usize];
        let mut occ_map: [BitBoard; MAX_NUM_BISHOP_OCCS] = [BitBoard::empty(); MAX_NUM_BISHOP_OCCS];

        let mut i = 0;
        while let Some(occ) = all_occupancies_bishop(i, pos) {
            i += 1;
            occ_map[hash_board_to_index_bishop(pos, occ, magic_number)] =
                calculate_attack_board_bishop(pos, occ);
        }
        attacking_array[pos as usize] = occ_map;
        pos += 1;
    }
    attacking_array
}

lazy_static! {
    static ref BISHOP_ATTACKING_TABLE: Box<[[BitBoard; MAX_NUM_BISHOP_OCCS]; 64]> =
        Box::new(bishop_attacking_table());
}

fn get_bishop_attack_board(pos: Position, occ: BitBoard) -> BitBoard {
    let masked_occ = BLOCKER_MASKS_BISHOP[pos as usize].const_and(occ);
    BISHOP_ATTACKING_TABLE[pos as usize]
        [hash_board_to_index_bishop(pos, masked_occ, MAGIC_NUMBERS[pos as usize])]
}

// ---------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitboard;

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

    #[test]
    fn test_blocker_mask_bishop() {
        assert_eq!(
            blocker_mask_bishop(0),
            BitBoard::from(0b0000000001000000001000000001000000001000000001000000001000000000)
        );
        assert_eq!(
            blocker_mask_bishop(9),
            BitBoard::from(0b0000000000000000001000000001000000001000000001000000001000000000)
        );
        assert_eq!(
            blocker_mask_bishop(1),
            BitBoard::from(0b0000000000100000000100000000100000000100000000100000000000000000)
        );
        assert_eq!(blocker_mask_bishop(17), bitboard!(10, 26, 35, 44, 53));
    }

    #[test]
    fn test_all_occupanices_bishop() {
        println!("{}", blocker_mask_bishop(54));
        assert_eq!(
            all_occupancies_bishop(0b10101, 54).unwrap(),
            bitboard!(9, 27, 45)
        );
        assert_eq!(
            all_occupancies_bishop(0b00000, 54).unwrap(),
            BitBoard::empty()
        );
        assert_eq!(
            all_occupancies_bishop(0b101010, 63).unwrap(),
            bitboard!(9, 27, 45)
        );
        assert_eq!(all_occupancies_bishop(0b010101010, 63), None);
    }

    #[test]
    fn test_attack_board_bishop() {
        assert_eq!(
            calculate_attack_board_bishop(0, bitboard!(18, 27)),
            bitboard!(9, 18)
        );
        assert_eq!(
            calculate_attack_board_bishop(0, bitboard!(27, 55)),
            bitboard!(9, 18, 27)
        );
        assert_eq!(
            calculate_attack_board_bishop(0, bitboard!(9, 18, 27, 22, 26, 28)),
            bitboard!(9)
        );
        assert_eq!(
            calculate_attack_board_bishop(0, bitboard!()),
            bitboard!(9, 18, 27, 36, 45, 54, 63)
        );
        assert_eq!(
            calculate_attack_board_bishop(49, bitboard!(35)),
            bitboard!(35, 42, 40, 56, 58)
        );
    }

    #[test]
    fn test_bishop_attack() {
        assert_eq!(
            get_bishop_attack_board(0, bitboard!(18, 27)),
            bitboard!(9, 18)
        );
        assert_eq!(
            get_bishop_attack_board(0, bitboard!(27, 55)),
            bitboard!(9, 18, 27)
        );
        assert_eq!(
            get_bishop_attack_board(0, bitboard!(9, 18, 27, 22, 26, 28)),
            bitboard!(9)
        );
        assert_eq!(
            get_bishop_attack_board(0, bitboard!()),
            bitboard!(9, 18, 27, 36, 45, 54, 63)
        );
        assert_eq!(
            get_bishop_attack_board(49, bitboard!(35)),
            bitboard!(35, 42, 40, 56, 58)
        );
    }
}
